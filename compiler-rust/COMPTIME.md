# ReScript Comptime: Compile-Time Metaprogramming

This document describes a proposed compile-time execution system for ReScript, inspired by Zig's comptime feature. The goal is to enable powerful metaprogramming capabilities that can replace all current PPX use cases while being simpler to use and maintain.

## Table of Contents

1. [Overview](#overview)
2. [Design Goals](#design-goals)
3. [Core Concepts](#core-concepts)
4. [Type Introspection](#type-introspection)
5. [Comptime Interpreter](#comptime-interpreter)
6. [Replacing PPXs](#replacing-ppxs)
7. [Implementation Plan](#implementation-plan)
8. [Examples](#examples)
9. [Appendix: Zig Comptime Reference](#appendix-zig-comptime-reference)

---

## Overview

### What is Comptime?

Comptime (compile-time execution) allows regular ReScript code to execute during compilation. Unlike PPXs, which require writing OCaml code that manipulates the AST, comptime lets you write ReScript code that:

1. **Runs at compile time** when all inputs are known
2. **Introspects types** via `@typeInfo`
3. **Generates specialized code** based on type structure
4. **Parses strings** (for DSLs like GraphQL, SQL, CSS)

### Why Comptime Over PPXs?

| Aspect | PPXs | Comptime |
|--------|------|----------|
| **Language** | OCaml | ReScript |
| **Toolchain** | Separate OCaml build | Part of compiler |
| **Debugging** | Complex AST manipulation | Regular ReScript code |
| **Testing** | Requires special setup | Standard unit tests |
| **Community** | OCaml expertise needed | Any ReScript developer |
| **Maintenance** | AST changes break PPXs | Stable interface |

### Key Insight

PPXs receive **strings** via extension points (`%sql(...)`, `%graphql(...)`), then parse those strings and generate code. Comptime can do the same thing—the DSL syntax is just string content, not ReScript syntax.

```rescript
// PPX approach:
let query = %sql(`SELECT * FROM users WHERE id = $id`)

// Comptime approach:
let query = sql(`SELECT * FROM users WHERE id = $id`)
//              ↑ comptime function  ↑ comptime string argument
```

---

## Design Goals

### 1. No Global State

The Rust compiler is designed for concurrent compilation. Comptime must not introduce global state:

- Each compilation unit gets its own interpreter instance
- Type contexts are passed explicitly
- No `static mut` or `thread_local!`

### 2. Same Language

Unlike macro systems with separate DSLs, comptime is just ReScript:

```rescript
// This is regular ReScript that happens to run at compile time
@comptime
let factorial = (n: int): int => {
  if n <= 1 { 1 } else { n * factorial(n - 1) }
}

let x = factorial(10) // Evaluated at compile time → 3628800
```

### 3. Hermetic Evaluation

Following Zig's design, comptime has no I/O:

- No file system access (except `@embedFile`)
- No network access
- No randomness
- Results are deterministic and cacheable

### 4. Explicit Opt-In

Code must be explicitly marked for compile-time evaluation:

```rescript
@comptime           // Function can run at comptime
let helper = ...

comptime T: type    // Parameter must be comptime-known
```

### 5. Fuel Limits

To prevent infinite loops from hanging the compiler:

```rust
const DEFAULT_FUEL: usize = 100_000;
```

Each interpreter step decrements fuel. Exhaustion produces a compile error.

---

## Core Concepts

### Comptime Functions

Functions marked `@comptime` can execute at compile time when all arguments are comptime-known:

```rescript
@comptime
let double = (x: int): int => x * 2

let a = double(5)        // Comptime: becomes `let a = 10`
let b = double(runtime)  // Runtime: becomes `let b = runtime * 2`
```

### Comptime Parameters

Parameters marked `comptime` must be known at compile time:

```rescript
@comptime
let repeat = (comptime n: int, s: string): string => {
  let result = ""
  for _ in 0 to n - 1 {
    result = result ++ s
  }
  result
}

let x = repeat(3, "hi")     // Comptime n, generates unrolled code
let y = repeat(count, "hi") // Error: count is not comptime-known
```

### Type Parameters

Types can be passed as comptime values:

```rescript
@comptime
let sizeOf = (comptime T: type): int => {
  switch @typeInfo(T) {
  | Int => 4
  | Float => 8
  | Record({fields}) => fields->Array.reduce(0, (acc, f) => acc + sizeOf(f.typ))
  | _ => 0
  }
}

let intSize = sizeOf(int)   // Comptime: becomes `let intSize = 4`
```

### Inline For

Loops over comptime arrays are unrolled:

```rescript
@comptime
let fields = @typeInfo(User)->getFields

// This loop is unrolled at compile time
@inline for field in fields {
  Console.log(field.name)
}

// Becomes:
Console.log("id")
Console.log("name")
Console.log("email")
```

---

## Type Introspection

### The Type.t Representation

Types at compile time are represented as a variant:

```rescript
// Provided by the compiler in a Type module
type rec t =
  | Var({name: option<string>})
  | Arrow({arg: labeledArg, ret: t})
  | Tuple({elements: array<t>})
  | Constr({path: string, args: array<t>})
  | Record({path: string, fields: array<field>})
  | Variant({path: string, constructors: array<constructor>})
  | PolyVariant({cases: array<variantCase>, closed: bool})
  | Object({methods: array<method>})

and field = {
  name: string,
  typ: t,
  mutable_: bool,
  optional: bool,
  attributes: array<attribute>,
}

and constructor = {
  name: string,
  args: constructorArgs,
  attributes: array<attribute>,
}

and constructorArgs =
  | Tuple(array<t>)
  | InlineRecord(array<field>)

and labeledArg = {
  label: option<string>,
  optional: bool,
  typ: t,
}

and variantCase = {
  name: string,
  payload: option<t>,
}

and method = {
  name: string,
  typ: t,
}

and attribute = {
  name: string,
  payload: option<string>,
}
```

### Introspection Builtins

```rescript
// Get type information
@typeInfo(User)           // → Type.Record({fields: [...], ...})

// Get type name as string
@typeName(User)           // → "User"

// Check if type has a field
@hasField(User, "name")   // → true

// Get field type
@fieldType(User, "name")  // → Type.Constr({path: "string", args: []})

// Get attribute value
@getAttr(field, "json.key") // → option<string>

// Access field value at comptime
@field(value, "name")     // → value.name (field name is comptime string)
```

### Example: Record Field Iteration

```rescript
type user = {
  id: int,
  name: string,
  email: option<string>,
}

@comptime
let printFields = (comptime T: type): unit => {
  switch @typeInfo(T) {
  | Record({fields}) =>
    fields->Array.forEach(field => {
      Console.log(`Field: ${field.name}, Type: ${@typeName(field.typ)}`)
    })
  | _ => @compileError("Expected a record type")
  }
}

printFields(user)
// Compile-time output:
// Field: id, Type: int
// Field: name, Type: string
// Field: email, Type: option<string>
```

---

## Comptime Interpreter

### Architecture

The comptime interpreter is a tree-walking interpreter written in Rust that evaluates Lambda IR:

```
ReScript Source
    ↓ Parser
AST (Surface Syntax)
    ↓ Type Checker
Typed AST + TypeContext
    ↓ Lambda Conversion
Lambda IR
    ↓
┌─────────────────────────────────────┐
│     Comptime Detection              │
│  (find @comptime functions/calls)   │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│     Comptime Interpreter            │
│  - Tree-walking evaluation          │
│  - Access to TypeContext            │
│  - Fuel-limited execution           │
└─────────────────────────────────────┘
    ↓ (splice results back)
Lambda IR (with comptime results inlined)
    ↓ JS Compilation
JavaScript
```

### Value Representation

Comptime values are represented in Rust:

```rust
/// Runtime values during comptime evaluation
#[derive(Debug, Clone)]
pub enum Value {
    /// A constant value (int, string, bool, etc.)
    Const(Constant),

    /// A type (result of @typeInfo)
    Type(ComptimeType),

    /// A closure (function + captured environment)
    Closure {
        params: Vec<Ident>,
        body: Box<Lambda>,
        env: Env,
    },

    /// A record/struct value
    Record(HashMap<String, Value>),

    /// An array value
    Array(Vec<Value>),

    /// A variant/constructor
    Variant {
        tag: String,
        payload: Option<Box<Value>>,
    },
}

type Env = HashMap<Ident, Value>;
```

### Interpreter Core

```rust
pub struct ComptimeInterpreter<'ctx> {
    /// Type context for @typeInfo
    type_ctx: &'ctx TypeContext<'ctx>,
    /// Variable environment
    env: Env,
    /// Remaining fuel (decremented each step)
    fuel: usize,
}

impl<'ctx> ComptimeInterpreter<'ctx> {
    pub fn eval(&mut self, lam: &Lambda) -> Result<Value, ComptimeError> {
        // Decrement fuel
        self.fuel = self.fuel.checked_sub(1)
            .ok_or(ComptimeError::OutOfFuel)?;

        match lam {
            Lambda::Lconst(c) => Ok(Value::Const(c.clone())),

            Lambda::Lvar(id) => self.env.get(id).cloned()
                .ok_or(ComptimeError::UnboundVariable(id.clone())),

            Lambda::Llet(_, id, value, body) => {
                let v = self.eval(value)?;
                self.env.insert(id.clone(), v);
                let result = self.eval(body);
                self.env.remove(id);
                result
            }

            Lambda::Lfunction(func) => Ok(Value::Closure {
                params: func.params.clone(),
                body: func.body.clone(),
                env: self.env.clone(),
            }),

            Lambda::Lapply(apply) => {
                let func = self.eval(&apply.ap_func)?;
                let args: Vec<Value> = apply.ap_args.iter()
                    .map(|a| self.eval(a))
                    .collect::<Result<_, _>>()?;
                self.apply(func, args)
            }

            Lambda::Lprim(info) => self.eval_primitive(&info.primitive, &info.args),

            Lambda::LifThenElse(cond, then_, else_) => {
                match self.eval(cond)? {
                    Value::Const(Constant::JsTrue) => self.eval(then_),
                    Value::Const(Constant::JsFalse) => self.eval(else_),
                    _ => Err(ComptimeError::TypeError("expected bool".into())),
                }
            }

            // ... other cases
        }
    }
}
```

### Supported Operations

The interpreter supports all pure Lambda operations:

| Operation | Supported | Notes |
|-----------|-----------|-------|
| Constants | ✅ | int, float, string, bool |
| Variables | ✅ | Lexically scoped |
| Let bindings | ✅ | Strict and recursive |
| Functions | ✅ | Closures with captured env |
| Application | ✅ | With arity checking |
| If/else | ✅ | Condition must be bool |
| Pattern matching | ✅ | Switch on constants/blocks |
| Arithmetic | ✅ | All int/float/bigint ops |
| String ops | ✅ | Concat, length, get, sub |
| Array ops | ✅ | Make, get, length |
| Record ops | ✅ | Field access, construction |
| Type introspection | ✅ | @typeInfo, @typeName |
| While/for loops | ❌ | Runtime only |
| Mutation | ❌ | Runtime only |
| Exceptions | ❌ | Runtime only |
| I/O | ❌ | By design |

### Primitive Evaluation

```rust
fn eval_primitive(&mut self, prim: &Primitive, args: &[Lambda]) -> Result<Value, ComptimeError> {
    let values: Vec<Value> = args.iter()
        .map(|a| self.eval(a))
        .collect::<Result<_, _>>()?;

    match prim {
        // Integer arithmetic
        Primitive::Paddint => {
            let (a, b) = self.extract_ints(&values)?;
            Ok(Value::Const(Constant::int(a.wrapping_add(b))))
        }
        Primitive::Psubint => {
            let (a, b) = self.extract_ints(&values)?;
            Ok(Value::Const(Constant::int(a.wrapping_sub(b))))
        }
        Primitive::Pmulint => {
            let (a, b) = self.extract_ints(&values)?;
            Ok(Value::Const(Constant::int(a.wrapping_mul(b))))
        }
        Primitive::Pdivint => {
            let (a, b) = self.extract_ints(&values)?;
            if b == 0 {
                return Err(ComptimeError::DivisionByZero);
            }
            Ok(Value::Const(Constant::int(a / b)))
        }

        // String operations
        Primitive::Pstringadd => {
            let (a, b) = self.extract_strings(&values)?;
            Ok(Value::Const(Constant::string(a + &b)))
        }
        Primitive::Pstringlength => {
            let s = self.extract_string(&values[0])?;
            Ok(Value::Const(Constant::int(s.len() as i32)))
        }

        // Type introspection (comptime-specific)
        Primitive::PcomptimeTypeInfo => {
            let type_ref = self.extract_type_arg(&args[0])?;
            let comptime_type = type_to_comptime(self.type_ctx, type_ref);
            Ok(Value::Type(comptime_type))
        }

        // Compile error
        Primitive::PcomptimeError => {
            let msg = self.extract_string(&values[0])?;
            Err(ComptimeError::UserError(msg))
        }

        _ => Err(ComptimeError::UnsupportedPrimitive(prim.clone())),
    }
}
```

### Type Conversion

Converting the compiler's internal type representation to comptime values:

```rust
/// Convert TypeExprRef to ComptimeType
pub fn type_to_comptime(ctx: &TypeContext, typ: TypeExprRef) -> ComptimeType {
    let ty = ctx.repr(typ);
    let desc = ctx.get(ty).desc();

    match &*desc {
        TypeDesc::Tvar(name) => ComptimeType::Var {
            name: name.clone()
        },

        TypeDesc::Tarrow { arg, ret, .. } => ComptimeType::Arrow {
            arg: Box::new(ComptimeArg {
                label: match &arg.lbl {
                    ArgLabel::Nolabel => None,
                    ArgLabel::Labelled(s) => Some(s.clone()),
                    ArgLabel::Optional(s) => Some(s.clone()),
                },
                optional: matches!(arg.lbl, ArgLabel::Optional(_)),
                typ: type_to_comptime(ctx, arg.typ),
            }),
            ret: Box::new(type_to_comptime(ctx, *ret)),
        },

        TypeDesc::Ttuple(elems) => ComptimeType::Tuple {
            elements: elems.iter()
                .map(|e| type_to_comptime(ctx, *e))
                .collect(),
        },

        TypeDesc::Tconstr { path, args, .. } => {
            // Look up type declaration for records/variants
            if let Some(decl) = ctx.find_type_declaration(path) {
                match &decl.type_kind {
                    TypeKind::TypeRecord(labels, _) => ComptimeType::Record {
                        path: path.to_string(),
                        fields: labels.iter()
                            .map(|l| ComptimeField {
                                name: l.ld_id.name().to_string(),
                                typ: type_to_comptime(ctx, l.ld_type),
                                mutable_: l.ld_mutable == MutableFlag::Mutable,
                                optional: l.ld_optional,
                                attributes: convert_attributes(&l.ld_attributes),
                            })
                            .collect(),
                    },
                    TypeKind::TypeVariant(constrs) => ComptimeType::Variant {
                        path: path.to_string(),
                        constructors: constrs.iter()
                            .map(|c| ComptimeConstructor {
                                name: c.cd_id.name().to_string(),
                                args: convert_constructor_args(&c.cd_args, ctx),
                                attributes: convert_attributes(&c.cd_attributes),
                            })
                            .collect(),
                    },
                    _ => ComptimeType::Constr {
                        path: path.to_string(),
                        args: args.iter().map(|a| type_to_comptime(ctx, *a)).collect(),
                    },
                }
            } else {
                ComptimeType::Constr {
                    path: path.to_string(),
                    args: args.iter().map(|a| type_to_comptime(ctx, *a)).collect(),
                }
            }
        },

        TypeDesc::Tvariant(row) => ComptimeType::PolyVariant {
            cases: row.row_fields.iter()
                .map(|(label, field)| ComptimeVariantCase {
                    name: label.clone(),
                    payload: match field {
                        RowField::Rpresent(Some(t)) => Some(type_to_comptime(ctx, *t)),
                        _ => None,
                    },
                })
                .collect(),
            closed: row.row_closed,
        },

        _ => ComptimeType::Unknown,
    }
}
```

### Splicing Results Back

After comptime evaluation, results are converted back to Lambda IR:

```rust
/// Convert a comptime value back to Lambda IR
pub fn splice_value(value: Value) -> Lambda {
    match value {
        Value::Const(c) => Lambda::Lconst(c),

        Value::Array(elements) => {
            let lam_elements: Vec<Lambda> = elements.into_iter()
                .map(splice_value)
                .collect();
            Lambda::prim(
                Primitive::Pmakearray,
                lam_elements,
                Location::none(),
            )
        }

        Value::Record(fields) => {
            // Convert to block with fields in order
            let elements: Vec<Lambda> = fields.into_iter()
                .map(|(_, v)| splice_value(v))
                .collect();
            Lambda::prim(
                Primitive::Pmakeblock(0, TagInfo::Record, Mutable::Immutable),
                elements,
                Location::none(),
            )
        }

        Value::Variant { tag, payload } => {
            match payload {
                Some(p) => Lambda::prim(
                    Primitive::Pmakeblock(
                        tag_to_int(&tag),
                        TagInfo::Variant(tag),
                        Mutable::Immutable,
                    ),
                    vec![splice_value(*p)],
                    Location::none(),
                ),
                None => Lambda::Lconst(Constant::int(tag_to_int(&tag))),
            }
        }

        Value::Type(_) => {
            // Types at runtime become unit (they're compile-time only)
            Lambda::unit()
        }

        Value::Closure { .. } => {
            // Closures become functions
            // This requires more sophisticated handling
            panic!("Cannot splice closure to runtime")
        }
    }
}
```

---

## Replacing PPXs

### Overview

Comptime can replace all current PPX use cases because:

1. **PPXs receive strings** via extension points
2. **Comptime can parse strings** at compile time
3. **Both generate code** based on the parsed/introspected information

### 1. JSON Serialization (ppx_spice, Decco)

**Current PPX:**
```rescript
@spice
type user = {
  name: string,
  @spice.key("user_age") age: int,
  email: option<string>,
}
// Generates: user_encode, user_decode
```

**With Comptime:**
```rescript
// codec.res - Generic JSON codec generator
@comptime
let makeCodec = (comptime T: type): Codec.t<T> => {
  switch @typeInfo(T) {
  | Record({fields}) => {
      encode: (value: T) => {
        let obj = Js.Dict.empty()
        @inline for field in fields {
          let key = switch @getAttr(field, "json.key") {
          | Some(k) => k
          | None => field.name
          }
          let fieldValue = @field(value, field.name)
          Js.Dict.set(obj, key, encodeField(fieldValue, field.typ))
        }
        Js.Json.object_(obj)
      },
      decode: (json: Js.Json.t) => {
        switch Js.Json.classify(json) {
        | JSONObject(dict) =>
          let result = {}
          @inline for field in fields {
            let key = switch @getAttr(field, "json.key") {
            | Some(k) => k
            | None => field.name
            }
            switch Js.Dict.get(dict, key) {
            | Some(v) => @setField(result, field.name, decodeField(v, field.typ))
            | None if field.optional => @setField(result, field.name, None)
            | None => return Error(`Missing field: ${field.name}`)
            }
          }
          Ok(result)
        | _ => Error("Expected object")
        }
      },
    }
  | _ => @compileError("makeCodec requires a record type")
  }
}

@comptime
let encodeField = (value: 'a, typ: Type.t): Js.Json.t => {
  switch typ {
  | Constr({path: "string"}) => Js.Json.string(value)
  | Constr({path: "int"}) => Js.Json.number(Int.toFloat(value))
  | Constr({path: "float"}) => Js.Json.number(value)
  | Constr({path: "bool"}) => Js.Json.boolean(value)
  | Constr({path: "option", args: [inner]}) =>
    switch value {
    | Some(v) => encodeField(v, inner)
    | None => Js.Json.null
    }
  | Constr({path: "array", args: [inner]}) =>
    Js.Json.array(value->Array.map(v => encodeField(v, inner)))
  | Record(_) => makeCodec(typ).encode(value)
  | _ => @compileError(`Unsupported type for JSON encoding: ${@typeName(typ)}`)
  }
}

// Usage
type user = {
  name: string,
  @json.key("user_age") age: int,
  email: option<string>,
}

let userCodec = makeCodec(user)
let json = userCodec.encode({name: "Alice", age: 30, email: Some("alice@example.com")})
```

### 2. GraphQL (graphql-ppx, rescript-relay)

**Current PPX:**
```rescript
module UserQuery = %graphql(`
  query UserQuery($id: ID!) {
    user(id: $id) {
      id
      name
      email
    }
  }
`)
```

**With Comptime:**
```rescript
// graphql/parser.res - GraphQL parser written in ReScript

type token =
  | LBrace | RBrace | LParen | RParen | Colon | Bang | Dollar
  | Name(string) | StringLit(string) | IntLit(int)

type graphqlType =
  | Named(string)
  | NonNull(graphqlType)
  | List(graphqlType)

type variable = {name: string, typ: graphqlType}
type field = {name: string, alias: option<string>, args: array<argument>, selectionSet: array<field>}
and argument = {name: string, value: argumentValue}
and argumentValue = Variable(string) | StringValue(string) | IntValue(int) | BoolValue(bool)

type operation = {
  kind: [#query | #mutation | #subscription],
  name: option<string>,
  variables: array<variable>,
  selectionSet: array<field>,
}

@comptime
let tokenize = (input: string): array<token> => {
  let tokens = []
  let i = ref(0)
  let len = String.length(input)

  while i.contents < len {
    let c = String.get(input, i.contents)
    switch c {
    | '{' => { Array.push(tokens, LBrace); i := i.contents + 1 }
    | '}' => { Array.push(tokens, RBrace); i := i.contents + 1 }
    | '(' => { Array.push(tokens, LParen); i := i.contents + 1 }
    | ')' => { Array.push(tokens, RParen); i := i.contents + 1 }
    | ':' => { Array.push(tokens, Colon); i := i.contents + 1 }
    | '!' => { Array.push(tokens, Bang); i := i.contents + 1 }
    | '$' => { Array.push(tokens, Dollar); i := i.contents + 1 }
    | ' ' | '\n' | '\t' | '\r' | ',' => i := i.contents + 1
    | '#' => {
      // Skip comment
      while i.contents < len && String.get(input, i.contents) != '\n' {
        i := i.contents + 1
      }
    }
    | 'a'..'z' | 'A'..'Z' | '_' => {
      let start = i.contents
      while i.contents < len {
        let ch = String.get(input, i.contents)
        switch ch {
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' => i := i.contents + 1
        | _ => break
        }
      }
      Array.push(tokens, Name(String.sub(input, start, i.contents - start)))
    }
    | '"' => {
      i := i.contents + 1
      let start = i.contents
      while i.contents < len && String.get(input, i.contents) != '"' {
        i := i.contents + 1
      }
      Array.push(tokens, StringLit(String.sub(input, start, i.contents - start)))
      i := i.contents + 1
    }
    | '0'..'9' => {
      let start = i.contents
      while i.contents < len {
        let ch = String.get(input, i.contents)
        switch ch {
        | '0'..'9' => i := i.contents + 1
        | _ => break
        }
      }
      Array.push(tokens, IntLit(Int.fromString(String.sub(input, start, i.contents - start))->Option.getExn))
    }
    | _ => @compileError(`Unexpected character in GraphQL: '${String.make(1, c)}'`)
    }
  }
  tokens
}

@comptime
let parse = (tokens: array<token>): operation => {
  let pos = ref(0)

  let peek = () => tokens[pos.contents]
  let advance = () => { pos := pos.contents + 1 }
  let expect = (expected: token) => {
    if peek() != expected {
      @compileError(`Expected ${tokenToString(expected)}, got ${tokenToString(peek())}`)
    }
    advance()
  }

  // Parse operation type
  let kind = switch peek() {
  | Name("query") => { advance(); #query }
  | Name("mutation") => { advance(); #mutation }
  | Name("subscription") => { advance(); #subscription }
  | _ => #query  // Default to query
  }

  // Parse operation name
  let name = switch peek() {
  | Name(n) if n != "query" && n != "mutation" => { advance(); Some(n) }
  | _ => None
  }

  // Parse variables
  let variables = switch peek() {
  | LParen => {
    advance()
    let vars = []
    while peek() != RParen {
      expect(Dollar)
      let varName = switch peek() {
      | Name(n) => { advance(); n }
      | _ => @compileError("Expected variable name")
      }
      expect(Colon)
      let varType = parseType(tokens, pos)
      Array.push(vars, {name: varName, typ: varType})
    }
    expect(RParen)
    vars
  }
  | _ => []
  }

  // Parse selection set
  expect(LBrace)
  let selectionSet = parseSelectionSet(tokens, pos)
  expect(RBrace)

  {kind, name, variables, selectionSet}
}

// graphql/codegen.res - Type generation

@comptime
let generateTypes = (op: operation, schema: Schema.t): GraphQLModule => {
  // Generate Variables type
  let variablesType = @generateRecord(
    op.variables->Array.map(v => {
      name: v.name,
      typ: graphqlTypeToRescript(v.typ, schema),
      optional: false,
    })
  )

  // Generate Response type from selection set
  let responseType = generateSelectionType(op.selectionSet, schema.queryType, schema)

  {
    Variables: variablesType,
    Response: responseType,
    query: operationToString(op),
  }
}

// Usage
@comptime
let graphql = (comptime query: string): GraphQLModule => {
  let tokens = GraphQL.Parser.tokenize(query)
  let operation = GraphQL.Parser.parse(tokens)
  GraphQL.Validator.validate(operation, Schema.schema)
  GraphQL.Codegen.generateTypes(operation, Schema.schema)
}

module UserQuery = graphql(`
  query UserQuery($id: ID!) {
    user(id: $id) {
      id
      name
      email
    }
  }
`)

// Generated types available:
// UserQuery.Variables = {id: string}
// UserQuery.Response = {user: {id: string, name: string, email: option<string>}}
// UserQuery.query = "query UserQuery($id: ID!) { ... }"
```

### 3. CSS-in-JS (styled-ppx)

**Current PPX:**
```rescript
module Button = %styled.button(`
  background-color: blue;
  padding: 10px 20px;
  &:hover {
    background-color: darkblue;
  }
`)
```

**With Comptime:**
```rescript
// css/parser.res - CSS parser

type cssValue =
  | Keyword(string)
  | Length(float, string)  // 10px, 2em, etc.
  | Color(string)
  | Number(float)

type cssRule = {
  property: string,
  value: cssValue,
}

type cssBlock = {
  selector: string,
  rules: array<cssRule>,
  nested: array<cssBlock>,
}

@comptime
let parseCSS = (input: string): cssBlock => {
  let tokens = tokenizeCSS(input)
  parseCSSBlock(tokens, "&")  // Root selector is &
}

@comptime
let tokenizeCSS = (input: string): array<cssToken> => {
  let tokens = []
  let i = ref(0)
  let len = String.length(input)

  while i.contents < len {
    let c = String.get(input, i.contents)
    switch c {
    | '{' => { Array.push(tokens, LBrace); i := i.contents + 1 }
    | '}' => { Array.push(tokens, RBrace); i := i.contents + 1 }
    | ':' => { Array.push(tokens, Colon); i := i.contents + 1 }
    | ';' => { Array.push(tokens, Semi); i := i.contents + 1 }
    | ' ' | '\n' | '\t' => i := i.contents + 1
    | '-' | 'a'..'z' | 'A'..'Z' => {
      let start = i.contents
      while i.contents < len {
        let ch = String.get(input, i.contents)
        switch ch {
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' => i := i.contents + 1
        | _ => break
        }
      }
      Array.push(tokens, Ident(String.sub(input, start, i.contents - start)))
    }
    | '0'..'9' | '.' => {
      let start = i.contents
      while i.contents < len {
        let ch = String.get(input, i.contents)
        switch ch {
        | '0'..'9' | '.' => i := i.contents + 1
        | _ => break
        }
      }
      let numStr = String.sub(input, start, i.contents - start)
      // Check for unit
      let unitStart = i.contents
      while i.contents < len {
        let ch = String.get(input, i.contents)
        switch ch {
        | 'a'..'z' | '%' => i := i.contents + 1
        | _ => break
        }
      }
      let unit = String.sub(input, unitStart, i.contents - unitStart)
      Array.push(tokens, Dimension(Float.fromString(numStr)->Option.getExn, unit))
    }
    | '#' => {
      i := i.contents + 1
      let start = i.contents
      while i.contents < len {
        let ch = String.get(input, i.contents)
        switch ch {
        | '0'..'9' | 'a'..'f' | 'A'..'F' => i := i.contents + 1
        | _ => break
        }
      }
      Array.push(tokens, Hash(String.sub(input, start, i.contents - start)))
    }
    | '&' => { Array.push(tokens, Ampersand); i := i.contents + 1 }
    | _ => i := i.contents + 1
    }
  }
  tokens
}

// css/validator.res - CSS property validation

let validProperties = [
  "background-color", "color", "padding", "margin", "width", "height",
  "display", "flex", "border", "font-size", "font-weight", // ...
]

@comptime
let validateCSS = (block: cssBlock): unit => {
  block.rules->Array.forEach(rule => {
    if !Array.includes(validProperties, rule.property) {
      @compileError(`Unknown CSS property: ${rule.property}`)
    }
  })
  block.nested->Array.forEach(validateCSS)
}

// css/codegen.res - Generate Emotion styles

@comptime
let generateClassName = (css: string): string => {
  let hash = hashString(css)
  `css-${hash}`
}

@comptime
let styled = (comptime tag: string, comptime css: string): React.component<{..}> => {
  let block = CSS.Parser.parseCSS(css)
  CSS.Validator.validateCSS(block)

  let className = generateClassName(css)
  let cssString = CSS.Printer.print(block)

  // Inject CSS at module load time
  @sideEffect
  Emotion.injectGlobal(`.${className} { ${cssString} }`)

  // Return component
  React.forwardRef((props, ref) => {
    React.createElement(
      tag,
      {
        ...props,
        className: className,
        ref: ref,
      }
    )
  })
}

// Usage
let Button = styled("button", `
  background-color: blue;
  padding: 10px 20px;
  &:hover {
    background-color: darkblue;
  }
`)

// Usage in JSX
<Button onClick={handleClick}> {React.string("Click me")} </Button>
```

### 4. SQL (rescript-embed-lang)

**Current approach:**
```rescript
let findMovie = %sql.one(`SELECT id, title FROM movies WHERE id = $id`)
```

**With Comptime:**
```rescript
// sql/parser.res - SQL parser

type sqlExpr =
  | Column(string)
  | Param(string)
  | Literal(literal)
  | BinOp(sqlExpr, binOp, sqlExpr)

type selectQuery = {
  columns: array<selectColumn>,
  from: string,
  where: option<sqlExpr>,
  orderBy: array<(string, [#asc | #desc])>,
  limit: option<int>,
}

@comptime
let parseSQL = (input: string): sqlQuery => {
  let tokens = tokenizeSQL(input)
  parseSQLQuery(tokens)
}

// sql/codegen.res - Type generation

@comptime
let sql = (comptime query: string, comptime schema: DBSchema): SQLQuery => {
  let parsed = SQL.Parser.parseSQL(query)

  // Extract parameters ($id, $name, etc.)
  let params = extractParams(parsed)

  // Infer column types from schema
  let columns = switch parsed {
  | Select(q) => q.columns->Array.map(col => {
      let table = schema.tables->Array.find(t => t.name == q.from)->Option.getExn
      let column = table.columns->Array.find(c => c.name == col.name)->Option.getExn
      {name: col.alias->Option.getOr(col.name), typ: column.typ}
    })
  | _ => []
  }

  // Generate types
  let paramsType = @generateRecord(params->Array.map(p => {
    name: p.name,
    typ: p.typ,
    optional: false,
  }))

  let resultType = @generateRecord(columns->Array.map(c => {
    name: c.name,
    typ: sqlTypeToRescript(c.typ),
    optional: c.nullable,
  }))

  {
    Params: paramsType,
    Result: resultType,
    query: query,
    execute: (db, params) => db.query(query, params),
  }
}

// Usage
let findMovie = sql(
  `SELECT id, title FROM movies WHERE id = $id`,
  MovieDB.schema
)

// Type-safe usage:
let movie = await findMovie.execute(db, {id: "123"})
// movie: {id: string, title: string}
```

### 5. TypeScript Binding Helpers (ppx_ts)

**Current PPX:**
```rescript
@ppx_ts.keyOf
type config = {
  host: string,
  port: int,
}
// Generates: type config_keyOf = Host | Port
```

**With Comptime:**
```rescript
@comptime
let keyOf = (comptime T: type) => {
  switch @typeInfo(T) {
  | Record({fields}) =>
    @generateVariant(
      fields->Array.map(f => {
        name: String.capitalize(f.name),
        payload: None,
      })
    )
  | _ => @compileError("keyOf requires a record type")
  }
}

@comptime
let keyToString = (comptime T: type) => {
  let keyType = keyOf(T)
  (key: keyType): string => {
    switch key {
    @inline for field in @typeInfo(T)->getFields {
      | @variant(String.capitalize(field.name)) => field.name
    }
    }
  }
}

// Usage
type config = {
  host: string,
  port: int,
}

type configKey = keyOf(config)  // Host | Port
let toString = keyToString(config)

let key: configKey = Host
Console.log(toString(key))  // "host"
```

### 6. Form Bindings (ppx_react_hook_form)

**With Comptime:**
```rescript
@comptime
let makeForm = (comptime T: type) => {
  switch @typeInfo(T) {
  | Record({fields}) =>
    {
      useForm: () => {
        let form = ReactHookForm.useForm()

        // Generate register functions for each field
        @inline for field in fields {
          let register = form.register(field.name)
        }

        form
      },

      // Type-safe field accessors
      fields: @generateRecord(
        fields->Array.map(f => {
          name: f.name,
          typ: FormField(f.typ),
          optional: false,
        })
      ),
    }
  | _ => @compileError("makeForm requires a record type")
  }
}

// Usage
type loginForm = {
  email: string,
  password: string,
  rememberMe: bool,
}

let LoginForm = makeForm(loginForm)

@react.component
let make = () => {
  let form = LoginForm.useForm()

  <form onSubmit={form.handleSubmit(data => Console.log(data))}>
    <input {...form.fields.email.register()} />
    <input {...form.fields.password.register()} type_="password" />
    <input {...form.fields.rememberMe.register()} type_="checkbox" />
    <button type_="submit"> {React.string("Login")} </button>
  </form>
}
```

---

## Implementation Plan

### Phase 1: Core Interpreter

**Goal:** Basic comptime evaluation of constants and simple functions

**Tasks:**
1. Add `@comptime` attribute parsing
2. Implement `ComptimeInterpreter` in Rust
3. Support basic types: int, float, string, bool, array, record
4. Implement arithmetic and string primitives
5. Add fuel limits and error handling
6. Implement value splicing back to Lambda IR

**Deliverables:**
- `compiler-rust/src/lambda/comptime/mod.rs`
- `compiler-rust/src/lambda/comptime/interpreter.rs`
- `compiler-rust/src/lambda/comptime/value.rs`
- `compiler-rust/src/lambda/comptime/splice.rs`

### Phase 2: Type Introspection

**Goal:** Enable `@typeInfo` and related builtins

**Tasks:**
1. Define `ComptimeType` enum in Rust
2. Implement `type_to_comptime` conversion
3. Add `Primitive::PcomptimeTypeInfo`
4. Implement `@typeInfo`, `@typeName`, `@hasField`, `@fieldType`
5. Add `@getAttr` for attribute access
6. Implement `@field` for runtime field access with comptime name

**Deliverables:**
- `compiler-rust/src/lambda/comptime/types.rs`
- Extended `Primitive` enum
- Type introspection tests

### Phase 3: Code Generation

**Goal:** Enable generating types and code from comptime

**Tasks:**
1. Implement `@generateRecord` primitive
2. Implement `@generateVariant` primitive
3. Implement `@inline for` loop unrolling
4. Add `@compileError` for user-defined errors
5. Implement module generation

**Deliverables:**
- Code generation primitives
- Loop unrolling in Lambda generation
- Module synthesis

### Phase 4: Standard Library

**Goal:** Provide parsers and utilities for common use cases

**Tasks:**
1. Write JSON codec generator
2. Write basic parsers (tokenizer utilities)
3. Write SQL parser
4. Write GraphQL parser
5. Write CSS parser
6. Document patterns and best practices

**Deliverables:**
- `packages/@rescript/comptime/` library
- Parser combinator utilities
- Example implementations

### Phase 5: Tooling

**Goal:** IDE support and developer experience

**Tasks:**
1. LSP support for comptime errors
2. Hover information for generated types
3. Go-to-definition through comptime
4. Debugging support (comptime traces)

---

## Examples

### Complete Example: Type-Safe API Client

```rescript
// api/schema.res - API schema definition
type endpoint<'params, 'response> = {
  method: [#GET | #POST | #PUT | #DELETE],
  path: string,
  params: 'params,
  response: 'response,
}

type user = {
  id: string,
  name: string,
  email: string,
}

type createUserParams = {
  name: string,
  email: string,
}

// api/client.res - Client generator
@comptime
let makeClient = (comptime endpoints: array<(string, endpoint<'p, 'r>)>) => {
  @generateRecord(
    endpoints->Array.map(((name, ep)) => {
      name: name,
      typ: Function({
        params: [@typeInfo(ep.params)],
        return: Promise(@typeInfo(ep.response)),
      }),
      optional: false,
    })
  )
}

// api/endpoints.res - Define endpoints
let endpoints = [
  ("getUser", {
    method: #GET,
    path: "/users/:id",
    params: type {id: string},
    response: type user,
  }),
  ("createUser", {
    method: #POST,
    path: "/users",
    params: type createUserParams,
    response: type user,
  }),
  ("deleteUser", {
    method: #DELETE,
    path: "/users/:id",
    params: type {id: string},
    response: type {success: bool},
  }),
]

// Usage
let client = makeClient(endpoints)

// Fully typed!
let user = await client.getUser({id: "123"})
let newUser = await client.createUser({name: "Alice", email: "alice@example.com"})
let result = await client.deleteUser({id: "123"})
```

### Complete Example: ORM Query Builder

```rescript
// orm/schema.res
type column<'t> = {
  name: string,
  typ: 't,
  nullable: bool,
  primaryKey: bool,
}

type table<'row> = {
  name: string,
  columns: array<column<_>>,
}

// orm/query.res
@comptime
let makeTable = (comptime name: string, comptime T: type): table<T> => {
  switch @typeInfo(T) {
  | Record({fields}) =>
    {
      name: name,
      columns: fields->Array.map(f => {
        name: f.name,
        typ: f.typ,
        nullable: f.optional,
        primaryKey: @hasAttr(f, "primaryKey"),
      }),
    }
  | _ => @compileError("Table row must be a record type")
  }
}

@comptime
let select = (comptime table: table<'row>): SelectQuery<'row> => {
  {
    from: table.name,
    columns: All,
    where: None,
    orderBy: [],
    limit: None,

    where: (condition) => { ...self, where: Some(condition) },
    orderBy: (column, dir) => { ...self, orderBy: [...self.orderBy, (column, dir)] },
    limit: (n) => { ...self, limit: Some(n) },

    execute: async (db) => {
      let sql = buildSQL(self)
      let rows = await db.query(sql)
      rows->Array.map(row => parseRow(row, table))
    },
  }
}

// Usage
type user = {
  @primaryKey id: string,
  name: string,
  email: option<string>,
  createdAt: Date.t,
}

let Users = makeTable("users", user)

let activeUsers = await
  select(Users)
  ->where(u => u.createdAt > Date.now() - days(30))
  ->orderBy("createdAt", #desc)
  ->limit(10)
  ->execute(db)

// activeUsers: array<user>
```

---

## Appendix: Zig Comptime Reference

### How Zig Does It

Zig's compiler uses a multi-stage pipeline:

1. **AST → ZIR**: Source is parsed into Zig Intermediate Representation
2. **ZIR → AIR**: Semantic analysis transforms ZIR to Analyzed IR
3. **Comptime evaluation happens in Sema** (step 2)

The Sema phase attempts to evaluate each instruction:
- If operands are comptime-known → evaluate immediately
- If runtime values needed → generate AIR instructions

### Key Zig Features

```zig
// comptime keyword forces compile-time evaluation
const len = comptime multiply(4, 5);  // 20

// comptime parameters
fn Vec(comptime T: type, comptime N: usize) type {
    return struct {
        data: [N]T,
    };
}

// Type introspection
fn dump(value: anytype) void {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .Int => std.debug.print("int: {}", .{value}),
        .Struct => |s| {
            inline for (s.fields) |field| {
                std.debug.print("{}: {}\n", .{field.name, @field(value, field.name)});
            }
        },
        // ...
    }
}

// inline for - unrolled at comptime
inline for (fields) |field| {
    // This generates separate code for each field
}
```

### Zig Limitations

Zig comptime has deliberate restrictions:

1. **No I/O** - Compilation is hermetic
2. **No heap allocation** - Stack-based evaluation only
3. **No runtime type info** - Types exist only at comptime
4. **No custom syntax** - Only Zig syntax, operates on values

### ReScript Advantages

Our design has some advantages over Zig:

1. **Heap allocation allowed** - Interpreter runs in Rust
2. **Richer type system** - Records, variants, objects
3. **String parsing** - Full parser combinators possible
4. **Attribute system** - Can read `@decorator` attributes

### References

- [Zig Comptime Guide](https://zig.guide/language-basics/comptime/)
- [What is Zig's Comptime?](https://kristoff.it/blog/what-is-zig-comptime/)
- [Things Zig comptime Won't Do](https://matklad.github.io/2025/04/19/things-zig-comptime-wont-do.html)
- [Comptime Zig ORM](https://matklad.github.io/2025/03/19/comptime-zig-orm.html)
- [Zig Sema: ZIR → AIR](https://mitchellh.com/zig/sema)
- [Zig Parser Combinators](https://devlog.hexops.com/2021/zig-parser-combinators-and-why-theyre-awesome/)
