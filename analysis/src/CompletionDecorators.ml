let local =
  [
    ( "as",
      Some "as(\"$0\")",
      [
        {|The `@as` decorator is commonly used on record types to alias record field names to a different JavaScript attribute name.

This is useful to map to JavaScript attribute names that cannot be expressed in ReScript (such as keywords).

It is also possible to map a ReScript record to a JavaScript array by passing indices to the `@as` decorator.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#as-decorator).|};
      ] );
    ( "dead",
      None,
      [
        {|The `@dead` decorator is for reanalyze, a static analysis tool for ReScript that can do dead code analysis.

`@dead` suppresses reporting on the value/type, but can also be used to force the analysis to consider a value as dead. Typically used to acknowledge cases of dead code you are not planning to address right now, but can be searched easily later.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#dead-decorator).

> Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
      ] );
    ( "deriving",
      Some "deriving($0)",
      [
        {|When the `@deriving` decorator is applied to a record type, it expands the type into a factory function plus a set of getter/setter functions for its fields.
  
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#deriving-decorator).|};
      ] );
    ( "deprecated",
      Some "deprecated(\"$0\")",
      [
        {|The `@deprecated` decorator is used to add deprecation notes to types, values and submodules. The compiler and editor tooling will yield a warning whenever a deprecated entity is being used.

Alternatively, use the `@@deprecated` decorator to add a deprecation warning to the file level.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#expression-deprecated-decorator).|};
      ] );
    ( "doesNotRaise",
      None,
      [
        {|The `@doesNotRaise` decorator is for reanalyze, a static analysis tool for ReScript that can perform exception analysis.

`@doesNotRaise` is uses to override the analysis and state that an expression does not raise any exceptions,
even though the analysis reports otherwise. This can happen for example in the case of array access where
the analysis does not perform range checks but takes a conservative stance that any access
could potentially raise.
[Read more and see examples in the documentation](https://github.com/rescript-association/reanalyze/blob/master/EXCEPTION.md).
> Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
      ] );
    ( "genType",
      None,
      [
        {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.
  
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#gentype-decorator).|};
      ] );
    ( "genType.as",
      Some "genType.as(\"$0\")",
      [
        {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.

[Read more and see examples in the documentation](https://rescript-lang.org/docs/gentype/latest/usage).|};
      ] );
    ( "genType.import",
      None,
      [
        {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.

[Read more and see examples in the documentation](https://rescript-lang.org/docs/gentype/latest/usage).|};
      ] );
    ( "genType.opaque",
      None,
      [
        {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.

[Read more and see examples in the documentation](https://rescript-lang.org/docs/gentype/latest/usage).|};
      ] );
    ( "get",
      None,
      [
        {|The `@get` decorator is used to bind to a property of an object.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#get-decorator).|};
      ] );
    ( "get_index",
      None,
      [
        {|The `@get_index` decorator is used to access a dynamic property on an object, or an index of an array.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#get-index-decorator).|};
      ] );
    ( "inline",
      None,
      [
        {|The `@inline` decorator tells the compiler to inline its value in every place the binding is being used, rather than use a variable.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#inline-decorator).|};
      ] );
    ( "int",
      None,
      [
        {|The `@int` decorator can be used with polymorphic variants and the @as decorator on externals to modify the compiled JavaScript to use integers for the values instead of strings.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#int-decorator).|};
      ] );
    ( "live",
      None,
      [
        {|The `@live` decorator is for reanalyze, a static analysis tool for ReScript that can do dead code analysis.

`@live` tells the dead code analysis that the value should be considered live, even though it might appear to be dead. This is typically used in case of FFI where there are indirect ways to access values. It can be added to everything that could otherwise be considered unused by the dead code analysis - values, functions, arguments, records, individual record fields, and so on.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#live-decorator).

Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
      ] );
    ( "meth",
      None,
      [
        {|The `@meth` decorator is used to call a function on a JavaScript object, and avoid issues with currying.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#meth-decorator).|};
      ] );
    ( "module",
      Some "module(\"$0\")",
      [
        {|The `@module` decorator is used to bind to a JavaScript module.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#module-decorator).|};
      ] );
    ( "new",
      None,
      [
        {|
The `@new` decorator is used whenever you need to bind to a JavaScript class constructor that requires the new keword for instantiation.|

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#new-decorator).|};
      ] );
    ( "obj",
      None,
      [
        {|The `@obj` decorator is used to create functions that return JavaScript objects with properties that match the function's parameter labels.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#obj-decorator).|};
      ] );
    ( "raises",
      Some "raises(\"$0\")",
      [
        {|The `@raises` decorator is for reanalyze, a static analysis tool for ReScript that can perform exception analysis.

`@raises` acknowledges that a function can raise exceptions that are not caught, and suppresses
a warning in that case. Callers of the functions are then subjected to the same rule.
Example `@raises(Exn)` or `@raises([E1, E2, E3])` for multiple exceptions.
[Read more and see examples in the documentation](https://github.com/rescript-association/reanalyze/blob/master/EXCEPTION.md).
> Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
      ] );
    ( "react.component",
      None,
      [
        {|The `@react.component` decorator is used to annotate functions that are RescriptReact components.

You will need this decorator whenever you want to use a ReScript / React component in ReScript JSX expressions.

Note: The `@react.component` decorator requires the `jsx` config to be set in your `rescript.json`/`bsconfig.json` to enable the required React transformations.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#react-component-decorator).|};
      ] );
    ( "jsx.component",
      None,
      [
        {|The `@jsx.component` decorator is used to annotate functions that are JSX components used with ReScript's [generic JSX transform](https://rescript-lang.org/docs/manual/latest/jsx#generic-jsx-transform-jsx-beyond-react-experimental).

You will need this decorator whenever you want to use a JSX component in ReScript JSX expressions.|};
      ] );
    ( "return",
      Some "return(${1:nullable})",
      [
        {|The `@return` decorator is used to control how `null` and `undefined` values are converted to option types in ReScript.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#return-decorator).|};
      ] );
    ( "scope",
      Some "scope(\"$0\")",
      [
        {|The `@scope` decorator is used with other decorators such as `@val` and `@module` to declare a parent scope for the binding.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#scope-decorator).|};
      ] );
    ( "send",
      None,
      [
        {|The `@send` decorator is used to bind to a method on an object or array.
  
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#send-decorator).|};
      ] );
    ( "set",
      None,
      [
        {|The `@set` decorator is used to set a property of an object.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#set-decorator).|};
      ] );
    ( "set_index",
      None,
      [
        {|The `@set_index` decorator is used to set a dynamic property on an object, or an index of an array.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#set-index-decorator).|};
      ] );
    ( "string",
      None,
      [
        {|The `@string` decorator can be used with polymorphic variants and the `@as` decorator on externals to modify the string values used for the variants in the compiled JavaScript.
  
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#string-decorator).|};
      ] );
    ( "this",
      None,
      [
        {|The `@this` decorator may be used to bind to an external callback function that require access to a this context.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#this-decorator).|};
      ] );
    ( "unboxed",
      None,
      [
        {|The `@unboxed` decorator provides a way to unwrap variant constructors that have a single argument, or record objects that have a single field.
  
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#unboxed-decorator).|};
      ] );
    ( "uncurry",
      None,
      [
        {|The `@uncurry` decorator can be used to mark any callback argument within an external function as an uncurried function without the need for any explicit uncurried function syntax (`(.) => { ... }`).

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#uncurry-decorator).|};
      ] );
    ( "unwrap",
      None,
      [
        {|The `@unwrap` decorator may be used when binding to external functions that accept multiple types for an argument.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#unwrap-decorator).|};
      ] );
    ( "val",
      None,
      [
        {|The `@val` decorator allows you to bind to JavaScript values that are on the global scope.
  
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#val-decorator).|};
      ] );
    ( "variadic",
      None,
      [
        {|The `@variadic` decorator is used to model JavaScript functions that take a variable number of arguments, where all arguments are of the same type.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#variadic-decorator).|};
      ] );
    ( "editor.completeFrom",
      None,
      [
        {|The `@editor.completeFrom` decorator instructs the editor where it can draw additional completions from for this type.|};
      ] );
  ]

let toplevel =
  [
    ( "deprecated",
      Some "deprecated(\"$0\")",
      [
        {|The `@@deprecated` decorator is used to add a deprecation note to the file-level of a module. The compiler and editor tooling will yield a warning whenever a deprecated file module is being used.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#module-deprecated-decorator).|};
      ] );
    ( "directive",
      Some "directive(\"$0\")",
      [
        {|The `@@directive` decorator will output that string verbatim at the very top of the generated JavaScript file, before any imports.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#directive-decorator).|};
      ] );
    ( "warning",
      Some "warning(\"$0\")",
      [
        {|The `@@warning` decorator is used to modify the enabled compiler warnings for the current module. See here for all available warning numbers.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#module-warning-decorator).
         |};
      ] );
    ( "jsxConfig",
      Some "jsxConfig({$0})",
      [
        {|The `@@jsxConfig` decorator is used to change the config for JSX on the fly.

[Read more and see examples in the documentation](https://rescript-lang.org/docs/manual/latest/jsx#file-level-configuration).|};
      ] );
  ]
