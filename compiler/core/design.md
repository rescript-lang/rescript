
# Ideas aobut boolean support
## The cases when boolean representation is not transparent 

- printing

```ocaml
Js.log true
```

- pattern match

```ocaml
let f x y = 
  match x,y with
  | true, false -> 0
```

- comparison

```ocaml
if v = true then 
```

## Where JS boolean could be introduced 

JS operatons which could generate JS booleans
- `not`
- Equality comparison

# `and`, `or` is fine
In JS, `and`, `or` is untyped, but it is a superset of OCaml semantics:


```js
x && y 
/* equivalent to  */ 
x ? x : y
```



There is no coersion so `1&&0`, `1&&1`, `0&&1`, `0&&0` are all the same as JS version.
Same for `or`

but `not` is not the same as `!`, `!` will do the conversion to enforce its result is JS boolean  

**ATT** if you want to support JS boolean better, think twice, it is
really hard to give two booleans first class support, and such bugs
are very hard to find (since in most cases they behave the same), so
what we can do is produce OCaml bool exclusively, only pass JS boolean
to JS ffi which requires it exclusively (very rare)
**NOTE** since OCaml boolean is everywhere, while JS boolean only happens in the FFI, we should by default produce OCaml boolean in the IR, and mark JS boolean explicitly instead.

## It does not affect `if_then_else` compilation

since JS if is more capable, there is no need do any coercion  


# Arity handling

The runtime support is in `Curry` module, we have several functions 

## `Curry.app`

```ocaml
Curry.app f args 
(** [f] is an curried function, [args] are supplied arguments
   if matches then like normal function apply.
   if over-supply, take the first [arity] arguments,
    fully apply and continue [app f' rest]
   if under-supply, 
     create a closure, wait until all arguments ready.
     [Note, we don't necessary 
     need a closure here, we can have some 
     data structures like bytecode]   
 *)
```

```ocaml

Curry.curry_N o a1 a2 .. arity 
(** used by [Curry._N]  *)

Curry._N o a1 a2 .. aN 
(** 
  A fast version of [app f [|a1; a2; ..; aN |]].
  Check the arity of [o]if it hits, just do the application
*)

Curry.__N o 
(**
   Make sure the output of [o] is arity [N]. 
   This is used to convert a curried function [o] into
   uncurried. for example 
   {[ 
     fun [@bs] x y -> f x y 
    ]}
    Another use case:
    {[ 
      external f : ('a -> 'b [@uncurry]) -> unit 

      f g (* The compiler will do such converison internally*)
    ]}
   Guess the arity of [o], if it hit, then return [o]

   Note when we want to target arity 0, in the first case
   {[
     fun [@bs] () -> f ()
   ]} will be compiled as 
   {{
     fun () -> f (0)
   }}
   SO [Curry.__0] will not be triggered
   
   We also have 
   some special logic to when converted to arity 0 
   in external settings
*)

```

# Toplevel module exports

Global exports identifiers are extracted from `Translmod.get_export_identifiers`
instead of inferred from lambda expression or cmi file.

- We need be careful about externals.
- Reading from fresh generated cmi is expensive.

# Variable usage

Lalias-bound variables are never assigned, so it can only 
appear in `Lvar`, then it is easy to eliminate it 


# interaction between `variadic` and `|>`

Note in general, it is fine whether we do beta reduction or not, it is just optimization. 

However, since we introduced `variadic` which does require the `spliced argument` to be captured

```ocaml
spliced_external a0 a1 [|b0;b1|]
```

There are two cases where get things complicated, people don't think `|>` is a function

```ocaml
x |> spliced_external a0 a1 [|b0;b1|]
```
Even though `|>` is a function, and `spliced_external` is escaped here, but people would 
expect it is equivalent to 

```ocaml
spliced_external a0 a1 [|b0;b1|] x 
```

So our optimizer needs to handle this case to make sure `spliced_external` not escaped, 
also becaues the interaction of `[@variadic]` and `[@send]`, the spliced argument
is no longer  in tail position, so that people can write such code

```ocaml
spliced_external a0 a1 [|b0;b1|]
```
Internally in lambda layer it would be 

```ocaml
(fun c0 c1 c2 c3-> spliced_external c0 c1 c2 c3) a0 a1 [|b0;b1|]
```

We can simply do inlining, it may have side efffect in `b0`, `b1`, our optimizer also need handle such case.

Maybe in the future, we should lift the restriction about `variadic` (delegate to `slow` mode when we can not resolve it statically, my personal expereince is that people will complain about why it fails to compile more than why it is slow in some corner cases)

Note this also interacts with `[@uncurry]`

for example

```ocaml
external filter : 'a array -> ('a -> bool [@uncurry]) -> 'a array = "filter"
[@@send]

let f xs =
    xs |. filter (fun x -> x > 2)
```

Here whether the callback gets inlined to the call of `filter` will have an effect on how `Pjs_fn_make` gets cancelled.

Note when we pattern match over the original lamba,`Levent` needs to be removed as early as possible. Due to the existence of `Levent`, we can not pattern match over nested original raw lambda.

We turned off event generation temporarily

# safe way to test undefined

Note such logic is already wrong:

```js
var x = undefined_value // thrown here 
if (typeof x === "undefined"){
    ...
}
```


# `#` primitive handling

1. Some primitives introduced are for performance reasons, for example:

`#String.fromCharCode` which is essentialy the same as 

```ocaml
external of_char : char -> string = "String.fromCharCode"
[@@val]
```

We introduced `#` so that we can do some optimizations.

2. Some of them are not expressible in OCaml FFI, for example
'#is_instance_array',
'#gt'

3. Some of them require a runtime polyfill support


# runtime

## anything to string
http://www.2ality.com/2012/03/converting-to-string.html
Note that `""+ Symbol()` does not work any more, we should favor `String` instead


# Name mangling

## let bound identifier mangling

### keyword
Note there are two issues, if it is keyword, the output may not be parsable if we don't do name mangling


```js
> var case = 3
< SyntaxError: Cannot use the keyword 'case' as a variable name.
```
### global variable 
If it is global variable, it is parsable, it may trigger even subtle errors:

```js
(function(){ 'use strict'; var document = 3; console.log(document)})()
VM1146:1 3
3
```  
This could be _problematic_ for bindings
```ocaml
let process = 3
Process.env##OCAML
```
In general global variables would be problematic for bindings

## property name mangling

Nowadays, JS engine support keywords as property name very well

```js
var f = { true : true, false : false }
```

But it has problems when it is too simple for parsing
```js
var f = { true, false} // parsign rules ambiguity
```

If we don't do ES6, we should not go with name mangling, however, it is mostly due to we can 
not express these keywords, such as `_open` as property in OCaml, so we did the name mangling

# Curry/Tuple: two kinds of function 

OCaml indeed support two kind calling convention.

```ocaml
(Lfunction (Tupled(a0,a1,a2))) [a0,a1,a2]
```

and 

```ocaml
(Lfunction Curried (a0,a1,a2)) a0 a1 a2 
```


They also affect how beta reduction works, 

```ocaml
| Lapply(Lfunction(Curried, params, body), args, _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
| Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args,_)], _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
```

It is generated by the backend via an argument 
`untuplify_fn` in `transl_function`.
Note if we want to take advantage of it in the future we need 
translate ` f x`  into (when `f` is `fun (a0,a1) -> a0 + a1 `

```ocaml
f (x[0],x[1])
```

currently it is turned on only in native mode

```ocaml
and transl_function loc untuplify_fn ..
```

two call sites

```ocaml
transl_function exp.exp_loc false ...
transl_function e.exp_loc !Clflags.native_code ...
```

# JS exception wrap and unwrap

### Pack and Unpack OCaml exceptions

http://eli.thegreenplace.net/2013/10/22/classical-inheritance-in-javascript-es5

http://stackoverflow.com/questions/1382107/whats-a-good-way-to-extend-error-in-javascript


https://caml.inria.fr/mantis/print_bug_page.php?bug_id=7375

http://stackoverflow.com/questions/3734236/how-can-i-rethrow-an-exception-in-javascript-but-preserve-the-stack
```js
class OCamlError extends Error{
  constructor(payload){
    super("OCamlError")
    this.payload = payload
  }
}
```

We can see the output from typescript to get a sense of what it will be transpiled into.

This works reasonably well (tested on Safari and Chrome)

```js
function OCamlError(camlExnData){
  var self = Error.call(this, "OCamlError")
  self.camlExnData = camlExnData
  return self
}
```

```js
function unpackError(exn){
  if(exn.camlExnData !== undefined){
    return exn.camlExnData
  } else {
    return exn
  }
}
```
```js
function packError(exn){
  if (Obj.tag(exn) === 248){
    return new OCamlError(exn)
  } else {
    return exn
  }
}
```
So whenever we raise an OCaml exception, we always wrapped it as a JS exception.
Now we unpack it, it could be OCaml exception or JS exception, so we  did 
a runtime dispatch.

Some potential optimization

```ocaml
try f x with 
Not_found -> ..
JsExn e
```

currently it would be transalted as 

```js
try {
  f(x)
  }
catch(e){
  var e = unpackError(e)
  if (e === "Not_found"){
    ...
  } else {
    throw packError(e) // re-raise
  }
} 
```

need check `Praise of raise_kind`

*Conclusion*: it is very hard to get it right when changig ocaml exception representation and js exception representation at the same time in the combination of *re-raiase*


# Several module components can have the same name #978

Note: ReScript compiler complains if exports have the same component name, this keeps its soundness.

A funny thing is that open variants does not have record disambiguion so that for:

```ocaml
type a = ..
type b = ..
type a += A 
type b += A
```

The compiler will only expose the last `A`, which means, ReScript will not complain, the limitation 
of the compiler preserves its soundness

# Print import module names

for `create_js_module`, we first create a mapping to make it a proper
module name, (also cached in a hashtbl). Note it is not a Js id, which
fails `Ext_ident.is_js`

# compilation

# static catches

# Comparison semantics

Cases when commparison are specialized (Note we need make sure the specialized version
is consistent with the generalized version):

- caml_int_max/min
- caml_bool_max/min
- caml_float_max/min
- caml_string_max/min
- caml_nativeint_max/min
- caml_int32_max/min
- caml_int64_max/min

- int_equal[null/undefined/nullable] [not]
- bool_equal[null/undefined/nullable] [not]
- float_equal[null/undefined/nullable] [not]
- string_equal[null/undefined/nullable] [not]
- nativeintequal[_null/unefined/nullable] [not]
- int32_equal[_null/undefined/nullable] [not]
- int64_equal[_null/undefined/nullable] [not]

- int_lessthan[greaterthan] [lessequal] [greaterequal]
- bool_lessthan[greaterthan] [lessequal] [greaterequal]
- float_lessthan[greaterthan] [lessequal] [greaterequal]
- string_lessthan[greaterthan] [lessequal] [greaterequal]
- nativeint_lessthan[greaterthan] [lessequal] [greaterequal]
- int32_lessthan[greaterthan] [lessequal] [greaterequal]
- int64_lessthan[greaterthan] [lessequal] [greaterequal]

- int_compare
- bool_compare
- float_compare
- string_compare
- nativeint_compare
- int32_comapre
- int64_compare

So far we haven't specialized option comparison, but we need be careful when 
we do the optimizer, e.g, `Js_exp_make.int_comp`, we need make sure the peepwhole is consistent





