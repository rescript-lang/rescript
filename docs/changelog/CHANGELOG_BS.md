# Changelog (bs-platform >= 9.1.4)

## 9.1.4

### Build

- #5167 add dump subcommand so that

```
rescript dump path/to/file.cmi
```

Will dump the interface to a readable output, note this is integrated into the build system that the build will try to build it if it is not already there

- clean will clean its dependency by default.
  subcommand `clean -with-deps`, `-with-deps` is not needed any more
- hide most bsc options, officially supported bsc flags (this is not a breaking change, those internal options are still there but subject to removal in the future)

```
Usage: bsc <options> <files>
Options are:
Options:
  -w                        <list>  Enable or disable warnings according to <list>:
                            +<spec>   enable warnings in <spec>
                            -<spec>   disable warnings in <spec>
                            @<spec>   enable warnings in <spec> and treat them as errors
                            <spec> can be:
                            <num>             a single warning number
                            <num1>..<num2>    a range of consecutive warning numbers
                            default setting is +a-4-9-20-40-41-42-50-61-102
  -bs-g                     Debug mode
  -bs-D                     Define conditional variable e.g, -D DEBUG=true
  -e                        (experimental) set the string to be evaluated in ReScript syntax
  -v                        Print compiler version and location of standard library and exit
  -version                  Print version and exit
  -warn-help                Show description of warning numbers
  -warn-error               <list>  Enable or disable error status for warnings according
                            to <list>.  See option -w for the syntax of <list>.
                            Default setting is -a+5+6+101+109
```

### Syntax

- #432 bad error message for unterminated quote

### Compiler

- #5165 bad error message for uncurried type mistmatch
- #5169 fix a code gen issue with user defined `None`

## 9.1.3 (bug fix release)

### Build

- #5154 when toplevel package-specs get changed, its dependencies should be rebuilt
- #5152 Rebuild not triggered when deletion with nested modules
- #5153 when Different compiler version triggered,
  it should clean the whole dependencies instead of just the repo itself
- #5080 Add back -ws option for the build

### Syntax

- #425 fix CRLF handling for windows
- #414 Fix printing of underscore Pexp_fun sugar in context of Array.get
- #408 Don't parse Int token with suffices as hash ident for poly variants
- #410 Fix parsing of arrow type with objects in constructor declaration args
- #404 fix printing of Osig_module in outcome printer
- #402 Implement printing of `Otyp_module` in outcome printer

## 9.1

- Remove depercated APIs Js.Re.exec, Js.Re.test, Node.Fs.on
- #5014 #5063 #5027 the new ReScript CLI interface
  Thew new CLI is self explainatory:

```
rescript -h
Available flags
-v, -version  display version number
-h, -help     display help
Subcommands:
    build
    clean
    format
    convert
    help
Run rescript subcommand -h for more details,
For example:
    rescript build -h
    rescript format -h
```

- #5025 in place format support, this is subsumed into `rescript format` subcommand
- #5060 #5055
  Add formatting support for stdin/sdout
- #5053 #5050 clean up structural object semantics
- #5037
  Allows coercion from nullary types to int/string.
  This applies to collections types too
- #5029
  int polyvar is compiled into int.
  `#0` is the same as 0 except it is structually typed as `#0`
- #5023 generate interface file in rescript syntax by default
- #5021 makes int64 runtime payload small for just comparison
- #5019, #5017, #5016, #5015, #5010 improve error message
- #5008 don't trigger a rebuild if mjs file changes, fix an infinite loop in watch mode
- #5005 not depending on Format for Arg module, smaller size
- #4985 fix the schema of bsb on `dev` property
- #4967 #4986, #4984, #4971, #4972, #4969 Breaking changes
  Remove ocaml style classes while structural objects and structural typings are simplified. Js.t is no longer needed. If user does not know
  what ocaml style class is, this should not affect them.

## 9.0.2

- #4990 Fix an optimization bug introduced in 9.0

- #4982 pattern match over modules

Provide user with a sugar to pattern match over modules:

```res
let {length, cons} = module(List)
```

More discussions can be found [here](https://forum.rescript-lang.org/t/introducing-an-extension-to-make-pattern-match-works-on-modules/1196)

## 9.0

- #4933 update syntax bf6561bb5d84
  syntax changes listed [here](https://github.com/rescript-lang/syntax/blob/master/CHANGELOG.md#90)
- #4934 generate `@pure` annotations to help esbuild remove more dead code

- #4932 #4931 turn flow syntax checking from a error into warning 103, so it can be turned off as below

```res
@@config({
  flags: ["-w", "-103"],
})

%%raw(`
if (import.meta.hot){
  console.log('es6')
}
`)
```

The rationale is that flow could be not standard compilant so we need provide a work around, here
`import.meta` is something new in Ecmascript

- #4926 #4928
  _internal_ changes, move jscomp/syntax to jscomp/frontend to avoid conflicts

- #4924 #4927 better code generated for pattern match.
  Take advantage of the JS runtime, some predicates can be simplified

- #4920 #4925 support external-stdlib config

```
"external-stdlib" : "@rescript/std"
```

- #4922 #4923 \*breaking changes" Allow embed records in structural js objects

- #4908 #4919 #4917 #4916 #4914 #4913 #4910
  Get rid of camlp4 as a dev dependency, introduce an optimized visitor pattern
  generator, better performance, no object usage and less dependency thanks to wasm

- #4911 Relax uninterpretable attributes from error to warn to make ppx_deriving happy

- #4905 _internal_ add `Js_exn.anyToExnInternal`

- #4903 porting to open BSD/adJ

- #4902 for stdlib es6 artifacts ship .mjs instead of .js, so that
  on the user side, if they config es6 with .mjs, it will work out of box

- #4900 #4986 `'` in string literals does not need to be escaped

- #4893 _internal_ simplify numbers in JS IR

- #4892 #4891 _internal_ simplify boxed int operations

- #4890 clean up constant in lambda IR, fix a subtle bug when do constant folding

- #4888 #4881 support external in private block

- #4882 #4884 #4887 remove nativeint, not allow literlas like `3n`

- #4873 #4875 #4876 better code generation for pattern match

- #4870 fix typo in uncurried error message

- #4867 _internal_ clean up bsb_helper

## 8.4.2

- #4864 #4865
  Not install dev directory for pinned dependencies
- #4863
  for a package, only cmi/cmj artifact changes will trigger a rebuild

## 8.4.1

- Syntax submodule upgrades from 7f5c968 to 7cc70c9
- #4856 #4858
  Improve code generation for pattern match:
  Input:

  ```res
  type t =
    | A
    | B
    | C
    | D (int )
    | E (int)

  let f = x => {
    switch x {
        | A => 0
        | B => 1
        | C => 2
        | D (x) => x
        | E (x) => x + 1
    }
  }
  ```

  Output was:

  ```js
    function f(x) {
      if (typeof x !== "number") {
        if (x.TAG) {
        return x._0 + 1 | 0;
      } else {
        return x._0;
      }

      switch (x) {
        case /* A */0 :
          return 0;
        case /* B */1 :
          return 1;
        case /* C */2 :
          return 2;
    }
  }
  ```

  Now:

  ```js
  function f(x) {
    if (typeof x !== "number") {
      if (x.TAG === /* D */ 0) {
        return x._0;
      } else {
        return (x._0 + 1) | 0;
      }
    }
    switch (x) {
      case /* A */ 0:
        return 0;
      case /* B */ 1:
        return 1;
      case /* C */ 2:
        return 2;
    }
  }
  ```

- #4855 _internal changes_
  changes to compiler-libs will trigger a rebuild of the compiler, this allows us to
  see how changes of compiler-libs affect bsc.exe quickly

- #4850 replace ocp-ocamlres with a lightweight nodejs script, get rid of such dev dependency

- #4854 #4848 #4847 #4844 #4836 #4826 #4824

  Pinned packages support and `-make-world` respect changes of dependencies

- #4840 #4841 more robust handling of removing stale output

- #4831 use relative paths in the command line
  It will be expanded to absolute path right after the compiler see the path,
  such changes work better with the underlying ninja build engine, and should perform slightly better

- #4828 no need pass -o for compiling, inferred directly (with namespace support too)

- #4827 _internal_ the dev version of bsc now behave roughly the same as the released version

- #4825 fix a typo in the warning `%@string` -> `@string`

- #4823 introduce a new warning 109: toplevel expression is expected to have type unit
  It is turned on as warn-error by default. This warning is introduced to avoid partial application errors in a curried language

- #4822 more robust hanlding of : ignore warnings and warn-error when bsb is building dependencies

## 8.3.3

This is a bug release for 8.3.\*

- #4817 _internal_ add an option RES_SKIP_STDLIB_CHECK so that
  for a true monorepo, it does not need follow `node_modules` layout
- #4807 #4815 remove unused code in refmt parser _a lot_ (around 50_000 loc)
  on darwin, the binary size is dropped fom 9.69M to 8.48M
- #4808 add back basic-reason theme to avoid breakage for existing docs
- #4806 Fix broken ocaml build with gcc 10
- #4804 restore back-wards compatibility with `build statement` in generated ninja files
- #4803 fix the bsb build schema location in the error message
- #4802 proper error message when bsconfig.json is missing
- #4801 add a sanity check for name field in bsconfig.json to match real package name
- #4810 #4784 regressions for weird indentation in warning output

## 8.3.1

This is a minor bug fix release for 8.3.0

- capture warnings when rebuild without enforce warn-as-error
- #4716 internal, make ninja a submodule in dev process
- #4722 better dataflow for cases like `let {a;b} as obj = ...`
- no need call `caml_enter_blocking_section` for single threaded compiler
- #4739 fix the interaction of exotic filenames like `[id]` with the build system.

## 8.3

- #4694, #4712 improving/customizing the underlying ninja build system, better performance

- #4681, #4710 creating persistent lib/bs.compiler.log per each build for editor diagnostics

- #4688, #4707 better error message

- - #4702 remove nativeint module which is not meaningful on js platform

- #4701 support both `bs.val` and `val` attributes, in the future to recommend the shorter ones

- #4693 Fix the compiler runtime issue, always flush err_formatter when at_exit

- #4687, #4689, #4691 allow user to customize js file extension in bsconfig.json (checkout the schema )

- #4685, #4624, #4690 allow more character set in filenames to make rescript play better with react native and next.js

- #4684 fix the raise of Sys.is_directory, make bsb works better with Emacs temp files

- #4679 better error message for nonrec GADT

- #4671, #4678 better strategies to remove stale output for the build system

- #4676 (internal) add Config.syntax_kind so that some changes in super_errors can be made upstream

- #4650, #4656, #4657, #4662 always warn-as-error while not degrading user expereince (with the help of build system)

- #4661 (internal) not depending on upstream compenv module

- #4639, #4642 refined static analysis to generate better code

- #4636, #4641 es6 default import support

- #4638 clean up the confusing error message over uncurry label

- #4637 remove unneeded mention of BuckleScript in uncurried message

- #4623 better data flow inference for common pattern: `let {a,b,c} = ...`

- #4622 add html element & observer phantom types

- #4618 fix combination of bs.obj with bs.as so that bs.as can carry more kinds of playload

- #4613 (internal) pass down @inlined attribute from upstream. (the info is passed down, how to make use of it is not done yet)

- #4609 Lift the restriction that user can only define a type with less than 256 constructors

- #4606, #3961 (internal) use is_a_functor from upstream instead of guessing

- #4605 (experimental) take `@@inline` attribute into consideration for functions

- #4604 enhance Random module

- #4600, #4599 fix missing bounds checking for Bytes.set

- #4597 fix Js.Array and Js.Array2 the wrong return type for `from` method

- #4513 better error message when interface/implementation mismatches (done in commit db485f1)

## 8.2

- `bsc -fmt myFile` now changed to `bsc -format myFile`

- #4573, #4565, #4559, #4558, #4556, #4554, #4553, #4550 introudce string literal types

- #4580 #4581, #4582, #4463, #4583 relax bs.as to allow object literals instead of json, so FFI below is allowed:

  ```ocaml
  external fff0 : int -> int -> (_[@bs.as {json|[undefined,undefined]|json}]) -> int = "say"
  [@@bs.val]

  let testUndefined () =
    fff0 1 2
  ```

- #4570 refine purity analysis so that object literals in raw will be considered pure

- #4548 #4555 fix ghost locaption in empty array

- #4540 optimize code generation for recursive modules
- #4530 internal -color option default to always

- #4569 emit a warning for use of `` ( [ `a| `b] [@bs.string])  `` since it is no longer needed
- #4531 better generated js code for belt
- #4526 add `bsc -fmt file` option, format into the new syntax
- #4495 enable newish es syntax in raw

- #4491, #4492, #4493 fix a bug when printing a single object literal as statement, optimize this case into a nop

- #4482, #4480 disable user to redefine `unit`, `true`, `false`
- #4474 #4465 [reactjs] add support for ref argument inside of React.forwardRef type applciations
- #4473 adding an experimental new syntax
- #4470 tweak error message for not found record fields/constructors

* bug fixes

- #4572, #4585 fix a corner case for ffi to allow such bindings:
  ```ocaml
  external get : _ -> _ -> _ = "" [@@bs.get_index]
  ```
- #4589 fix building failure on freebsd
- #4524, #4519 fix an inlining bug
- #4505 #4502, fix bad error message in bsb -bla

* internal

- #4497 #4507, #4516 compiler performance improvement

## 8.0

- Code generation

* #4308,#4309, #4397 #4403 #4404 #4409 variant as objects
  ```
  A (0,1)
  ```
  now is
  ```js
  { TAG : /*A*/0, _0 : 0, _1:1 }
  ```
* #4399 remove magics in Belt.List to prepare new data representations
* #4405 polyvar as objects
  ```
  A 1
  ```
  now is
  ```js
  {HASH:MAGIC_NUMBER, VAL:1}
  ```
* #4331,#4332 #4337,#4339, #4338, #4337 Encoding exception as dictionary, add stacktrace support
* #4322, #4325,#4326, #4364, #4383, #4371 lazy values as objects, make `caml_update_dummy` generalized
* #4456, #4458 optimize String.make
* #4447, #4442 improve arity inference over raw named function expression
* #4413 changed internal encoding of Some ((Some .. None)), not relying on physiclal equivalence, friendlier to serialization
* #4379 make bs.config take effect ASAP, however, it can not happen befor parsing.
  This meangs, it won't have effect over flags like `-bs-D` when it happens in lexing.
* #4426, #4428 apply bs.inline to float literals
* #4378 apply bs.inline to int64, proper error for not supported types
* #4425 optimize `bs.as "0"` to allow users to turn record representation into array when needed
* #4407, #4423 Fix compatiblity layer between debug mode and none-debug mode
  - For exmaple, `A (1,2)` are equal for code generated either in debug or non-debug mode
* #4422 remove Unix module from stdlib
* #4421 special encode list as `{hd : v0, tl : ...}`
* #4420 remove legacy jsx v2

* #4390 less parens for `bs.as` json literals
* #4245, #4385 breaking changes: enable strict-sequence, strict-formats by default
* #4304, #4293 better code generated for string interpolation

- Fix

* #4451, #4454 fix imprecise locations over pipe
* #4442 fix gentypeconfig.language parsing
* #4430 when cleaning generated files, use `langauge` from `gentypeconfig`
* #4324 fix react-hooks theme name field

- Libs

* #4443 provide a best effort generic seralization mechanism
* #4427 better error message for non-existing module in `sources[n].public`
* #4414, #4419 better error message for cases like below
  ```
  Error: This expression should not be a function, the expected type is
  (int bounce -> 'a bounce [@bs])
  ```
* #4416 fix typo (unrecognized primitive -> unrecognized primitive)
* #4408 [playground] fix jsoo_refmt_main, remove load_modules (uneeded)
* #4395 fix jsoo_refmt_main, delete playground folder
* #4394 repl.js: add dev mode
* #4393 Playground: bring back load_modules API
* #4389 React JSX ppx: better error message for missing labels
* #4388 [jsx] fix unused pattern warnings
* #4380 [jsx] remove bs.config handling in jsx ppx, use bs.config.flags instead
* #4376 remove Math.imul polyfill, inline Math.imul
* #4370 remove deprecated support for `fun%raw`
* #3845, #4367 fix `output too many parens for callback`
* #4359 [jsx] rais error if creating react.component with unnamed argument
* #4364 remove customised formatter for exceptions
* #4358 change reasonreact version for templates
* #4351 force binding location on the actual make binding

- Internal tools/refactoring

* #4460 bspack is a stand alone file not relying on c stubs
* #4459 bsb customized command line parsing, prettier output over `bsb -h`
* #4455 simplify bsb_helper command line parsing
* #4458 clean up lexing runtime, stay close to lexing.c
* #4441 remove unused Obj.set_tag
* #4438 tweak post-processing after linking, better constant substitution
* #4429 #4431 #4435 minor breaking changes
  - more compatct encoding of .bsbuild
  - flatten dev gropus
  - for files in dev directory they can refer each other
* #4402 move `caml_set_oo_id` into `caml_oo` module
* #4392 remove unused return_exp in J.ml ast
* #4387 fix a build bug when no mli provided, add reason-react for testing
* #4366, #4373 update polymoprhic comparison not relying for lazy values
* #4372 vendor jsoo byte code, build playground on the fly
* #4354 `bstracing -all` can handle multiple build sessions

## 7.3.2

- #4315 synup super_errors for better uncurry error message
- #4335 fix js_date.setUTCTime
- #4343 fix regex syntax checking for empty or comment string
- #4351 precie binding location for reasonreact jsx

## 7.3

- #4235 #4237 #4241 #4244 #4240 generalized uncurry work
- #4255 #4258 #3953 code generation: compiling unit to `undefined`
- #4288 code generation: improve module alias code generation
- #4287 code generation: meaningful names for pattern match
- #4286 code generation: better code generation for loops
- #4224 #4262 code generation: eleminate intermediate variables when inlining
- #4198 #4228 remove some long deprecated APIs, Js.Math.pow_int
- #4257 docs: add docs about Belt.Id.comparable
- #4256 tweaks to super error message
- #4284 regression fix: better dumping cmi files to avoid scary names
- #4285 #4268 turn off warning 20 by default (which produces false alarm)
- #4283 tweak react-hooks template
- #4270 #4273 deriving abstract free in belt, belt no longer relies on such extension
- #4272 code generation: better handling of option unpacking
- #4269 not rely on `npm link` for bsb templates
- #4266 #4265 fix belt.MutableMap
- #4259 consistent warning docs in `bsc -warn-help`
- #4251 remove String.prototype.polyfill
- #4250 adding `-unboxed-types` option (where @unboxed attribute may not be needed)
- #4217 make compiler int64 encoding agnostic

## 7.2

- #4211 #4210 optimize int64 performance, Int64.to_string (10x faster), Int64.neg etc
- #4209 add a space after "acquire lock" in bsb error handling
- #4208 #4206 let%private support
  ```ocaml
  let %private  x = 3
  ```
  `x` will not be exported
  ```ocaml
  module N = struct
    let %private x = 3
  end
  ```
  `x` will not be exported by N
- #4196 fix printing indexed operators
- #4177 #4180 support `[@@@bs.config {flags = [| ".." |] }]` per file level to allow file level special flags

- #4158 #4157 #4166 #4168 loading stdlib from memory, no postinstall needed
- #4152 support copyright style comments preserved in JS
  ```
  [%%raw "//copyright ]
  ```
  copyright will be preserved in output js
- #4191 #4189 add a flag -bs-unsafe-empty-array for easy transition (regain polymorphism for empty array), this is a temporary flag which will be removed eventually

- #4190 (internal) remove bsdep which is not used
- #4188 better encoding around internals for performance and size
- #4155 fix React PPX regressions from 7.1.0 which caused a type error when writing recursive components.
- #4185 remove stale tasks.json in bsb themes for vscode, leave it for users to keep it up to date
- #4159 #4161 #4182 improve the startup time, reducing both the size of cmi and cmj
- #4179 (internal) remove bsppx, use "bsc.exe -as-ppx" for editor tooling
- #4171 add a warning for using `fun%raw`, use `[%raw]` directly
- #4169 An escape hatch for function level comments
- #4164 #4162 #4165 make code generation platform agnostic (not depending on printf either)
- #4164 add Node.Buffer.toStringWithEncoding
- #4150 Grab the hostname from window.location when conntecting to websocket for react-hooks theme
- #4143 better compilation of optional arguments
- #4142 fix yarn start command
- #4140 docs: update README in basic reason template

## 7.1.0

(it was 7.0.2 but bumped into 7.1.0 due to a breaking change introduced in 7.0.2)

- #4131 support `bstracing -C`

## 7.0.2

- #4117 Upgrade to Reason 3.6.0 @ 8f71db0
- #4097 introduce a js parser for syntax checking inside raw.

  We can now tell whether the code inside raw is a function or not and the arity of raw function, so

  ```ocaml
  let f = [%raw{|function(x){return x}|}]
  ```

  ```ocaml
  let f = fun%raw x -> {|x|}
  ```

  will be treated the same, to make FFI simpler, the special form `fun%raw` will be discouraged.

- #4090 #4087 fix the unsoundess issue of Js.Array.push
  now the empty array `[||]` won't have a polymorophic type, it will have a concrete type.
  This is a bug fix but also a breaking change
- #4038 Duplicated package warning for symlinked folders
- #41112 #4111 #4067 #4101 provide websocket error better error message and better docs

- #4108 fix warning concat in some edge cases (bsb)
- #4100 make node process exit return 'a instead of unit
- #4098 restore encoding int64 using a tuple for compatibility reasons

- #4114 fix SPA mode watcher path in react-hooks template

- #4199 Change ReactJS PPX to avoid modifying locations of existing code (better integration with editors)
- #4122 toplevel [@@@warning "+101"] works for our own warnings (apart from OCaml ones)

  `bsc -warn-help` listed several of our own warnings

  ```
  101 BuckleScript warning: Unused bs attributes
  102 BuckleScript warning: polymorphic comparison introduced (maybe unsafe)
  103 BuckleScript warning: about fragile FFI definitions
  104 BuckleScript warning: bs.deriving warning with customized message
  105 BuckleScript warning: the external name is inferred from val name is unsafe from refactoring when changing value name
  106 BuckleScript warning: Unimplemented primitive used:
  107 BuckleScript warning: Integer literal exceeds the range of representable integers of type int
  108 BuckleScript warning: Uninterpreted delimiters (for unicode)
  ```

- #4060 #4057 add unboxed type support

- #4078, #4069 better code generated for cases like `match x with true -> ..| false -> ..`
- #4075, #4065 allow emoji in folder name
- #4074 allow defining a custom hostname for websocket
- #4064 fix shake_compile prefix & code param order
- #4053 use setImmediate for rebuilding to fix watch mode for some specific editors
- #4050 support pipe first poly variant
- #4049 Add support for custom underscored namespace
- #4018 #4020 #4028 #4031 #4037 #4048 #4042 #4041 #4040 improve compiler performance (optimize hash base data structure)
- #4039 upgrade refmt (various bug fixes)
- #4006 #4029 fix misleading error message
- #4025 #4026 fix corner case of code gen for empty object literal
- #4024 Playground: update repl.js and docs to support jsoo 3.5.1

## 7.0.1

- #4009 #4011 fix ocaml type checking bug (cherry pick)
- #4000 #4010 add react-starter theme
- #4005 fix windows installing issues

## 7.0.0

- #4003 not doing ast invariant checking, leave it to us (faster compilation)
- #4002 upgrade ocamldoc in 4.06, fix document build
- #4001 tweak error message
- #3999 #3987 fix ppx reactjs regression keep attributes on object fields
- #3998 #3996 less strict check for duplication in record labels
- #3996 #3969 fix refmt upgrade regressions
- #3982 bs.as for record support (language level feature)
- #3989 #3993 check renamed label is unique when using bs.as
- #3985 more precise runtime information passed down from upstream, fix a corner case of compiling ocaml class
- #3986 more precise runtime information passed down from upstream, add module alias comments in generated code
- #3991 #3980 fix a corner case when printing js object in statement position
- #3977 #3978 not relying on ref internal for the runtime
- #3976 add bs js playground reason support
- #3974 bsb tweak react hooks template README
- #3971 docs about format of lib/bs/.bsbuild
- #3967 #3973 fix recursive value initialization
- #3972 upgrade repl.js to 4.06.1
- #3890 Fix typo in readme
- #3898 #3900 #3911 #3913 #3915 #3925 #3928 #3929 #3932 #3936 #3934 OCaml record as Js object
- #3945 sync refmt 3.5.3
- #3916 add large ocaml object test
- #3935 #3931 partial fix for recursive values inside recursive modules
- #3917 #3926 speicalize all ref handling (prepare for js object interaction)
- #3918 use folders from esy ocaml for snapshots
- #3920 remove size field which is never used
- #3921 add queue_402 for testing recursive values
- #3912 add -bs-noloc opton for easy debugging (when using -dparsetree -dtypedtree)
- #3927 more tests for records
- #3901 internal rewrite float_of_bits to not rely on int64 representation
- #3903 Fix invalid json in hooks template
- #3905 Fix int64 test specification
- #3906 remove optimizations relying on int64 internals which is fragile when changing into bigint
- #3907 remove some transformations in printer
- #3893 specialize code generation for Sys.os_type better code for stdlib
- #3899 add missing optimization `let _ = xx in yy`

- #3878 add Belt.Option.forEach
- #3941 remove null_to_undefined since type is specialized
- #3943 fix SPA mode regression for hooks template
- #3944 #3956 add spacing to variant formatter
- #3950 fix react-hooks watcher websocket support
- #3954 alias result type using stdlib `result`
- #3955 update ¨CONTRIBUTING file
- #3959 internal, remove alias_table which is not needed
- #3960 add test case for functor coercion
- #3962 #3960 set env var `BSB_PROJECT_ROOT` for bsb
- #3965 better inline heruistics
- #3966 #3897 improve module coercion code gen in strict subtyping
  Deprecations
- #3889 remove deprecated light names in bs.deriving abstract

## 5.2.1 (2019/10/16)

Fixes

- #3884 (not using temp file when creating cmt for ocaml 4.06 only)
- #3877, #3881 Pattern match bug over list of integers
- #3875, #3879 regression introduced in 5.2.0 over pattern match optimization
- #3865 consistent name mangling when compiling modules into object
- #3852, #3870 for module with all module aliases and no interface file, always make it pure (treat it the same as namespace file)
- #3874 internal bug fix for ocaml cmpiler
  Codegen
- #3880 optimize pattern match for (string|int) option
- #3866 update react-hooks template
- #3843 mitigate windows anti-virius issue

## 5.2.0 (2019/09/23)

Features

- #3803 Add a -install flag for mono-repository improvement for parallel compilation
- #3790 #3802 #3799 #3792 #3836 Compile locla modules to objects and clean up
- #3836 initial support of esy
- #3780 Better code generation for if branches
- #3799 Improve code gen, better code equality for block
- #3778 #3770 Improve pattern match compilation against the outer-most staticfail
- #3764 better code gen for lazy evaluation
- #3822 allow `f##"Content-Type"` for uncommon property name
- #3810 annoate constructor names in pattern match compilation
  Fixes
- #3809 Escape question mark and ampersand to special chars
- #3805 #3796 Fix stale builds (module alias + namespace interaction)
- #3777 More friendly error message for invalid package name
- #3794 Calling genType from bsc instead from the patched ocaml compiler
- #3784 avoid trailing white space in case branch
- #3781 #3783 quote package path properly
- #3793 pass bs-version to genType
- #3674 installation from master works out of box
- #3823 fix reason language server

Docs

- #3795 Fix docs for stringifyAny
- #3788 Rename getUnasfe to getUnsafe
- #3830 fix isSortedExample
- #3829 fix mapWithDefault example
- #3828 fix getExn example
- #3827 fix partition example
- #3826 simplify truncateToLengthUnsafe example

## 5.1.0 (2019/08/15)

Features

- #3731 #3734 #3724 #3714 enable bsc for reason, so that for one file, `bsc hi.re` works
- #3671 add tool `bstracing` to visualize the building process
- #3730 #3738 Code gen: simplify `return undefined` as `return`
- #3713 support ppx with arguments (extended the schema)
- #3708 #3701 respect NODE_PATH when resolving node modules

Fixes

- #3699 Exit code from bsb in watch mode should be 0 instead of 2
- #3692, #3693 fix "cyclic dependencies error is swallowed"
- #3530, #3690 best effort support for shared library support
- #3683, #3678 Docs: fix example in Belt.Array
- #3657, #3669 Fix "For dependencies with namespace, the namespace file is always rebuilt"
- #3666, #3668 Fix "Ninja fails to compile on alpine linux "
- #3667, #3664 Fix "Warning number does not concat properly in some cases"
- #3662, #3515 Fix "bsb doesn't work with non-Unicode characters in the file path "
- #3653, #3519 Fix "[@react.component] vs .rei " in 4.06 branch
- #3655 Improve OCaml version file error
- #3652 Fix pnpm install (again)
- #3651 Fix "React PPX: Show warning 26 on unused props with default value"
  Internal
- #3711 not inlining self recursive functions
- #3740 enable backtrace by default for compiler in dev mode
- #3705 dump package path in .sourcedirs.json for troubleshooting
- #3698 better data format for .bsdeps
- #3680, #3684, #3677, #3675, #3672 better encoding for .bsbuild
- #3673 strip the binary by default on \*nix platform
- #3658 #3545 fix a bunch of edge cases with dev build
- #3643 Fix nasty out of bound error in string/bytes access

## 5.0.6

Fixes

- #3648, #3647, #3645 make sure bsppx.exe (used by editor tools/Merlin, Reason Language service) behaves consistently with bsc.exe

- #3643 fix a hidden out of bounds bug

- #3642 pass down ninja internal flags in combination of `bsb -make-world`, for example `bsb -make-world -- -d explain`

- #3641 fix pnpm install
- #3635 fix debug mode runtime crash
- #3633 fix bs dev dependency issue

## 5.0.5

Fixes

- #3615 pruning stale build artifacts in bsb, more robust to file changes (moving files around, renaming)

- #3609, #3914 Fix a fatal error in code generation
- #3598, #3595 Fix code generation when toplevel binding is partial match (edge case)
- #3588 Fix double quote -ppx argument on windows
- #3577 fix webpack file serving for direct route access
- #3574, #3566 Fix code generation when some built in module names are reused
- #3572, #3570 fix infinite loop in bsb -w (edge case)

- #3558, #3557 fix missing Js.MapperRt module (playground js)
- #3555, #3546 bs.deriving `accessors` add support for GADT
- #3553, #3549 Fix code generation for leading zero float (edge case)
- #3474 fix bad error message when bsconfig `dev` and `non-dev` overlap

- #3532 add missing docs for `Js.error` and `Js.trace`
- #3536 fixing nesting `|.` issue
- #3519 avoid `'a array` manifested in external generated signature which causes inconsistent signatures. The concrete issue is that when adding `.rei` file for `[@react.component]` it triggers not match type error
- #3534 correct commands for building vendor OCaml from ocaml.tar.gz
- `*` enforce the rule that a module has to contain `.ml` or `.re` file, interface only file is not supported

Features

- #3600 allow user to polyfill missing c stubs
- #3613, #3612 add a warning number 105 (on by default) for cases as below

```ocaml
external f : int -> int = "" [@@bs.val]
```

Such ffi declaration is fragile to refactoring when changing names of `f`

- #3587, #3571, #3568 simplify debugger mode, `debugger.chrome` is not needed to turn on debug mode

Internals

- #3556, #3554 allow test reason files directly
- #3594, #3586, #3580, #3575 upgrade ninja to a customized more performant internal version

## 5.0.4

Features

- #3523, #3516 Fusing react-jsx ppx as a flag

Docs

- #3522 add BS_VSCODE variable docs

Fixes

- #3540, #3482 remove unsupported items in the bsconfig.json schema
- #3539, #3474 fix bad error message when a repo has same name for two modules
- #3538, #3532 update docs
- #3536, #3537 fix nesting (|.) ppx issues
- #3519, #3535 fix external declarations that can not be generalized (uncovered by react jsx ppx v3)
- #3534 fix commands building from ocaml.tar.gz
- #3527, #3525, #3504, #3500 playground upgrade
- #3518, #3507, #3517 not emit warnings for dependencies
- #3515 fix on binding renameSync
- #3508 tweak error message for syntax error
- #3506 sync location and optional fixes for new jsx ppx
- #3501, #3505 fix inconsistency between Js.String and Js.String2
- #3502, #3503 fix pipe syntax on qualified opens
- #3492, #3499 fix code gen in external when apply bs.uncurry to (unit -> ..)
- #3496, #3495 fix 'bs.module isn't being resolved relatively correctly'

## 5.0.1

Features

- #3479 add a theme named react-hooks for the new ppx
- #3476 add bs.inline support for literals (int, string, bool) so that it gets a stronger guarantee for inlining
- #3473 upstream reason@3c6a9ca98
- #3470,#3466 ract jsx ppx

Fixes

- #3455 fix polymorphic comparison and equality for js date
- #3465 fix brutal console.clear
- #3468 add BS_VSCODE to disable -super-errors, which works better with vscode problem matcher

## 5.0.0

Features

- #3418 sync up with refmt 681c491ba760cdf3b49f92297c3dab1703682808
- #3395, #3417 better gentype support (gentype.import)
- #3412,#3416 Warning against usage of `string_of_float`
- #3414, #3415, #2893 allow usage of ` a |. M.(f a b)`
- #3403 first class bs.variadic support
- #3402 in watch mode, clear the screen upon rebuilding
- #3397 add ignored-dirs support in bsconfig.json
- #3377, #3376 add Linux prebuilt support for official release
- #3372 add Belt.Array.getIndexBy
- #3357, add `-bs-cmi-only` flag support to bsc so that no js emitted
- #3356, #3354 add gentypeconfig support in bsconfig.json
- #3352 fix minor mistake in Js.Dict.values doc
- #3329 Allow namespace in bsconfig.json to be customized
- #3334 Add Belt.Array.getBy
- #3204, #3208 add bs.deriving {light} support to allow short names
  Fixes
- #3413, #2893 deprecate Js.Array.join
- #3407, #3408 rebuild when ppx binary changes
- #3406, #3399 fix the interaction between external and relative paths
- #3393 deprecate Node.Fs.Watch.on in favor of Node.Fs.Watch.on\_
- #3315 depercate Js.Re.test, Js.Re.exec in favor of Js.Re.test*, Js.Re.exec*
- #3386, #3387 fix a codegen in with bs.raw
- #3386 make it more forgiving when interact with Js functions with arity 0
- #3381 remove golang as a dev dependency

* #3388 (breaking) Fix Js.Re.(splitbyReAtMost, splitByRe) binding

- #3332 remove `-bs-gen-tds` from docs in favor of gentype

## 4.0.17

Features

- #3229 true seperate compilation, incredible perf for incremental build

Fixes

- #3226, #3223 absolute path generated in recursive module path and `assert false`
- #3220 ppx-flags & scoped packages
- #3214 shadowing of js Promise constructor
- #3213 Allow build to be re-entrant to deal with yarn issues

## 4.0.10

Fixes

- upstream a bug fix for refmt
- Fix installation issues

## 4.0.8

Features

- Support OCaml 4.06 under a config

* #3146 support `#if 1` and `#if 0` in the built-in conditional compilation language
* #3159 Add Node.Buffer.concat
* #3181 sync in refmt 9fcbbca
* #3185 better performance in compilation, not reading runtime cmj files when not needed

Code gen

- #3134 Better arity infer when using first class module as function
- #3169 allow _ in bs.raw so that `fun%raw a _ -> ` works
- #3170, #3171 better code gen for bs.raw
- #2967 bs.variadic attribute (bs.splice still works)
  Fixes
- #3154 Fix binding `Js.Dict.get`
- #3132 Fix `int_of_string` semantics in an edge case
- #3142 Fix the combination of bs.as and unicode
- #3177 Webpack dev server mode
- #3180 clean up .gen.js/.gen.tsx for gentype

Docs

- #3133 Tweak Belt docs
- #3136 Fix typo in react and react-lite tempaltes
- #3161 improve perf of some functions in String module

## 4.0.7

Features

- #3072 Add List.filter/WithIndex and List.keep/WithIndex

Fix

- #3084,#3083 optimization triggers exception
- #3085 Wrong optimizer
- #3105 A corner case of optional encoding

Code gen

- - #3073, #3079, #3080 no arity tweaking. Function with unit as argument will have arity one
- #3078 better codegen for switch
- #3088 better codegen for if statement

## 4.0.6

### ReasonML synced 76b27

Fixes

- #3064 upgrade `webpack-cli` to fix broken `npm run webpack`
- #3054, #3502 fix some potential bugs in codegen
- #3047 handle null values correctly in the devtools custom formatter
- #3036 fix #3018 about dom in playground
- #3017 _important_ fix #3010 nodejs browser loader evaluated code twice

Features

- #3051,#3039 add List.keepWithIndex, reduceWithIndex
- #3046 add Js.Global.setTimeOutFloat and setIntervalFloat

Docs

- #3603 add documentation to Belt.Result
- #3031 fix typo in Belt.Option.flatMap example

## 4.0.4

Fixes

- #3001 fix regressios in refmt
- #2986 #2973 #2974 fix bsb websocket exit error
- #2983 #2988 determinsic behavior
  when NINJA_ANSI_FORCED=0 no color
  when NINJA_ANSI_FORCED=1 yes color

## 4.0.3

Fixes

- #2956 clean re.js for genFlow proj
- #2970 remove one obsolte error
- #2970 address one regression from refmt

## 4.0.2

Fixes

- #2963 fix ppx-flags quoting issue

Features

- #2951 sync up with reason
- #2964 customize ninja to make output less verbose
  Add NINJA_ANSI_FORCE env variable support so that third party tools running bsb can still preserve colors
- #2960 add tea theme support
- #2959 less verbose bsb output
- #2958 make `bsb -init` more forgiving

## 4.0.1

Fixes:

- #2949 fix optional regression
- #2946 fix react-lite theme on Linux

## 4.0.0

Fixes:

- #2832 fix compiler crash
- #2837 `toFixed`, `toExponential` too strict
- #2841 fix some inconsistency betweeen debug mode and release mode
- #2865 fix reload latency issue in react-lite theme
- #2864 fix parallel build random failure
- #2874 consistency check for global bsb and local bsb
- #2914 Fix bug on Windows where path has colon in command line arg
- #2919 fix return value of `Js.Date.toJSON`
- #2921 bs.deriving label -> labelGet, the `label` accessor is deprecated
- #2924 rename Js.Nullable.test -> Js.Nullable.isNullable
- #2923 fix ghost location in error message
- #2931 fix a codegen bug in optimization pattern match

Features:

- #2280 prettier output in debug mode (chrome custom formatter)
- #2823 add build-success hook

```
bsb -make-world -w -build-success 'your_script'
```

- #2856 provide websocket intergration with bsb
- #2858 add react-lite theme hot module reloading without webpack
- #2873 add Belt.Array.sliceToEnd
- #2825 Add Belt.Array.partition
- #2882, #2885, #2886, #2889, #2890,#2894, #2896, #2897,#2898, #2900
  #2901, #2905, #2907, #2908, #2909, #2912, #2913
  unbox optional and code optimization based on type kinds

- #2910 fix optional inline regression, better codegen for optional equality
- #2904 fix Js.Date.parse binding
- #2899 Add Dom.htmlFormElement and Dom.htmlInputElement types
- #2910 Improve package not found message

- #2916 optimize value based optional
- #2918 adapt polymoprhic comparison for the new optional representation
- #2917 remove trailing `return undefined`
- #2935 comments in codegen for `ref` generation
- #2863 optimize away unused blocks

## 3.1.4

Fixes:

- Put back the deprecated `Js.to_bool` and `Js.Boolean.*` for a little bit longer to avoid breackage in userland. These functions are all deprecated and don't do anythinng anymore, since we compile `bool` to `Js.boolean` directly.

## 3.1.0

Features:

- #2809, #2806 Sync up with latest reason
- #2805, allow `x |. Some `
- #2797, #2771 `-bs-g`
- #2793 more bindings to Js.Console
- #2621 add Belt.Result module
- #2760 add Belt.Array.unzip

Perf:

- #2808 better code gen for if then else
- #2804 make HashSet.String size smaller

Fixes:

- #2812, compatibility with Node 10
- #2789, fix Weak.length
- #2790, fix Belt.Set performance issue
- #2786, fix polymorphic compare on nullables
- #2781, improve location info on bs.deriving abstract
- #2776, lift the function limitation on bs.deriving abstract
- #2752, fix binding Buffer.toString

## 3.0.0

Features:

- #2740, #2726 Generalized safe/cleaner embedding raw function (https://rescript-lang.org/docs/manual/latest/embed-raw-javascript)

- #2687, #2665, #2663 bs.deriving abstract type, a powerful way for idiomatic JS and FFI

- #2696, #2742, #2706, #2711, compile OCaml boolean as JS boolean
  Breaking change to your code path relying on `Obj.magic` and `bs.raw`

- #2741 add Node.Buffer.fromStringWithEncoding
- #2737 add Js.Json.stringifyWithSpace
- #2728 add console.error and console.trace
- #2666 add List.sort to belt
- #2664 pipe syntax support tuple
  `obj |. method a b ` and `obj |. (m1 , m2)`

Perf:

- #2676 beter optimizations for tuple allocation

Docs:

- #2730, #2729, #2733, #2722, #2701, #2699 More docs to Belt

Fixes:

- #2657 more intuitive polymorphic comparison with objects
- #2686 playground unicode
- #2720, #2719, #2718, error message enhancment
- #2731 not inlining function contains `raw`
- #2672 Fix ci
- #2652 fix Buffer name mangling on Node
- #2642 weird indentation in generated code

## 2.2.3

Features:

- #2646,#2622 Adding Belt.Option
- #2342, #2624 (|.) pipe syntax for t first convention
- #2589 Expose Id.MakeComparable functor
- #2587 Added production ready settings for react theme
- - remove refmt syntax version 2

Performances:

- #2438 using concrete predicates for integer comparison
- #2584 better handling of if then else common sub expression

Fixes:

- #2303 defining or using a module named "Block" causes runtime errors
- #2616,#2612,#2554 better error message
- #2352 return value of assignment expression
- #2413 no break generated after return statement in some code branches
- #2633 [@bs.string] in FFI
- #2608 short-circuiting of && fails due to extraction of variable
- #2559 fix Bytes.create semantics
- #2638, bsb -w on windows freez
- #2448, js_int.ml not installed on windows

## 2.2.2

Features:

- Upgrade with latest reason syntax (native unucurry support etc)
- #2531 add missing functions in Js.Nullable fromoption/toOption
- #2527 Belt.List.shuffle

Fixes:

- #2503 bs.string and bs.obj interaction
- #2549, #2548, $2548, #2542, #2541 improve error message

## 2.2.0

Features:

- A beta release for the new stdlib called Belt
- #2436, #2381, #2377, #2353 bs.deriving abstract support

Performances:

- #2452 specialized comparison with Js.null, Js.undefined, Js.boolean
- #2412, provide specialized primitives for comparison with null/undefined
- #2361, better optimization for temporary tuple
  Fixes:
- #2451 better error message when arity mismatch for reason syntax
- #2399, turn partial application warnings to error in react template
- #2465 build on FreeBSD
- #2450 ignore bsb.lock
- #2356 ship build-schema.json
- #2489, #2464 capitalize names in combination of '/'
- #2459 subdirs:true by default for templates
- #2428, fix trailing space on react-jsx
- #2401, stop tab-aligning imports for smaller diff
- #2383, drop bs.deriving attribute after post-processing

## 2.1.0

Features:

- #2282, #2280,#2272,#2271,#2270,#2262,#2260,#2255,#2253
  Automatically derive js converter between ocaml and Js values
- #2238, #2225, #2221
  Make the compiler relocatable
  prebuilt compiler (this release for Mac/Win)
- #2276 update reason syntax@d0d18
- #2229 improve error message with regard to `@bs`
- #2266, add Js_global.(encode|decode)URI(Component) bindings
- #2220 make watcher mode in linux accept ninja progress animation
- #2163 better hints for binding module name
- #2187, #2186, #2179 add two warning numbers 101, 102 for polymorhic comparisons
  and unused bs attributes

Performance:

- #2269 type specialized comparison also applied to nullable polymorphic variants
- #2208 type specialized comparison allpied to nullable variants
- #2213 refine caml_obj_dump into caml_array_dup better array initialization code

Fixes:

- #2316 Pattern match with exception case and a single catch-all pattern is optimized incorrectly
- #2318 no absname in Match_failure
- #2250 #1285, fix code gen for object oriented code
- #2278, #2274 fix fatal errors regression and syntactice fatal errors(-werror A) don't stop building
- #2259 fix fatal errors don't stop generating cmj file
- #1972 bsb -init does not rely on `npm link` on \*nix platform
- #2207 nop rebuild to work around yarn bug
- #2226 kill bsb -w when stdin is closed
- #2184 bsb should exclude -I empty dirs

## 2.0

Features

- update reason3 syntax

## 1.10.3

Features:

- #2112, introduced a key `suffix`, so that user can
  choose `suffix : ".bs.js"`
- #2138, in combination of `.bs.js` suffix and `in-source` build,
  bsb is able to remove stale build artifact
- #2091 bsc xx.cmi will print xx.mli so users can generate
  mli in the beginning. `bsc -output-re xx.cmi` will print
  it in reason syntax
- #2096, clorized ninja build output

- #2120 better error message in the location of `{json||json}`
- #2123 avoid namespace leaking in types
- #2130 make Sys module not break React Native bindings
- #2159, #2165 enhance user expereince of bsb (less verbose, status bar when failed)
- #2134, allow people to make customized playground via a plugin

Fixes:

- #2157, fix unnecessary rebuilding when adding files with namespace option on
- #2145, fix bsb unnecessary rebuild issues
- #2150, fix an edge case of comparison between cyclic value
- #2140 tweak invalid npm package error message
- #2080, #2094 bsb -w can detect multiple processes running
  so no race condition
- #2097 default warning with "-30"
- #2101 fix bs.splice error message
- #2102 fix reason printer bug
- #2089 more logs when ocaml fails to build
- e16cbd03c64eb2b7e99570abdc29f5799778835f fix a binding of `Js.Re.matches`

## 1.9.3

Features:

- #2049, add a dedicated `warning` field, so that it is easy to override, enable `warn-error` and customize it.
- React JSX PPX V3 is in. This allows a custom component's `children` to be of any type. When the child's a single non-JSX item, it's not wrapped in an array anymore.
- #2016, add a flag `bsb -v` to tell the version number
- 64ea144746f998955f69d8eb4ec2b0179ce2d5b4 add Js.Typed_array subarray API
- #2030, error message: better unbound value message
- #2041, add a flag `bsb -verbose`, by default it is less noisy
- #2047, make bsb error message more professional
- #2048, turn `-bs-super-error` by default for reason files

Deprecations:

- React JSX PPX V1 has been removed, and the bsconfig `"react-jsx": true` is removed too (use `2` or `3`). JSX only accept a version number
- #1666, remove deprecated API

Fixes:

- #2029, error message: fix display line and column
- #2033, #2040 error message: trim output when too many lines
- #2042, better react error message
- #2053, better error message when file name is non-existent
- #2066, add a missing escaped char ':'
- #2063, add Js.Float for windows, remove deprecated Js.Float API
- #2070, fix -warn-error interaction with -bs-super-error

## 1.9.2

Fixes:

- #1943, Wrong name mangling for properties "\_50"
- #1029, tree shaking in playground
- #1946, Fix invalid JS output
- #1965, more JS modules exported in playground
- #1559, add a comment when no js output produced
- #1989, fix a bug for exit inlining
- #2002, make default exports work with transpiled babel ES6 import
- A bunch of improvements for better error message by Cheng Lou and Cristiano Calcagno

Features:

- #1944, bspack support -main-export
- #1990, better optimizations for pattern match compilation
- #1991, Accept `bs.deriving accessors` applies to `bs.config` as well for single field
- #2001, improve global module compilation
- #2006, `"subdirs": true` will traverse the directory recursively
- #1964, for `Capital_file.ml` generate `Capital_file.js` instead of `capital_file.js`

Deprecations:

- #1968, remove support for Google module system

## 1.9.1 (Recovery 1.9.0)

Fixes

- #1933 hyphen directory name fixes

## 1.9.0

Features:

- Namespace support in the build system
- #1839, #1653, allow in-source build in package-specs , allow single package-spec element in package-specs
- #1802 introduce [@bs.unwrap] for polymorphic variant as external argument
- Improve error message via -bs-super-errors
- Reason syntax error message for .re/.rei files
- #1908 two APIs for Js.Re
- #1905, #1906, simplify the workflow of handling null or undefined (via nullable)
  Optimizations:
- #1918, better code gen for pattern match
- #1865, Add Js.logN

Fixes

- #1925, fix missing js modules in playground
- #1893, fix Js.Array.append (Js.Vector.append)

## 1.8.2

Features:

- #1798 make `default` the same semantics as ES6 exports
- #1785 upgrade playground
- #1758 generator support
- #1826 add `bsb -where` support so that bsb.exe can be located and cached in a more robust way

Optimizations:

- #1796, #1793 improve submodule arity inference
- #1810 don't rebuild ninja if binary already exists in bin folder

Fixes:

- #1811 add relative PPX paths to .merlin correctly
- #1822, fix an optimization bug

Internal:

- add a tool cmjdump.exe

## 1.8.1

Fixes:

- #1762, discard effectful unused code
- #1760, div by zero effect analysis

## 1.8.0

Fixes:

- #1573, don't include `-bs` flags in `.merlin`
- #1716, fix wrong optimization of recursive values
- #1728, bad inlining
- #1409, make sure when optional is None, bs.obj will not introduce such label
- #1737, same as #1409
- #1746, a corner case when mixing recursive value and functions, should make markup.ml work
- #1749, fix nested if, code block discarded issue

## 1.7.5

Fixes:

- #1676, `bsb -w` will always build regardless of filetype when fs.watch doesn't send a filename
- #1655, fix #1653 Js.Promise.all[n] interfaces
- #1658, fix typeof = "null" issue
- #1656, bs.get/set/get_index/set_index respects bs.ignore
- #1654, `bsb -init` fails if package or current dir has space (parent dir can have spaces)
- #1678, bs.get{null;undefined} in object type
- #1692, fix invalid js syntax output
- #1701, fix tailcall handling interaction with exception handler
- #1666, fix misuse of GADT API

Features:

- #1648, exposed `bsc` in the npm environment
- #1647, special handling `bsb -init .` to reuse current directory
- #1667, fix an optimization bug
- #1698, fix exit code incorrectly aggregated issue
- #1666, add Js.Json.classify and Js.Types.classify
- #1705, add DOM storage API
- #1672, sync up with new reason
- #1696, provide reason-react template

## 1.7.4

Internal:

- #1583, add -U -D support for bspack

Features:

- #1630, add modules Option, Result, List, and Vector into Js namespace, update docs

- #1613, allow bs.scope with bs.send/bs.send.pipe/bs.set/bs.get/bs.set_index/bs.get_index
- #1604, add the functions `entries`, `values`, `fromList`, `fromArray` and `map` to `Js.Dict`

- #1632, bsb themes support

Fixes:

- #1581, more error checking
- #1633, fix missing installations
- #1581, more error checking %identity

## 1.7.3

Fixes:

- #1556, fix duplicated requires of runtime (report by Cheng Lou)
- #1568, internal compiler error

Features:

- #1564: scoped values in FF, see `bs.scope` in the Manual

## 1.3.2

Features:

- Significantly improve bsb experience

## 1.2.1 + dev

Features:

- #861, add `-bs-assume-no-mli` and `-bs-no-implicit-include` for deterministic build
- #851, add -bs-D -bs-list-conditionals flags
- add `-bs-syntax-only`
- #854, add `-bs-binary-ast`

## 1.1.2

Fixes:

- #831, bug fix with opam issues

Features:

- Provide `bspp.exe` for the official compiler

## 1.1.1

Features:

- #822, add `bsdep.exe`
- #820, conditional compilation support
- #793, relax syntactic restrictions for all extension point so that `bs.obj`, `obj`, `bs.raw`, `raw`, etc. will both work. Note that all attributes will still be qualified
- #793, support `bs.splice` in `bs.new`
- #798, complete `bs.splice` support and documentation

## 1.0.3

Incompatible changes (to better support Windows):

- `bsc`, `bspack` and `bsppx` are renamed into `bsc.exe`, `bspack.exe` and `bsppx.exe`
- No symlink from .bin any more.

  **Old symlinks**:

  ```sh
  tmp > ls -al node_modules/.bin/
  total 96
  drwxr-xr-x  14 hzhang295  staff  476 Sep 20 17:26 .
  drwxr-xr-x   4 hzhang295  staff  136 Sep 20 17:27 ..
  lrwxr-xr-x   1 hzhang295  staff   22 Sep 20 17:26 bsc -> ../bs-platform/bin/bsc
  lrwxr-xr-x   1 hzhang295  staff   25 Sep 20 17:26 bspack -> ../bs-platform/bin/bspack
  lrwxr-xr-x   1 hzhang295  staff   24 Sep 20 17:26 bsppx -> ../bs-platform/bin/bsppx
  ```

  Now these symlinks are removed. You have to refer to `bs-platform/bin/bsc.exe`.

Features:

- #787, add an option `-bs-no-warn-unused-bs-attribute`

## 1.0.2

Fixes:

- #743, Fix Bytes.blit when `src == dst`

Features:

- #783, by default, `bsc.exe` will warn when it detect some OCaml data types are passed from/to external FFI
- #784, add an option `-bs-eval`

## 1.0.1

Fixes:

- #718, Enforce `#=` always return unit for better error messages

Features:

- FFI
  - #694, support fields and mutable fields in JS object creation and private method
  - #686, introduce phantom arguments (`bs.ignore`) for ad-hoc polymorphism

## 1.0.0

Initial release
