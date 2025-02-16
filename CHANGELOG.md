# Changelog

> **Tags:**
>
> - :boom: [Breaking Change]
> - :eyeglasses: [Spec Compliance]
> - :rocket: [New Feature]
> - :bug: [Bug Fix]
> - :memo: [Documentation]
> - :house: [Internal]
> - :nail_care: [Polish]

# 12.0.0-alpha.9 (Unreleased)

#### :nail_care: Polish

- Allow single newline in JSX. https://github.com/rescript-lang/rescript/pull/7269
- Editor: Always complete from Core first. Use actual native regex syntax in code snippets for regexps. https://github.com/rescript-lang/rescript/pull/7295

#### :bug: Bug fix

- Fix async context checking for module await. https://github.com/rescript-lang/rescript/pull/7271
- Fix `%external` extension. https://github.com/rescript-lang/rescript/pull/7272
- Fix issue with type environment for unified ops. https://github.com/rescript-lang/rescript/pull/7277
- Fix completion for application with tagged template. https://github.com/rescript-lang/rescript/pull/7278
- Fix error message for arity in the presence of optional arguments. https://github.com/rescript-lang/rescript/pull/7284
- Fix issue in functors with more than one argument (which are curried): emit nested function always. https://github.com/rescript-lang/rescript/pull/7273
- Fix dot completion issue with React primitives.  https://github.com/rescript-lang/rescript/pull/7292

#### :house: Internal

- Remove ignore in res_scanner.ml . https://github.com/rescript-lang/rescript/pull/7280
- Use the new stdlib modules in the analysis tests. https://github.com/rescript-lang/rescript/pull/7295
- Build with OCaml 5.3.0. https://github.com/rescript-lang/rescript-compiler/pull/7294

# 12.0.0-alpha.8

#### :bug: Bug fix

- Editor: Fix issue where pipe completions would not trigger with generic type arguments. https://github.com/rescript-lang/rescript/pull/7231
- Fix leftover `assert false` in code for `null != undefined`. https://github.com/rescript-lang/rescript/pull/7232
- Editor: Fix issue where completions would not show up inside of object bodies. https://github.com/rescript-lang/rescript/pull/7230
- Fix issue with pattern matching empty list which interferes with boolean optimisations. https://github.com/rescript-lang/rescript/pull/7237
- Fix Cannot combine `@react.component` and `@directive`. https://github.com/rescript-lang/rescript/pull/7260
- Fix issue where attributes on an application were not preserved by the AST conversion for ppx. https://github.com/rescript-lang/rescript/pull/7262

#### :house: Internal

- AST cleanup: Prepare for ast async cleanup: Refactor code for `@res.async` payload handling and clean up handling of type and term parameters, so that now each `=>` in a function definition corresponds to a function. https://github.com/rescript-lang/rescript/pull/7223
- AST: always put type parameters first in function definitions. https://github.com/rescript-lang/rescript/pull/7233
- AST cleanup: Remove `@res.async` attribute from the internal representation, and add a flag to untyped and typed ASTs instead. https://github.com/rescript-lang/rescript/pull/7234
- AST cleanup: Remove unused `expression_desc.Pexp_new`, `expression_desc.Pexp_setinstvar`, `expression_desc.Pexp_override`, `expression_desc.Pexp_poly`, `exp_extra.Texp_poly`, `expression_desc.Texp_new`, `expression_desc.Texp_setinstvar`, `expression_desc.Texp_override` & `expression_desc.Texp_instvar` from AST. https://github.com/rescript-lang/rescript/pull/7239
- AST cleanup: Remove `@res.partial` attribute from the internal representation, and add a flag to untyped and typed ASTs instead. https://github.com/rescript-lang/rescript/pull/7238 https://github.com/rescript-lang/rescript/pull/7240
- AST cleanup: Remove unused `structure_item_desc.Pstr_class`, `signature_item_desc.Psig_class`, `structure_item_desc.Pstr_class_type`, `signature_item_desc.Psig_class_type`, `structure_item_desc.Tstr_class`, `structure_item_desc.Tstr_class_type`, `signature_item_desc.Tsig_class`, `signature_item_desc.Tsig_class_type` from AST. https://github.com/rescript-lang/rescript/pull/7242
- AST cleanup: remove `|.` and rename `|.` to `->` in the internal representation for the pipe operator. https://github.com/rescript-lang/rescript/pull/7244
- AST cleanup: represent concatenation (`++`) and (dis)equality operators (`==`, `===`, `!=`, `!==`) just like in the syntax. https://github.com/rescript-lang/rescript/pull/7248
- AST cleanup: use inline record for `Ptyp_arrow`. https://github.com/rescript-lang/rescript/pull/7250
- Playground: Bundle stdlib runtime so that the playground can execute functions from Core/Belt/Js. https://github.com/rescript-lang/rescript/pull/7255
- AST cleanup: Remove `res.namedArgLoc` attribute and store the location information directly into the label. https://github.com/rescript-lang/rescript/pull/7247

#### :nail_care: Polish

- Rewatch 1.0.10. https://github.com/rescript-lang/rescript/pull/7259

# 12.0.0-alpha.7

#### :bug: Bug fix

- Editor: Fix issue where completions would stop working in some scenarios with inline records. https://github.com/rescript-lang/rescript/pull/7227

#### :nail_care: Polish

- Add all standard CSS properties to `JsxDOMStyle`. https://github.com/rescript-lang/rescript/pull/7205

#### :house: Internal

- AST cleanup: use inline record for Pexp_fun. https://github.com/rescript-lang/rescript/pull/7213
- Add support for "dot completion everywhere" (ported from https://github.com/rescript-lang/rescript-vscode/pull/1054). https://github.com/rescript-lang/rescript/pull/7226
- Add assertions to stdlib docstring examples. Extract examples into Mocha tests, compile and run the tests in CI. https://github.com/rescript-lang/rescript/pull/7219

# 12.0.0-alpha.6

#### :rocket: New Feature

- Add `Option.all` & `Result.all` helpers. https://github.com/rescript-lang/rescript/pull/7181
- Add `@react.componentWithProps` for React component functions taking a props record instead of labeled arguments. https://github.com/rescript-lang/rescript/pull/7203

#### :bug: Bug fix

- Fix exponential notation syntax. https://github.com/rescript-lang/rescript/pull/7174
- Fix bug where a ref assignment is moved ouside a conditional. https://github.com/rescript-lang/rescript/pull/7176
- Fix nullable to opt conversion. https://github.com/rescript-lang/rescript/pull/7193
- Raise error when defining external React components with `@react.componentWithProps`. https://github.com/rescript-lang/rescript/pull/7217
- Fix formatter handling of wildcard in pattern matching records with no fields specified. https://github.com/rescript-lang/rescript/pull/7224

#### :house: Internal

- Use latest compiler for tests. https://github.com/rescript-lang/rescript/pull/7186
- Added infra to modernise AST: theres' Parsetree, Parsetree0 (legacy), and conversion functions to keep compatibility with PPX. https://github.com/rescript-lang/rescript/pull/7185
- AST cleanup: remove exp object and exp unreachable. https://github.com/rescript-lang/rescript/pull/7189
- AST cleanup: explicit representation for optional record fields in types. https://github.com/rescript-lang/rescript/pull/7190 https://github.com/rescript-lang/rescript/pull/7191
- AST cleanup: first-class expression and patterns for records with optional fields. https://github.com/rescript-lang/rescript/pull/7192
- AST cleanup: Represent the arity of uncurried function definitions directly in the AST. https://github.com/rescript-lang/rescript/pull/7197
- AST cleanup: Remove Pexp_function from the AST. https://github.com/rescript-lang/rescript/pull/7198
- Remove unused code from Location and Rescript_cpp modules. https://github.com/rescript-lang/rescript/pull/7150
- Build with OCaml 5.2.1. https://github.com/rescript-lang/rescript-compiler/pull/7201
- AST cleanup: Remove `Function$` entirely for function definitions. https://github.com/rescript-lang/rescript/pull/7200
- AST cleanup: store arity in function type. https://github.com/rescript-lang/rescript/pull/7195
- AST cleanup: remove explicit uses of `function$` in preparation for removing the type entirely. https://github.com/rescript-lang/rescript/pull/7206
- AST cleanup: remove `function$` entirely. https://github.com/rescript-lang/rescript/pull/7208

# 12.0.0-alpha.5

#### :rocket: New Feature

- Introduce "Unified operators" for arithmetic operators (`+`, `-`, `*`, `/`, `mod`). https://github.com/rescript-lang/rescript-compiler/pull/7057
- Add remainder (`%`, aka modulus) operator. https://github.com/rescript-lang/rescript-compiler/pull/7152

#### :bug: Bug fix

- Fix and clean up boolean and/or optimizations. https://github.com/rescript-lang/rescript-compiler/pull/7134 https://github.com/rescript-lang/rescript-compiler/pull/7151
- Fix identifiers with name `arguments` and `eval` to be mangled. https://github.com/rescript-lang/rescript/pull/7163

#### :nail_care: Polish

- Improve code generation for pattern matching of untagged variants. https://github.com/rescript-lang/rescript-compiler/pull/7128
- Improve negation handling in combination with and/or to simplify generated code (especially coming out of pattern matching). https://github.com/rescript-lang/rescript-compiler/pull/7138
- Optimize JavaScript code generation by using `x == null` checks and improving type-based optimizations for string/number literals. https://github.com/rescript-lang/rescript-compiler/pull/7141
- Improve pattern matching on optional fields. https://github.com/rescript-lang/rescript-compiler/pull/7143 https://github.com/rescript-lang/rescript-compiler/pull/7144
- Optimize compilation of switch statements for untagged variants when there are no literal cases. https://github.com/rescript-lang/rescript-compiler/pull/7135
- Further improve boolean optimizations. https://github.com/rescript-lang/rescript-compiler/pull/7149
- Simplify code generated for conditionals. https://github.com/rescript-lang/rescript-compiler/pull/7151

#### :house: Internal

- Move rescript-editor-analysis and rescript-tools into compiler repo. https://github.com/rescript-lang/rescript-compiler/pull/7000

# 12.0.0-alpha.4

#### :boom: Breaking Change

- OCaml compatibility in the stdlib and primitives are dropped/deprecated. https://github.com/rescript-lang/rescript-compiler/pull/6984
- Remove JSX v3. https://github.com/rescript-lang/rescript-compiler/pull/7072
- Remove js_cast.res. https://github.com/rescript-lang/rescript-compiler/pull/7075

#### :rocket: New Feature

- Use FORCE_COLOR environmental variable to force colorized output. https://github.com/rescript-lang/rescript-compiler/pull/7033
- Allow spreads of variants in patterns (`| ...someVariant as v => `) when the variant spread is a subtype of the variant matched on. https://github.com/rescript-lang/rescript-compiler/pull/6721
- Fix the issue where dynamic imports are not working for function-defined externals. https://github.com/rescript-lang/rescript-compiler/pull/7060
- Allow pattern matching on dicts. `switch someDict { | dict{"one": 1} => Js.log("one is one") }`. https://github.com/rescript-lang/rescript-compiler/pull/7059
- "ReScript Core" standard library is now included in the `rescript` npm package. https://github.com/rescript-lang/rescript-compiler/pull/7108 https://github.com/rescript-lang/rescript-compiler/pull/7116
- Handle absolute filepaths in gentype. https://github.com/rescript-lang/rescript-compiler/pull/7104

#### :bug: Bug fix

- Fix tuple coercion. https://github.com/rescript-lang/rescript-compiler/pull/7024
- Fix attribute printing. https://github.com/rescript-lang/rescript-compiler/pull/7025
- Fix "rescript format" with many files. https://github.com/rescript-lang/rescript-compiler/pull/7081
- Fix bigint max, min. https://github.com/rescript-lang/rescript-compiler/pull/7088
- Fix parsing issue with nested variant pattern type spreads. https://github.com/rescript-lang/rescript-compiler/pull/7080
- Fix JSX settings inheritance: only 'version' propagates to dependencies, preserving their 'mode' and 'module'. https://github.com/rescript-lang/rescript-compiler/pull/7094
- Fix variant cast to int. https://github.com/rescript-lang/rescript-compiler/pull/7058
- Fix comments formatted away in function without arguments. https://github.com/rescript-lang/rescript-compiler/pull/7095
- Fix genType JSX component compilation. https://github.com/rescript-lang/rescript-compiler/pull/7107

#### :nail_care: Polish

- Add some context to error message for unused variables. https://github.com/rescript-lang/rescript-compiler/pull/7050
- Improve error message when passing `children` prop to a component that doesn't accept it. https://github.com/rescript-lang/rescript-compiler/pull/7044
- Improve error messages for pattern matching on option vs non-option, and vice versa. https://github.com/rescript-lang/rescript-compiler/pull/7035
- Improve bigint literal comparison. https://github.com/rescript-lang/rescript-compiler/pull/7029
- Improve output of `@variadic` bindings. https://github.com/rescript-lang/rescript-compiler/pull/7030
- Improve error messages around JSX components. https://github.com/rescript-lang/rescript-compiler/pull/7038
- Improve output of record copying. https://github.com/rescript-lang/rescript-compiler/pull/7043
- Provide additional context in error message when `unit` is expected. https://github.com/rescript-lang/rescript-compiler/pull/7045
- Improve error message when passing an object where a record is expected. https://github.com/rescript-lang/rescript-compiler/pull/7101

#### :house: Internal

- Remove uncurried flag from bsb. https://github.com/rescript-lang/rescript-compiler/pull/7049
- Build runtime/stdlib files with rescript/bsb instead of ninja.js. https://github.com/rescript-lang/rescript-compiler/pull/7063
- Build tests with bsb and move them out of jscomp. https://github.com/rescript-lang/rescript-compiler/pull/7068
- Run `build_tests` on Windows. https://github.com/rescript-lang/rescript-compiler/pull/7065
- Rename folder "jscomp" to "compiler". https://github.com/rescript-lang/rescript-compiler/pull/7086
- Disable -bs-cross-module-opt for tests. https://github.com/rescript-lang/rescript-compiler/pull/7071
- Move `ounit_tests` into the `tests` folder. https://github.com/rescript-lang/rescript-compiler/pull/7096
- Move `syntax_tests` into the `tests` folder. https://github.com/rescript-lang/rescript-compiler/pull/7090 https://github.com/rescript-lang/rescript-compiler/pull/7097
- Capitalize runtime filenames. https://github.com/rescript-lang/rescript-compiler/pull/7110
- Build mocha tests as esmodule / .mjs. https://github.com/rescript-lang/rescript-compiler/pull/7115
- Use dict instead of Dict.t everywhere. https://github.com/rescript-lang/rescript-compiler/pull/7136

# 12.0.0-alpha.3

#### :bug: Bug fix

- Revert "Throws an instance of JavaScript's `new Error()` and adds the extension payload for `cause` option" (https://github.com/rescript-lang/rescript-compiler/pull/6611). https://github.com/rescript-lang/rescript-compiler/pull/7016
- Fix dict literals error. https://github.com/rescript-lang/rescript-compiler/pull/7019

# 12.0.0-alpha.2

#### :rocket: New Feature

- Allow coercing polyvariants to variants when we can guarantee that the runtime representation matches. https://github.com/rescript-lang/rescript-compiler/pull/6981
- Add new dict literal syntax (`dict{"foo": "bar"}`). https://github.com/rescript-lang/rescript-compiler/pull/6774
- Optimize usage of the new dict literal syntax to emit an actual JS object literal. https://github.com/rescript-lang/rescript-compiler/pull/6538

#### :bug: Bug Fix

- Fix issue where long layout break added a trailing comma in partial application `...`. https://github.com/rescript-lang/rescript-compiler/pull/6949
- Fix incorrect format of function under unary operator. https://github.com/rescript-lang/rescript-compiler/pull/6953
- Fix incorrect printing of module binding with signature. https://github.com/rescript-lang/rescript-compiler/pull/6963
- Fix incorrect printing of external with `@as` attribute and `_` placholder (fixed argument). https://github.com/rescript-lang/rescript-compiler/pull/6970
- Disallow spreading anything but regular variants inside of other variants. https://github.com/rescript-lang/rescript-compiler/pull/6980
- Fix comment removed when function signature has `type` keyword. https://github.com/rescript-lang/rescript-compiler/pull/6997
- Fix parse error on doc comment before "and" in type def. https://github.com/rescript-lang/rescript-compiler/pull/7001

#### :house: Internal

- Add dev container. https://github.com/rescript-lang/rescript-compiler/pull/6962
- Convert more tests to the node test runner. https://github.com/rescript-lang/rescript-compiler/pull/6956
- Remove attribute "internal.arity". https://github.com/rescript-lang/rescript-compiler/pull/7004
- Remove dead modules. https://github.com/rescript-lang/rescript-compiler/pull/7008

#### :nail_care: Polish

- Improve formatting in the generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6932
  - `}\ncatch{` -> `} catch {`
  - `for(let i = 0 ,i_finish = r.length; i < i_finish; ++i){` -> `for (let i = 0, i_finish = r.length; i < i_finish; ++i) {`
  - `while(true) {` -> `while (true) {`
  - Fixed tabulation for `switch case` bodies
  - Fixed tabulation for `throw new Error` bodies
  - Removed empty line at the end of `switch` statement
  - Removed empty `default` case from `switch` statement in the generated code
- Optimised the Type Extension runtime code and removed trailing `/1` from `RE_EXN_ID`. https://github.com/rescript-lang/rescript-compiler/pull/6958
- Compact output for anonymous functions. https://github.com/rescript-lang/rescript-compiler/pull/6945 https://github.com/rescript-lang/rescript-compiler/pull/7013
- Rewatch 1.0.9. https://github.com/rescript-lang/rescript-compiler/pull/7010

# 12.0.0-alpha.1

#### :rocket: New Feature

- Allow `@directive` on functions for emitting function level directive code (`let serverAction = @directive("'use server'") (~name) => {...}`). https://github.com/rescript-lang/rescript-compiler/pull/6756
- Add `rewatch` to the npm package as an alternative build tool. https://github.com/rescript-lang/rescript-compiler/pull/6762
- Throws an instance of JavaScript's `new Error()` and adds the extension payload for `cause` option. https://github.com/rescript-lang/rescript-compiler/pull/6611
- Allow free vars in types for type coercion `e :> t`. https://github.com/rescript-lang/rescript-compiler/pull/6828
- Allow `private` in with constraints. https://github.com/rescript-lang/rescript-compiler/pull/6843
- Add regex literals as syntax sugar for `@bs.re`. https://github.com/rescript-lang/rescript-compiler/pull/6776
- Improved mechanism to determine arity of externals, which is consistent however the type is written. https://github.com/rescript-lang/rescript-compiler/pull/6874 https://github.com/rescript-lang/rescript-compiler/pull/6881 https://github.com/rescript-lang/rescript-compiler/pull/6883
- Add `Js.globalThis` object binding. https://github.com/rescript-lang/rescript-compiler/pull/6909

#### :boom: Breaking Change

- Make `j` and `js` allowed names for tag functions. https://github.com/rescript-lang/rescript-compiler/pull/6817
- `lazy` syntax is no longer supported. If you're using it, use `Lazy` module or `React.lazy_` instead. https://github.com/rescript-lang/rescript-compiler/pull/6342
- Remove handling of attributes with `bs.` prefix (`@bs.as` -> `@as` etc.). https://github.com/rescript-lang/rescript-compiler/pull/6643
- Remove obsolete `@bs.open` feature. https://github.com/rescript-lang/rescript-compiler/pull/6629
- Drop Node.js version <18 support, due to it reaching End-of-Life. https://github.com/rescript-lang/rescript-compiler/pull/6429
- Remove deprecated -bs-super-errors option. https://github.com/rescript-lang/rescript-compiler/pull/6814
- Some global names and old keywords are no longer prefixed. https://github.com/rescript-lang/rescript-compiler/pull/6831
- Remove ml parsing tests and conversion from `.ml` to `.res` via format. https://github.com/rescript-lang/rescript-compiler/pull/6848
- Remove support for compiling `.ml` files, and general cleanup. https://github.com/rescript-lang/rescript-compiler/pull/6852
- Remove `rescript convert` subcommand. https://github.com/rescript-lang/rescript-compiler/pull/6860
- Remove support for `@bs.send.pipe`. This also removes all functions in `Js_typed_array` that rely on `@bs.send.pipe`. https://github.com/rescript-lang/rescript-compiler/pull/6858 https://github.com/rescript-lang/rescript-compiler/pull/6891
- Remove deprecated `Js.Vector` and `Js.List`. https://github.com/rescript-lang/rescript-compiler/pull/6900
- Remove support for `%time` extension. https://github.com/rescript-lang/rescript-compiler/pull/6924
- Remove `caml_external_polyfill` module and the related behavior. https://github.com/rescript-lang/rescript-compiler/pull/6925

#### :bug: Bug Fix

- Fix unhandled cases for exotic idents (allow to use exotic PascalCased identifiers for types). https://github.com/rescript-lang/rescript-compiler/pull/6777 https://github.com/rescript-lang/rescript-compiler/pull/6779 https://github.com/rescript-lang/rescript-compiler/pull/6897
- Fix unused attribute check for `@as`. https://github.com/rescript-lang/rescript-compiler/pull/6795
- Reactivate unused attribute check for `@int`. https://github.com/rescript-lang/rescript-compiler/pull/6802
- Fix issue where using partial application `...` can generate code that uses `Curry` at runtime. https://github.com/rescript-lang/rescript-compiler/pull/6872
- Avoid generation of `Curry` with reverse application `|>`. https://github.com/rescript-lang/rescript-compiler/pull/6876
- Fix issue where the internal ppx for pipe `->` would not use uncurried application in uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6878

#### :house: Internal

- Build with OCaml 5.2.0. https://github.com/rescript-lang/rescript-compiler/pull/6797
- Convert OCaml codebase to snake case style. https://github.com/rescript-lang/rescript-compiler/pull/6702
- Fix `-nostdlib` internal compiler option. https://github.com/rescript-lang/rescript-compiler/pull/6824
- Remove a number of ast nodes never populated by the .res parser, and resulting dead code. https://github.com/rescript-lang/rescript-compiler/pull/6830
- Remove coercion with 2 types from internal representation. Coercion `e : t1 :> t2` was only supported in `.ml` syntax and never by the `.res` parser. https://github.com/rescript-lang/rescript-compiler/pull/6829
- Convert `caml_format` and `js_math` to `.res`. https://github.com/rescript-lang/rescript-compiler/pull/6834
- Convert `js.ml` files to `.res`. https://github.com/rescript-lang/rescript-compiler/pull/6835
- Remove old `.ml` tests. https://github.com/rescript-lang/rescript-compiler/pull/6847
- Make compiler libs ready for uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6861
- Make tests ready for uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6862
- Make gentype tests uncurried. https://github.com/rescript-lang/rescript-compiler/pull/6866
- Remove `@@uncurried.swap`, which was used for internal tests. https://github.com/rescript-lang/rescript-compiler/pull/6875
- Build the compiler libraries/tests in uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6864
- Ignore `-uncurried` command-line flag. https://github.com/rescript-lang/rescript-compiler/pull/6885
- Cleanup: remove tracking of uncurried state in parser/printer. https://github.com/rescript-lang/rescript-compiler/pull/6888
- Remove `%opaque` primitive. https://github.com/rescript-lang/rescript-compiler/pull/6892
- Reunify JsxC/JsxU -> Jsx etc. https://github.com/rescript-lang/rescript-compiler/pull/6895
- Remove the transformation of `foo(1,2)` into `Js.Internal.opaqueFullApply(Internal.opaque(f), 1, 2)`, and change the back-end to treat all applications as uncurried. https://github.com/rescript-lang/rescript-compiler/pull/6893
- Remove `@uncurry` from ReScript sources (others, tests). https://github.com/rescript-lang/rescript-compiler/pull/6938
- Remove leftover uncurried handling. https://github.com/rescript-lang/rescript-compiler/pull/6939 https://github.com/rescript-lang/rescript-compiler/pull/6940
- Start converting tests from mocha to the node test runner. https://github.com/rescript-lang/rescript-compiler/pull/6956

#### :nail_care: Polish

- Make the `--help` arg be prioritized in the CLI, so correctly prints help message and skip other commands. https://github.com/rescript-lang/rescript-compiler/pull/6667
- Remove redundant space for empty return in generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6745
- Remove redundant space for export in generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6560
- Remove redundant space after continue in generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6743
- Remove empty export blocks in generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6744
- Fix indent for returned/thrown/wrapped in parentheses objects in generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6746
- Fix indent in generated js code. https://github.com/rescript-lang/rescript-compiler/pull/6747
- In generated code, use `let` instead of `var`. https://github.com/rescript-lang/rescript-compiler/pull/6102
- Turn off transformation for closures inside loops when capturing loop variables, now that `let` is emitted instead of `var`. https://github.com/rescript-lang/rescript-compiler/pull/6480
- Improve unused attribute warning message. https://github.com/rescript-lang/rescript-compiler/pull/6787
- Remove internal option `use-stdlib` from build schema. https://github.com/rescript-lang/rescript-compiler/pull/6778
- Fix `Js.Types.JSBigInt` payload to use native `bigint` type. https://github.com/rescript-lang/rescript-compiler/pull/6911
- Deprecate `%external` extension, which has never been officially introduced. https://github.com/rescript-lang/rescript-compiler/pull/6906
- Deprecate `xxxU` functions in Belt. https://github.com/rescript-lang/rescript-compiler/pull/6941
- Improve error messages for function arity errors. https://github.com/rescript-lang/rescript-compiler/pull/6990
- Add missing HTML attribute capture to JsxDOM.res. https://github.com/rescript-lang/rescript-compiler/pull/7006

# 11.1.3

#### :bug: Bug Fix

- Fix tag function location on compiler error. https://github.com/rescript-lang/rescript-compiler/pull/6816
- Fix Deno compatibility issues on Windows. https://github.com/rescript-lang/rescript-compiler/pull/6850
- Fix issue with infinite loops with type errors on recursive types. https://github.com/rescript-lang/rescript-compiler/pull/6867
- Ignore `@uncurry` attribute in uncurried mode, to avoid generating calls to `Curry` at runtime. https://github.com/rescript-lang/rescript-compiler/pull/6869
- Avoid generating calls to Curry when adjusting arity of uncurried functions. https://github.com/rescript-lang/rescript-compiler/pull/6870
- Fix build after calling without `-warn-error`, see https://github.com/rescript-lang/rescript-compiler/issues/6868 for more details. https://github.com/rescript-lang/rescript-compiler/pull/6877
- Fix issue with uninitialized `_param` in recursive functions with unit argument. https://github.com/rescript-lang/rescript-compiler/pull/6907

# 11.1.3-rc.1

#### :bug: Bug Fix

- Omit standard library dir from load path if -nostdlib is set. https://github.com/rescript-lang/rescript-compiler/pull/6833

# 11.1.2

#### :bug: Bug Fix

- Fix issue where capitalised type variables were only allowed in certain positions. https://github.com/rescript-lang/rescript-compiler/pull/6820

# 11.1.2-rc.1

#### :rocket: New Feature

- Support Windows 11 ARM (using the x64 binaries in emulation). https://github.com/rescript-lang/rescript-compiler/pull/6813

#### :bug: Bug Fix

- Fix location of let bindings with attributes. https://github.com/rescript-lang/rescript-compiler/pull/6791
- PPX v4: mark props type in externals as `@live` to avoid dead code warnings for prop fields in the editor tooling. https://github.com/rescript-lang/rescript-compiler/pull/6796
- Fix issue where optional labels were not taken into account when disambiguating record value construction. https://github.com/rescript-lang/rescript-compiler/pull/6798
- Fix issue in gentype when type `Jsx.element` surfaces to the user. https://github.com/rescript-lang/rescript-compiler/pull/6808
- Fix inclusion check (impl vs interface) for untagged variants, and fix the outcome printer to show tags. https://github.com/rescript-lang/rescript-compiler/pull/6669
- Fix encoding inside tagged template literals. https://github.com/rescript-lang/rescript-compiler/pull/6810

# 11.1.1

#### :bug: Bug Fix

- Fix issue of incorrect switch cases with identical bodies when mixing object and array. https://github.com/rescript-lang/rescript-compiler/pull/6792
- Fix formatter eats comments on the first argument of a uncurried function. https://github.com/rescript-lang/rescript-compiler/pull/6763
- Fix formatter removes parens in pipe operator with anonymous uncurried function. https://github.com/rescript-lang/rescript-compiler/pull/6766

# 11.1.0

#### :bug: Bug Fix

- Revert escape JSX prop names with hyphens (#6705). https://github.com/rescript-lang/rescript-compiler/pull/6731

# 11.1.0-rc.8

#### :rocket: New Feature

- Add `%todo` extension for leaving implementation for later. https://github.com/rescript-lang/rescript-compiler/pull/6713
- Add `-warn-error` argument for generating errors in CI. Useful for `%todo` extension. https://github.com/rescript-lang/rescript-compiler/pull/6717

#### :bug: Bug Fix

- Improve error when using `@deriving(accessors)` on a variant with record arguments. https://github.com/rescript-lang/rescript-compiler/pull/6712
- Stop escaping JSX prop names with hyphens. https://github.com/rescript-lang/rescript-compiler/pull/6705
- Fix trailing undefined for optional parameters not omitted with `@send` and `@new`. https://github.com/rescript-lang/rescript-compiler/pull/6716
- Fix JSX4 adding the incorrect type annotation for the prop `ref` in `React.forwardRef` component. https://github.com/rescript-lang/rescript-compiler/pull/6718
- Fix description for warning number 110. https://github.com/rescript-lang/rescript-compiler/pull/6725

#### :nail_care: Polish

- Module spec `es6` and `es6-global` is deprecated in favor of `esmodule`. https://github.com/rescript-lang/rescript-compiler/pull/6709

# 11.1.0-rc.7

#### :bug: Bug Fix

- Fix variance setting for builtin `dict` type. Fixes issues around inference. https://github.com/rescript-lang/rescript-compiler/pull/6707

# 11.1.0-rc.6

#### :rocket: New Feature

- Add experimental BigInt support. https://github.com/rescript-lang/rescript-compiler/pull/6670, https://github.com/rescript-lang/rescript-compiler/pull/6696

#### :bug: Bug Fix

- Fix mishandling of uncurried functions in super errors. https://github.com/rescript-lang/rescript-compiler/pull/6694

# 11.1.0-rc.5

#### :bug: Bug Fix

- Fix misparsing in/after JSX. https://github.com/rescript-lang/rescript-compiler/pull/6686
- Fix `@deriving(accessors)` outputting curried functions in uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6687

# 11.1.0-rc.4

#### :bug: Bug Fix

- Fix emitting static import instead of dynamic import. https://github.com/rescript-lang/rescript-compiler/pull/6664
- Fix local type variables breaking react components. https://github.com/rescript-lang/rescript-compiler/pull/6665
- Fix remove redundant branches in generated switch body. https://github.com/rescript-lang/rescript-compiler/pull/6672
- Fix issue in partial application when the last named arg is provided. https://github.com/rescript-lang/rescript-compiler/pull/6681

#### :nail-care: Polish

- Omit `undefined` in external function calls for trailing optional arguments when not supplied. https://github.com/rescript-lang/rescript-compiler/pull/6653
- Make pattern match suggestions to be easier to copy-paste. https://github.com/rescript-lang/rescript-compiler/pull/6656

# 11.1.0-rc.3

#### :nail_care: Polish

- No parens around tagged template literals. https://github.com/rescript-lang/rescript-compiler/pull/6639
- Allow identifier with modules in tagged template literals (e.g. Pg.sql`select * from ${table} where id = ${id}`). https://github.com/rescript-lang/rescript-compiler/pull/6645

#### :bug: Bug Fix

- Fix compiler crash when reexporting tagged template literal externals. https://github.com/rescript-lang/rescript-compiler/pull/6645

# 11.1.0-rc.2

#### :rocket: New Feature

- Add support for array spread. https://github.com/rescript-lang/rescript-compiler/pull/6608
- Support import attributes (https://github.com/tc39/proposal-import-attributes) in `@module()`. https://github.com/rescript-lang/rescript-compiler/pull/6599
- allow hyphens in jsx tag names (e.g. `<mj-column>`). https://github.com/rescript-lang/rescript-compiler/pull/6609

#### :bug: Bug Fix

- Fix issue with async and newtype in uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6601
- Generic JSX transform: Rename expected module name for lowercase JSX to `Elements` from `DOM`. https://github.com/rescript-lang/rescript-compiler/pull/6606
- Generic JSX transform: Set default config params for `jsxConfig`. https://github.com/rescript-lang/rescript-compiler/pull/6606
- Generic JSX transform: Handle namespaced names. https://github.com/rescript-lang/rescript-compiler/pull/6606
- Fix issue with doc comment in recursive module. https://github.com/rescript-lang/rescript-compiler/pull/6613
- Fix issue with Exceptions and Extensible types runtime generation. https://github.com/rescript-lang/rescript-compiler/pull/6570
- Fix inline comment before spread syntax in record. https://github.com/rescript-lang/rescript-compiler/pull/6615

#### :house: Internal

- Use OCaml 4.14.1 (+ Alpine 3.19 container) for CI build. https://github.com/rescript-lang/rescript-compiler/pull/6600

# 11.1.0-rc.1

#### :rocket: New Feature

- Experimental support of tagged template literals, e.g. `` sql`select * from ${table}` ``. https://github.com/rescript-lang/rescript-compiler/pull/6250
- Experimental support for generic/custom JSX transforms. https://github.com/rescript-lang/rescript-compiler/pull/6565
- `dict` is now a builtin type. https://github.com/rescript-lang/rescript-compiler/pull/6590

#### :bug: Bug Fix

- GenType: distinguish inline records from unary variant cases of object type. https://github.com/rescript-lang/rescript-compiler/pull/6586

# 11.0.1

#### :bug: Bug Fix

- Renamed inline record fields: fix renamed field access in inline records. https://github.com/rescript-lang/rescript-compiler/pull/6551
- Fixed issue with coercions sometimes raising a `Not_found` instead of giving a proper error message. https://github.com/rescript-lang/rescript-compiler/pull/6574
- Fix issue with recursive modules and uncurried. https://github.com/rescript-lang/rescript-compiler/pull/6575

#### :nail_care: Polish

- Improve error message for missing label(s) in function application. https://github.com/rescript-lang/rescript-compiler/pull/6576

# 11.0.0

No changes compared to rc.9.

# 11.0.0-rc.9

#### :rocket: New Feature

- GenType: support `@deriving(accessors)` outputs. https://github.com/rescript-lang/rescript-compiler/pull/6537
- Allow coercing ints and floats to unboxed variants that have a catch-all unboxed int or float case. https://github.com/rescript-lang/rescript-compiler/pull/6540
- Allow tuples in untagged variants. https://github.com/rescript-lang/rescript-compiler/pull/6550

#### :bug: Bug Fix

- GenType: now emits full suffix on JS import path to be compatible with `.res.js`. https://github.com/rescript-lang/rescript-compiler/pull/6541

#### :nail_care: Polish

- Format docstrings. https://github.com/rescript-lang/rescript-compiler/pull/6417
- JSX v4: make automatic mode the default. https://github.com/rescript-lang/rescript-compiler/pull/6552

# 11.0.0-rc.8

#### :rocket: New Feature

- Add support for type coercion for invariant type arguments such as array payloads. https://github.com/rescript-lang/rescript-compiler/pull/6518
- Start treating `rescript` command the same as the `rescript build` command, so now you can do `rescript -w`. https://github.com/rescript-lang/rescript-compiler/pull/6524

#### :bug: Bug Fix

- Fix accidental removal of `Belt.Result.Ok` and `Belt.Result.Error` constructors in rc.5. https://github.com/rescript-lang/rescript-compiler/pull/6514
- Add missing check that the runtime representation of variants matches implementation and interface. https://github.com/rescript-lang/rescript-compiler/pull/6513/files
- GenType: only export types (not values) from module types. https://github.com/rescript-lang/rescript-compiler/pull/6516
- Fix compiler crash with unboxed variant definition with only 1 constructor. https://github.com/rescript-lang/rescript-compiler/pull/6523
- GenType: support mutual recursive types inside modules. https://github.com/rescript-lang/rescript-compiler/pull/6528
- Workaround for `@as` in labels in uncurried externals, which was broken. https://github.com/rescript-lang/rescript-compiler/pull/6527

#### :nail_care: Polish

- GenType: make outputs DCE-friendly. https://github.com/rescript-lang/rescript-compiler/pull/6508

# 11.0.0-rc.7

#### :rocket: New Feature

- Allow empty inline records in variants. https://github.com/rescript-lang/rescript-compiler/pull/6494
- Allow empty record patterns in pattern matching. https://github.com/rescript-lang/rescript-compiler/pull/6494

#### :bug: Bug Fix

- Fix issue where an inline record with attributes did not parse. https://github.com/rescript-lang/rescript-compiler/pull/6499
- Fix issue with uncurried function with 1 arg being a variable where an undefined variable could be emitted. https://github.com/rescript-lang/rescript-compiler/pull/6507
- Fix runtime errors on `@genType.as("alias")` output. https://github.com/rescript-lang/rescript-compiler/pull/6509

# 11.0.0-rc.6

#### :rocket: New Feature

- Freely configurable suffix for generated .js files. https://github.com/rescript-lang/rescript-compiler/pull/6472

#### :bug: Bug Fix

- Fix issue with GenType and `result` introduced in rc.5. https://github.com/rescript-lang/rescript-compiler/pull/6464
- Fix compiler crash when inlining complex constants in pattern matching. https://github.com/rescript-lang/rescript-compiler/pull/6471
- Fix issue with generating async functions inside loops. https://github.com/rescript-lang/rescript-compiler/pull/6479
- Fix issue with Gentype and string annotations with numbers such as `@as("0")`. https://github.com/rescript-lang/rescript-compiler/pull/6487
- Fix error message on curried/uncurried signature mismatch. https://github.com/rescript-lang/rescript-compiler/pull/6414

#### :nail_care: Polish

- Improve some error messages in rescript.conf parsing. https://github.com/rescript-lang/rescript-compiler/pull/6469

# 11.0.0-rc.5

#### :rocket: New Feature

- Allow coercing unboxed variants with only strings (now including with a single payload of string) to the primitive string. https://github.com/rescript-lang/rescript-compiler/pull/6441
- Allow coercing strings to unboxed variants that have a catch-all unboxed string case. https://github.com/rescript-lang/rescript-compiler/pull/6443
- Allow coercing `int` to `float`. https://github.com/rescript-lang/rescript-compiler/pull/6448

#### :bug: Bug Fix

- Fix issue with dynamic import of module in nested expressions. https://github.com/rescript-lang/rescript-compiler/pull/6431
- Fix issue where GenType was not supporting `@tag` on ordinary variants. https://github.com/rescript-lang/rescript-compiler/pull/6437
- Fix using dynamic import of module in block instead of async function. https://github.com/rescript-lang/rescript-compiler/pull/6434
- Fix issue with using dynamic import of module in uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6434
- Fix build error with JSX v4 transformation of React.forwardRef in uncurried mode. https://github.com/rescript-lang/rescript-compiler/pull/6447
- Fix printing of exotic JSX names. https://github.com/rescript-lang/rescript-compiler/pull/6451
- Fix locations when code with `await` fails to compile (all locations would point to the internal function `unsafe_await`). https://github.com/rescript-lang/rescript-compiler/pull/6452
- Fix renaming fields (with @as) in inline records doesn't work when destructuring. https://github.com/rescript-lang/rescript-compiler/pull/6456
- Fix `rc.4` regressions:
  - Don't show compilation time when calling `rescript build -help` command. https://github.com/rescript-lang/rescript-compiler/pull/6439
  - Running `rescript build -w` with a compilation error doesn't exit with an error code and continues waiting for changes. https://github.com/rescript-lang/rescript-compiler/pull/6460

#### :house: Internal

- Remove dependency stdlib-406 -> belt. https://github.com/rescript-lang/rescript-compiler/pull/6453
- Playground: Add support for implicitly opened modules. https://github.com/rescript-lang/rescript-compiler/pull/6446

#### :nail_care: Polish

- Add [`Deno`](https://deno.land/api?s=Deno) to reserved names, so that modules named `Deno` don't clash with the globally exposed `Deno` object. https://github.com/rescript-lang/rescript-compiler/pull/6428
- Disable ESLint/TSLint on gentype outputs properly. https://github.com/rescript-lang/rescript-compiler/pull/6442
- Improve `rescript` CLI to use `stdout`/`stderr` appropriately for help command's message. https://github.com/rescript-lang/rescript-compiler/pull/6439
- Generate `f()` instead of `f(undefined)` for `f()`. https://github.com/rescript-lang/rescript-compiler/pull/6459

# 11.0.0-rc.4

#### :rocket: New Feature

- Support renaming fields in inline records with `@as` attribute. [#6391](https://github.com/rescript-lang/rescript-compiler/pull/6391)
- Support renaming object fields of `@obj` external ppx with `@as` attribute. [#6391](https://github.com/rescript-lang/rescript-compiler/pull/6412)
- Add builtin abstract types for File and Blob APIs. https://github.com/rescript-lang/rescript-compiler/pull/6383
- Untagged variants: Support `promise`, RegExes, Dates, File and Blob. https://github.com/rescript-lang/rescript-compiler/pull/6383
- Untagged variants: Support `bool`. https://github.com/rescript-lang/rescript-compiler/pull/6368
- Support aliased types as payloads to untagged variants. https://github.com/rescript-lang/rescript-compiler/pull/6394
- Support the async component for React Server Component in JSX V4. https://github.com/rescript-lang/rescript-compiler/pull/6399
- Support `rescript.json` configuration file and deprecate `bsconfig.json`. https://github.com/rescript-lang/rescript-compiler/pull/6382

#### :boom: Breaking Change

- Update watcher rules to recompile only on config and `*.res`/`*.resi`/`*.ml`/`.mli` file changes. Solves the issue of unnecessary recompiles on `.css`, `.ts`, and other unrelated file changes. https://github.com/rescript-lang/rescript-compiler/pull/6420
- Add smart printer for pipe chains. https://github.com/rescript-lang/rescript-compiler/pull/6411 (the formatter will reformat existing code in certain cases)
- `Js.Json.t` now uses `Boolean(bool)` instead of explicit `@as(true) True | @as(false) False`. https://github.com/rescript-lang/rescript-compiler/pull/6421

#### :bug: Bug Fix

- Fix issue with GenType and labelled arguments. https://github.com/rescript-lang/rescript-compiler/pull/6406
- Fix dependencies reinitialization on every change in watch mode. Leads to faster rebuilds and cleaner terminal. https://github.com/rescript-lang/rescript-compiler/pull/6404

#### :nail_care: Polish

- A little performance improvement for JSX V4 runtime helper by removing one object allocation for components with key prop. https://github.com/rescript-lang/rescript-compiler/pull/6376
- The error message for "toplevel expressions should evaluate to unit" has been revamped and improved. https://github.com/rescript-lang/rescript-compiler/pull/6407
- Improve "Somewhere wanted" error messages by changing wording and adding more context + suggested solutions to the error messages where appropriate. https://github.com/rescript-lang/rescript-compiler/pull/6410
- Display the compile time for `rescript build` command. https://github.com/rescript-lang/rescript-compiler/pull/6404
- Improve help message for `build` and `clean` commands. https://github.com/rescript-lang/rescript-compiler/pull/6404
- Pass through the `-verbose` flag to builds in watch mode. https://github.com/rescript-lang/rescript-compiler/pull/6404
- Improve error message when defining duplicate labels in a record. https://github.com/rescript-lang/rescript-compiler/pull/6415
- Improve error message when trying to concatenate strings using the wrong operator. https://github.com/rescript-lang/rescript-compiler/pull/6416

# 11.0.0-rc.3

#### :bug: Bug Fix

- Fix issue with JSX V4 when component props have the default value with same name. https://github.com/rescript-lang/rescript-compiler/pull/6377
- Fixed code formatter with `"uncurried": false` in bsconfig. https://github.com/rescript-lang/rescript-compiler/pull/6378

#### :nail_care: Polish

- Add [`Bun`](https://bun.sh) to reserved names, so that modules named `Bun` don't clash with the globally exposed `Bun` object. https://github.com/rescript-lang/rescript-compiler/pull/6381

# 11.0.0-rc.2

#### :rocket: New Feature

- `rescript build` will always build its dependency by default. The argument `-with-deps` is not needed anymore. https://github.com/rescript-lang/rescript-compiler/pull/6350

#### :boom: Breaking Change

- Stop mangling object field names. If you had objects with field names containing "\_\_" or leading "\_", they won't be mangled in the compiled JavaScript and represented as it is without changes. https://github.com/rescript-lang/rescript-compiler/pull/6354

#### :bug: Bug Fix

- Fixed outcome printer resolution of uncurried config. https://github.com/rescript-lang/rescript-compiler/pull/6353

# 11.0.0-rc.1

#### :rocket: New Feature

- GenType: Propagate comments from record fields to emitted TypeScript types. https://github.com/rescript-lang/rescript-compiler/pull/6333

#### :boom: Breaking Change

- `$$default` is no longer exported from the generated JavaScript when using default exports. https://github.com/rescript-lang/rescript-compiler/pull/6328

#### :nail_care: Polish

- Conditionally print error message about record with missing label potentially being a component. https://github.com/rescript-lang/rescript-compiler/pull/6337
- Put definition in the bottom and the actual error at the top when reporting errors for supplying fields etc with the wrong name. https://github.com/rescript-lang/rescript-compiler/pull/6336
- Fix left over places where polyvariant tag names were printed in OCaml syntax instead of ReScript. https://github.com/rescript-lang/rescript-compiler/pull/6348

# 11.0.0-beta.4

#### :rocket: New Feature

- Variants: Allow coercing from variant to variant where applicable. https://github.com/rescript-lang/rescript-compiler/pull/6314
- Variants: Experimental support for spreading variant type definitions to copy constructors from one variant to another. https://github.com/rescript-lang/rescript-compiler/pull/6316

#### :boom: Breaking Change

- Fixed name collision between the newly defined Js.Json.t and the variant constructor in the existing Js.Json.kind type. To address this, the usage of the existing Js.Json.kind type can be updated to Js.Json.Kind.t. https://github.com/rescript-lang/rescript-compiler/pull/6317

#### :bug: Bug Fix

- Fixed outcome printing of uncurried higher order function types. https://github.com/rescript-lang/rescript-compiler/pull/6323
- Fixed printing of type constraints in template literal substitutions. https://github.com/rescript-lang/rescript-compiler/pull/6324

# 11.0.0-beta.3

#### :rocket: New Feature

- Untagged variants: consider regexp as an object type. https://github.com/rescript-lang/rescript-compiler/pull/6296
- Semantic-based optimization of code generated for untagged variants. https://github.com/rescript-lang/rescript-compiler/issues/6108
- Record type spreads: Allow using type variables in type spreads. Both uninstantiated and instantiated ones. https://github.com/rescript-lang/rescript-compiler/pull/6309
- Variants: Allow coercing variants to string/int/float when applicable. https://github.com/rescript-lang/rescript-compiler/pull/6311

#### :bug: Bug Fix

- Fix issue with dynamic import of modules in expressions. https://github.com/rescript-lang/rescript-compiler/pull/6310

# 11.0.0-beta.2

#### :rocket: New Feature

- Introduced a new `%ffi` extension (_experimental_ - not for production use!) that provides a more robust mechanism for JavaScript function interoperation by considering function arity in type constraints. This enhancement improves safety when dealing with JavaScript functions by enforcing type constraints based on the arity of the function. https://github.com/rescript-lang/rescript-compiler/pull/6251
- Extended untagged variants with function types. https://github.com/rescript-lang/rescript-compiler/pull/6279

#### :boom: Breaking Change

- Remove rudimentary node bindings and undocumented `%node` extension. https://github.com/rescript-lang/rescript-compiler/pull/6285

#### :bug: Bug Fix

- Fix issue where uncurried type internals leak in type error. https://github.com/rescript-lang/rescript-compiler/pull/6264
- Improve error messages for untagged variant definition. https://github.com/rescript-lang/rescript-compiler/pull/6290
- Fix type checking performance issue for large records. https://github.com/rescript-lang/rescript-compiler/pull/6289

# 11.0.0-beta.1

#### :rocket: Main New Feature

- Make uncurried mode opt-out: by default, every project is now in uncurried mode, unless `"uncurried": false` is specified in the project config. https://github.com/rescript-lang/rescript-compiler/pull/6249

#### :nail_care: Polish

- Removed duplicate Super_error implementation in syntax. https://github.com/rescript-lang/rescript-compiler/pull/6246

#### :bug: Bug Fix

- Fix issue with inlining records in the presence of record coercion. https://github.com/rescript-lang/rescript-compiler/pull/6256

# 11.0.0-alpha.6

#### :boom: Breaking Change

- `-bs-super-errors` flag has been deprecated along with Super_errors. https://github.com/rescript-lang/rescript-compiler/pull/6243
- `@rescript/react` >= 0.12.0-alpha.2 is now required because of the React.fragment's children type fix. https://github.com/rescript-lang/rescript-compiler/pull/6238

#### :bug: Bug Fix

- Remove unnecessary require and import statements when using dynamic imports. https://github.com/rescript-lang/rescript-compiler/pull/6232
- Fix option unboxing logic in the presence of untagged variants. https://github.com/rescript-lang/rescript-compiler/pull/6233
- Fix printing of local module with type. https://github.com/rescript-lang/rescript-compiler/issues/6212
- Adapting JSX4 to React.fragment's children type change (`'children` -> `React.element`) https://github.com/rescript-lang/rescript-compiler/pull/6238

#### :nail_care: Polish

- In uncurried mode, outcome printer swaps curried and uncurries function printing compared to legacy.
- Add location information to duplicate type definition error messages. https://github.com/rescript-lang/rescript-compiler/pull/6199
- Replace normal module errors with Super_error module, and clean up Super_error. https://github.com/rescript-lang/rescript-compiler/pull/6199
- `Js.Json.t`, `Js.null` and `Js.nullable` are now untagged variants representing their runtime values, instead of abstract types. https://github.com/rescript-lang/rescript-compiler/pull/6218

# 11.0.0-alpha.5

#### :rocket: Main New Feature

- Add support for Dynamic import. https://github.com/rescript-lang/rescript-compiler/pull/5703
- GenType: Add `moduleResolution` option to customize extensions on emitted import statements. This helps to adjust output compatibility with TypeScript projects using ESM. https://github.com/rescript-lang/rescript-compiler/pull/6182
  - `node` (default): Drop extensions.
  - `node16`: Use TS output's extensions. Make it ESM-compatible.
  - `bundler`: Use TS input's extensions. Make it ESM-compatible.
- Make untagged variants understand payloads defined as records. https://github.com/rescript-lang/rescript-compiler/pull/6208

#### :boom: Breaking Change

- Parse `assert` as a regular function. `assert` is no longer a unary expression. Example: before `assert 1 == 2` is parsed as `(assert 1) == 2`, now it is parsed as `assert(1 == 2)`. https://github.com/rescript-lang/rescript-compiler/pull/6180

#### :bug: Bug Fix

- Make "rescript format" work with node 10 again and set minimum required node version to 10 in package.json. https://github.com/rescript-lang/rescript-compiler/pull/6186
- Fix partial application for uncurried functions with labeled args https://github.com/rescript-lang/rescript-compiler/pull/6198
- Add error messages for dangling doc comments/attributes and mutable in record type definition. https://github.com/rescript-lang/rescript-compiler/pull/6206
- Fix issue with overlapping array and object in untagged variants https://github.com/rescript-lang/rescript-compiler/pull/6219

# 11.0.0-alpha.4

#### :rocket: Main New Feature

- Add surface syntax for partial application of uncurried functions: `foo(1, ...)`. This corresponds to curried application in the old mode. https://github.com/rescript-lang/rescript-compiler/pull/6166

#### :bug: Bug Fix

- Fix broken formatting in uncurried mode for functions with \_ placeholder args. https://github.com/rescript-lang/rescript-compiler/pull/6148
- Fix issue where spreading record types with optional labels would not have their labels preserved as optional. https://github.com/rescript-lang/rescript-compiler/pull/6154
- Fix error location to be the type with the spreads when spreading record types with duplicate labels. https://github.com/rescript-lang/rescript-compiler/pull/6157
- Disable warning on `@inline` attibute on uncurried functions. https://github.com/rescript-lang/rescript-compiler/pull/6152
- Support doc comments on arguments of function types. https://github.com/rescript-lang/rescript-compiler/pull/6161
- Fix issue with record type coercion and unboxed. https://github.com/rescript-lang/rescript-compiler/issues/6158
- Fixed subtype checking for record types with "@as" attributes: The subtype relationship now takes into account the compatibility of "@as" attributes between corresponding fields, ensuring correctness in runtime representation.
  https://github.com/rescript-lang/rescript-compiler/issues/6158
- Emit directive above header comment. https://github.com/rescript-lang/rescript-compiler/pull/6172
- Add error message to private extension. https://github.com/rescript-lang/rescript-compiler/pull/6175

#### :nail_care: Polish

- Update list of reserved JS keywords. https://github.com/rescript-lang/rescript-compiler/pull/6167
- Add error message to `@@directive`. https://github.com/rescript-lang/rescript-compiler/pull/6174

# 11.0.0-alpha.3

#### :rocket: Main New Feature

- Add support for extensible records (e.g. `type t = {...t1, x:int, ...t2}`) https://github.com/rescript-lang/rescript-compiler/pull/5715

#### :bug: Bug Fix

- Fix formatting and parentheses placement in uncurried functions with constraints. https://github.com/rescript-lang/rescript-compiler/pull/6143

# 11.0.0-alpha.2

#### :rocket: Main New Feature

- Add support for type coercion `:>` for records. https://github.com/rescript-lang/rescript-compiler/pull/5721

#### :bug: Bug Fix

- Special case generation of uncurried functions with 1 argument of unit type so they don't take a parameter. https://github.com/rescript-lang/rescript-compiler/pull/6131

# 11.0.0-alpha.1

#### :rocket: Main New Feature

- Introduce experimental uncurried mode. For experimentation only. [PR #5796](https://github.com/rescript-lang/rescript-compiler/pull/5796)
- Customization of runtime representation of variants and introduction of untagged variants [PR #6095](https://github.com/rescript-lang/rescript-compiler/pull/6095), [PR #6103](https://github.com/rescript-lang/rescript-compiler/pull/6103)

#### :rocket: New Feature

- Add support for uncurried mode: a mode where everything is considered uncurried, whether with or without the `.`. This can be turned on with `@@uncurried` locally in a file. For project-level configuration in `bsconfig.json`, there's a boolean config `"uncurried"`, which propagates to dependencies, to turn on uncurried mode.
  Since there's no syntax for partial application in this new mode, introduce `@res.partial foo(x)` to express partial application. This is temporary and will later have some surface syntax.
  Make uncurried functions a subtype of curried functions, and allow application for uncurried functions.
  The `make` function of components is generated as an uncurried function.
  Use best effort to determine the config when formatting a file.
  https://github.com/rescript-lang/rescript-compiler/pull/5968 https://github.com/rescript-lang/rescript-compiler/pull/6080 https://github.com/rescript-lang/rescript-compiler/pull/6086 https://github.com/rescript-lang/rescript-compiler/pull/6087
- Customization of runtime representation of variants. This is work in progress. E.g. some restrictions on the input. See comments of the form "TODO: put restriction on the variant definitions allowed, to make sure this never happens". https://github.com/rescript-lang/rescript-compiler/pull/6095
- Introduce untagged variants https://github.com/rescript-lang/rescript-compiler/pull/6103
- Add support for unary uncurried pipe in uncurried mode https://github.com/rescript-lang/rescript-compiler/pull/5804
- Add support for partial application of uncurried functions: with uncurried application one can provide a
  subset of the arguments, and return a curried type with the remaining ones https://github.com/rescript-lang/rescript-compiler/pull/5805
- Add support for uncurried externals https://github.com/rescript-lang/rescript-compiler/pull/5815 https://github.com/rescript-lang/rescript-compiler/pull/5819 https://github.com/rescript-lang/rescript-compiler/pull/5830 https://github.com/rescript-lang/rescript-compiler/pull/5894
- Parser/Printer: unify uncurried functions of arity 0, and of arity 1 taking unit. There's now only arity 1 in the source language. https://github.com/rescript-lang/rescript-compiler/pull/5825
- Add support for default arguments in uncurried functions https://github.com/rescript-lang/rescript-compiler/pull/5835
- Inline uncurried application when it is safe https://github.com/rescript-lang/rescript-compiler/pull/5847
- Support optional named arguments without a final unit in uncurried functions https://github.com/rescript-lang/rescript-compiler/pull/5907
- GenType: add the option to use the `@genType` annotation at the module level, meaning that all the items in the module should be exported. https://github.com/rescript-lang/rescript-compiler/pull/6113
- GenType: add support for `@genType` annotations on module definitions. https://github.com/rescript-lang/rescript-compiler/pull/6113
- Prebuilt binaries are now provided for all major platforms:
  - macOS x64
  - macOS ARM
  - Linux x64 (statically linked)
  - Linux ARM (statically linked)
  - Windows x64

#### :boom: Breaking Change

- Remove support for the legacy Reason syntax. Existing Reason code can be converted to ReScript syntax using ReScript 9 as follows:
  - `npm i -g rescript@9`
  - `rescript convert <reason files>`
- Remove obsolete built-in project templates and the "rescript init" functionality. This is replaced by [create-rescript-app](https://github.com/rescript-lang/create-rescript-app) which is maintained separately.
- Do not attempt to build ReScript from source on npm postinstall for platforms without prebuilt binaries anymore.
- Made pinned dependencies transitive: if _a_ is a pinned dependency of _b_ and _b_ is a pinned dependency of _c_, then _a_ is implicitly a pinned dependency of _c_. This change is only breaking if your build process assumes non-transitivity.
- Curried after uncurried is not fused anymore: `(. x) => y => 3` is not equivalent to `(. x, y) => 3` anymore. It's instead equivalent to `(. x) => { y => 3 }`.
  Also, `(. int) => string => bool` is not equivalen to `(. int, string) => bool` anymore.
  These are only breaking changes for unformatted code.
- Exponentiation operator `**` is now right-associative. `2. ** 3. ** 2.` now compile to `Math.pow(2, Math.pow(3, 2))` and not anymore `Math.pow(Math.pow(2, 3), 2)`. Parentheses can be used to change precedence.
- Remove unsafe `` j`$(a)$(b)` `` interpolation deprecated in compiler version 10 https://github.com/rescript-lang/rescript-compiler/pull/6068
- Remove deprecated module `Printexc`
- `@deriving(jsConverter)` not supported anymore for variant types https://github.com/rescript-lang/rescript-compiler/pull/6088
- New representation for variants, where the tag is a string instead of a number. https://github.com/rescript-lang/rescript-compiler/pull/6088
- GenType: removed support for `@genType.as` for records and variants which has become unnecessary. Use the language's `@as` instead to channge the runtime representation without requiring any runtime conversion during FFI. https://github.com/rescript-lang/rescript-compiler/pull/6099 https://github.com/rescript-lang/rescript-compiler/pull/6101

#### :bug: Bug Fix

- Fix issue where uncurried was not supported with pipe https://github.com/rescript-lang/rescript-compiler/pull/5803
- Fix printing of nested types in uncurried mode https://github.com/rescript-lang/rescript-compiler/pull/5826
- Fix issue in printing uncurried callbacks https://github.com/rescript-lang/rescript-compiler/pull/5828
- Fix formatting uncurried functions with attributes https://github.com/rescript-lang/rescript-compiler/pull/5829
- Fix parsing/printing uncurried functions with type parameters https://github.com/rescript-lang/rescript-compiler/pull/5849
- Fix compiler ppx issue when combining `async` and uncurried application https://github.com/rescript-lang/rescript-compiler/pull/5856
- Fix issue where the internal representation of uncurried types would leak when a non-function is applied in a curried way https://github.com/rescript-lang/rescript-compiler/pull/5892
- Fix some comments disappearing in array access expressions https://github.com/rescript-lang/rescript-compiler/pull/5947
- Parser: fix location of variable when function definition `{v => ...}` is enclosed in braces https://github.com/rescript-lang/rescript-compiler/pull/5949
- Fix issue with error messages for uncurried functions where expected and given type were swapped https://github.com/rescript-lang/rescript-compiler/pull/5973
- Fix issue with integer overflow check https://github.com/rescript-lang/rescript-compiler/pull/6028
- Make internal encoding of locations aware of unicode https://github.com/rescript-lang/rescript-compiler/pull/6073
- Fix issue where `foo(x,_)` in uncurried mode would generate a curried function https://github.com/rescript-lang/rescript-compiler/pull/6082
- Fix printing of uncurried application when the lhs is a function definition https://github.com/rescript-lang/rescript-compiler/pull/6084
- Fix parsing uncurried type starting with path https://github.com/rescript-lang/rescript-compiler/pull/6089
- Fix bigInt comparison https://github.com/rescript-lang/rescript-compiler/pull/6097
- Fixed a bug where the async attribute was not preserved when using the `@this` decorator in ReScript functions. This fix allows proper handling of async functions with the `@this` decorator. Issue: https://github.com/rescript-lang/rescript-compiler/issues/6100
- Fix issue with GenType and module aliases https://github.com/rescript-lang/rescript-compiler/issues/6112

#### :nail_care: Polish

- Syntax: process uncurried types explicitly in the parser/printer https://github.com/rescript-lang/rescript-compiler/pull/5784 https://github.com/rescript-lang/rescript-compiler/pull/5822
- Syntax: process uncurried function declarations explicitly in the parser/printer https://github.com/rescript-lang/rescript-compiler/pull/5794
- PPX V4: allow uncurried `make` function and treat it like a curried one [#5802](https://github.com/rescript-lang/rescript-compiler/pull/5802) [#5808](https://github.com/rescript-lang/rescript-compiler/pull/5808) [#5812](https://github.com/rescript-lang/rescript-compiler/pull/5812)
- Remove processing of objects expressions, which don't exist in `.res` syntax (`Pexp_object`) https://github.com/rescript-lang/rescript-compiler/pull/5841
- Remove class type processing from compiler ppx https://github.com/rescript-lang/rescript-compiler/pull/5842
- Remove method application via operator `##`, which does not exist in `.res` syntax https://github.com/rescript-lang/rescript-compiler/pull/5844
- Treat `@meth` annotation as making the type uncurried for backwards compatibitly with some examples https://github.com/rescript-lang/rescript-compiler/pull/5845
- Process `@set` annotation for field update as generating an uncurried function https://github.com/rescript-lang/rescript-compiler/pull/5846
- Treat uncurried application of primitives like curried application, which produces better output https://github.com/rescript-lang/rescript-compiler/pull/5851
- New internal representation for uncurried functions using built-in type `function$<fun_type, arity>` this avoids having to declare all the possible arities ahead of time https://github.com/rescript-lang/rescript-compiler/pull/5870
- PPX V3: allow uncurried `make` function and treat it like a curried one https://github.com/rescript-lang/rescript-compiler/pull/6081
- Add support for `|>` in uncurried mode by desugaring it https://github.com/rescript-lang/rescript-compiler/pull/6083
- Change the compilation of pattern matching for variants so it does not depends on variats being integers https://github.com/rescript-lang/rescript-compiler/pull/6085
- Improve code generated for string templates https://github.com/rescript-lang/rescript-compiler/pull/6090
- Move Jsx and JsxDOM and JsxEvent and JsxPPXReactSupport inside Pervasives and build them separately for curried and uncurried mode https://github.com/rescript-lang/rescript-compiler/pull/6091
- Gentype: allow recursive data types https://github.com/rescript-association/genType/issues/585

# 10.1.4

#### :bug: Bug Fix

- Fix implementation of directives https://github.com/rescript-lang/rescript-compiler/pull/6052
- Fix issue if the `lib` dir is included in the sources of bsconfig.json https://github.com/rescript-lang/rescript-compiler/pull/6055
- Fix issue with string escape in pattern match https://github.com/rescript-lang/rescript-compiler/pull/6062
- Fix issue with literal comparison of string constants https://github.com/rescript-lang/rescript-compiler/pull/6065

#### :rocket: New Feature

- Add support for toplevel `await` https://github.com/rescript-lang/rescript-compiler/pull/6054

#### :nail_care: Polish

- Better error message for extension point https://github.com/rescript-lang/rescript-compiler/pull/6057
- Improve format check help https://github.com/rescript-lang/rescript-compiler/pull/6056
- Deprecate unsafe `` j`$(a)$(b)` `` interpolation: use string templates `` `${a}${b}` `` instead https://github.com/rescript-lang/rescript-compiler/pull/6067

# 10.1.3

#### :rocket: New Feature

- Add experimental suppport for directives. An annotation such as `@@directive("use client;")` emits `use client;` verbatim before imports https://github.com/rescript-lang/rescript-compiler/pull/5999
- `genType`: add `Core` standard library support for the following builtin types: `Null.t`, `Nullable.t`, `Undefined.t`, `Dict.t<_>`, `Promise.t<_>`, `Date.t`, `BigInt.t`, `RegExp.t`, `Map.t<_, _>`, `WeakMap.t<_, _>`, `Set<_>`, `WeakSet<_>` https://github.com/rescript-lang/rescript-compiler/pull/6024

#### :boom: Breaking Change

- `genType`: streamline the treatment of optionals as undefined https://github.com/rescript-lang/rescript-compiler/pull/6024
  - Represent `option<t>` as `undefined | t` instead of `null | undefined | t`. This is more permissive when importing functions taking optional values (allows to use option types), but stricter when e.g. exporting ReScript functions taking arguments of option type. Fallback: use `Js.undefined<_>` instead.
  - Represent `{x:option<string>}` as `{x:(undefined | string)}` instead of `{x?: string}`. This is more in line with TS's behaviour. Fallback: use `{x?:string}`.

#### :nail_care: Polish

- Add the gap property to jsxDOMStyle https://github.com/rescript-lang/rescript-compiler/pull/5956

#### :bug: Bug Fix

- Fix issue where error messages related to non-existent props were displayed without location information https://github.com/rescript-lang/syntax/pull/730
- Fix issue where uncurried functions were incorrectly converting the type of a prop given as a default value to curried https://github.com/rescript-lang/syntax/pull/731
- Fix issue with nested async functions, where the inner function would be emitted without `async` https://github.com/rescript-lang/rescript-compiler/pull/5984
- Fix issue with printing async functions with locally abstract types https://github.com/rescript-lang/syntax/pull/732
- Fix issue with async context and locally abstract types https://github.com/rescript-lang/rescript-compiler/pull/5985
- Fix support for recursive components in JSX V4 https://github.com/rescript-lang/syntax/pull/733
- GenType: fix issue with V3 compatibility mode (see https://github.com/rescript-lang/rescript-compiler/issues/5990) https://github.com/rescript-lang/rescript-compiler/pull/5992
- Fix issue with overlapping labelled argument with default value https://github.com/rescript-lang/syntax/pull/734
- Fix issue with using alias and default value together https://github.com/rescript-lang/syntax/pull/734
- Fix issue in `Js.Promise2` where `then` and `catch` were returning `undefined` https://github.com/rescript-lang/rescript-compiler/pull/5996
- Fix issue in the compiler back-end where async functions passed to an `@uncurry` external would be inlined and transformed in a way that loses async https://github.com/rescript-lang/rescript-compiler/pull/6011
- Fix location issue for the treatment of `async` functions where hovering on the body with a type error would show `'a => promise<'a>` everywhere https://github.com/rescript-lang/rescript-compiler/pull/6014
- Fix formatting of `switch` expressions that contain braced `cases` inside https://github.com/rescript-lang/syntax/pull/735
- Fix formatting of props spread for multiline JSX expression https://github.com/rescript-lang/syntax/pull/736
- Support `@gentype.import` as an alias to `@genType.import` in the compiler https://github.com/rescript-lang/rescript-compiler/pull/6021
- In GenType, check annotations also in module types to decide whether to produce the `.gen.tsx` file https://github.com/rescript-lang/rescript-compiler/pull/5903
- Fix issue with JSX V4 and newtype https://github.com/rescript-lang/syntax/pull/737
- Fix issue with JSX V4 when components are nested https://github.com/rescript-lang/syntax/pull/738
- Fix issue where generic compare on `float` values would be different from the compare for type `float` https://github.com/rescript-lang/rescript-compiler/pull/6043
- Improve code generated for default arguments in JSX V4 https://github.com/rescript-lang/syntax/pull/739
- Fix issue with JSX V4 props of the form `~p as module(...)` https://github.com/rescript-lang/syntax/pull/739

# 10.1.2

#### :bug: Bug Fix

- Fix an issue where error messages related to duplicate props were displayed without a loc and were unclear https://github.com/rescript-lang/syntax/pull/728

# 10.1.1

#### :boom: Breaking Change

- Parse the attributes of labelled argument to the pattern attributes of argument instead of function. https://github.com/rescript-lang/syntax/pull/722
- The prop names duplicated to keyword are not mangled automatically in JSX v4.
  - Use `@as` instead

#### :rocket: New Feature

- Add support for empty inlined record literal `{}` for inlined records where all fields are optional https://github.com/rescript-lang/rescript-compiler/pull/5900

#### :bug: Bug Fix

- Prevent inlining of async functions in additional cases https://github.com/rescript-lang/rescript-compiler/issues/5860
- Fix build error where aliasing arguments to `_` in the make function with JSX V4. https://github.com/rescript-lang/rescript-compiler/pull/5881
- Fix parsing of spread props as an expression in JSX V4 https://github.com/rescript-lang/rescript-compiler/pull/5885
- Fix dropping attributes from props in make function in JSX V4 https://github.com/rescript-lang/rescript-compiler/pull/5905

# 10.1.0

#### :bug: Bug Fix

- Fix issue where no error was reported when ? was used for non-optional fields. https://github.com/rescript-lang/rescript-compiler/pull/5853
- Fix issue where optional fields in inline records were not supported and would cause type errors https://github.com/rescript-lang/rescript-compiler/pull/5827

# 10.1.0-rc.5

#### :bug: Bug Fix

- Prevent inlining of async functions in last stage of the compiler when the functions are not exported (not in interface file or shadowed) https://github.com/rescript-lang/rescript-compiler/pull/5790

# 10.1.0-rc.4

#### :rocket: New Feature

- Support format check with `rescript format -check`. https://github.com/rescript-lang/rescript-compiler/pull/5760

#### :bug: Bug Fix

- Fix issue where the last line of `rescript format --help` usage was being swallowed https://github.com/rescript-lang/rescript-compiler/pull/5760
- Specialize the printing of the rhs of a record field assignment for optional values `{x: ? e}` https://github.com/rescript-lang/syntax/issues/714

# 10.1.0-rc.3

#### :rocket: New Feature

- Support the use of spread anywhere in list creation (e.g. `list{...x, 1, ...y, ...z}`). https://github.com/rescript-lang/syntax/pull/692
- Add support for the argument of `@react.component` to set a props type from the outside. https://github.com/rescript-lang/syntax/pull/699

#### :bug: Bug Fix

- Fix issue where the JSX key type is not an optional string https://github.com/rescript-lang/syntax/pull/693
- Prevent inlining of async functions https://github.com/rescript-lang/rescript-compiler/issues/5754
- Fix build error for JSX fragment without children https://github.com/rescript-lang/syntax/pull/704
- Fix issue where async as an id cannot be used with application and labelled arguments https://github.com/rescript-lang/syntax/issues/707
- Fix 5557: the exhaustive checking for char is incorrect during the unicode migration https://github.com/rescript-lang/rescript-compiler/pull/5749
- Fix 5753: the comment for unicode char is inaccurate https://github.com/rescript-lang/syntax/pull/709
- Treat await as almost-unary operator weaker than pipe so `await foo->bar` means `await (foo->bar)` https://github.com/rescript-lang/syntax/pull/711

#### :nail_care: Polish

- Change payload of `Pconst_char` from `char` to `int` https://github.com/rescript-lang/syntax/pull/709

# 10.1.0-rc.2

#### :bug: Bug Fix

- Fix issue with changes not being applied with React Native's Metro bundler for files with warnings https://github.com/rescript-lang/rescript-compiler/pull/5738
- Fix emitting unary minus for floats in case of negative constants https://github.com/rescript-lang/rescript-compiler/pull/5737
- Fix issue where a spread `...x` in non-last position would not be reported as syntax error https://github.com/rescript-lang/syntax/pull/673/
- Fix issue where the formatter would delete `async` in a function with labelled arguments.
- Fix several printing issues with `async` including an infinite loop https://github.com/rescript-lang/syntax/pull/680
- Fix issue where certain JSX expressions would be formatted differenctly in compiler 10.1.0-rc.1 https://github.com/rescript-lang/syntax/issues/675
- Fix issue where printing nested pipe discards await https://github.com/rescript-lang/syntax/issues/687

# 10.1.0-rc.1

#### :boom: Breaking Change

- Deprecate DOM element attributes in `JsxDOM.domProps`: `begin_`, `end_`, `to_`
  - Use `begin`, `end`, `to` instead.
- Emit an error when a `@string` or `@int` attribute is used in a V4 component https://github.com/rescript-lang/rescript-compiler/issues/5724

## :rocket: New Feature

- Add extra variants for output filename suffixes in `bsconfig.json`: `.bs.mjs` and `.bs.cjs` are allowed https://github.com/rescript-lang/rescript-compiler/pull/5631
- Safe promises: t-first Js.Promise2 bindings, and remove warning for nested promises https://github.com/rescript-lang/rescript-compiler/pull/5709

#### :bug: Bug Fix

- Fix issue where uncurried async functions were emitted without `async` https://github.com/rescript-lang/rescript-compiler/pull/5718
- Fix location issue in error messages with JSX V4 where the multiple props types are defined https://github.com/rescript-lang/syntax/pull/655
- Fix location issue in make function in JSX V4 that breaks dead code elimination https://github.com/rescript-lang/syntax/pull/660
- Fix parsing (hence pretty printing) of expressions with underscore `_` and comments.
- Fix printing of comments inside JSX tag https://github.com/rescript-lang/syntax/pull/664
- Fix issue where formatter erases tail comments inside JSX tag https://github.com/rescript-lang/syntax/issues/663
- Fix issue where the JSX prop has type annotation of the first class module https://github.com/rescript-lang/syntax/pull/666
- Fix issue where an empty record literal {} expected to have a non-record type would type check https://github.com/rescript-lang/rescript-compiler/pull/5729

#### :eyeglasses: Spec Compliance

- Functions with consecutive dots now print as multiple arrow functions like in JavaScript.

#### :nail_care: Polish

- Add `loading`, `aria-*` DOM element attributes in `JsxDOM.domProps`: `ariaCurrent`, `ariaInvalid`, `ariaAutocomplete`, etc.
- Change the internal representation of props for the lowercase components to record. https://github.com/rescript-lang/syntax/pull/665
- Add `JsxPPXReactSupport` module to relocate the helper functions for JSX v4 from `rescript-react`

# 10.1.0-alpha.2

#### :rocket: New Feature

- Fix pretty printer where it would print doc comments on the same line as other attributes https://github.com/rescript-lang/syntax/pull/642
- Propagte `"jsx"` configuration to dependencies https://github.com/rescript-lang/rescript-compiler/pull/5661
- Add support for empty record literal `{}` for records where all fields are optional https://github.com/rescript-lang/rescript-compiler/pull/5658
- Add support for empty record type (e.g. `type empty = {}`) https://github.com/rescript-lang/rescript-compiler/pull/5658

#### :bug: Bug Fix

- Fix issue in formatting JSX spread props https://github.com/rescript-lang/syntax/pull/644
- Fix location issue in error messages with JSX V4 where the body of the component is an application https://github.com/rescript-lang/syntax/pull/633
- Fix printing of type declarations in error message where they would be considered recursive by default
- Fix issue where the printer would omit attributes for `->` and `|>` https://github.com/rescript-lang/syntax/pull/629
- Fix printing of optional fields in records https://github.com/rescript-lang/rescript-compiler/issues/5654
- Fix printing of comments inside empty blocks https://github.com/rescript-lang/syntax/pull/647

#### :nail_care: Polish

- Improvements and fixes for JSX V4. See guide https://github.com/rescript-lang/syntax/blob/master/cli/JSXV4.md
- Mention all missing fields in error message for records, not just one https://github.com/rescript-lang/rescript-compiler/pull/5657

# 10.1.0-alpha.1

#### :boom: Breaking Change

- Pipe `->` does not support a code block on the right-hand side e.g. `x->{ open A; get("test") }`

#### :rocket: New Feature

- Experimental support for for `async`/`await` https://github.com/rescript-lang/rescript-compiler/pull/5537
- Make `promise` a built-in type https://github.com/rescript-lang/rescript-compiler/pull/5650
- Initial support for JSX V4 including genType, still work in progress.
  - :boom: when V4 is activated, at most one component is allowed for each module.
- Add placeholder types for ES6 collections: `Set`, `Map`, `WeakSet`, and `WeakMap` https://github.com/rescript-lang/rescript-compiler/pull/5630

#### :bug: Bug Fix

- Fix issue with arrays and creation of recursive values https://github.com/rescript-lang/rescript-compiler/pull/5640
- Fix issue where characters such as newlines would be escaped in a template string expression https://github.com/rescript-lang/rescript-compiler/issues/5638
- Fix issue where pipe `->` processing eats up attributes https://github.com/rescript-lang/rescript-compiler/pull/5581
- Fix issue where cancelling `rescript build` would leave the `.bsb.lock` file behind and block future builds

#### :nail_care: Polish

- Print patterns in warnings using rescript printer https://github.com/rescript-lang/rescript-compiler/pull/5492

# 10.0.1

#### :bug: Bug Fix

- Fix issue where watch mode would give an error on Windows https://github.com/rescript-lang/rescript-compiler/pull/5621

# 10.0.0

**Compiler**

#### :boom: Breaking Change

- `bsconfig.json` does not support `// line` comments anymore.
  - Example: `"suffix": ".bs.js" // determine the suffix`
  - Fix: remove the comment and use standard json.
- Changed return type of `Js.String.match_` as it was wrong. [#5070](https://github.com/rescript-lang/rescript-compiler/pull/5070)
  - Example: any use of `Js.String.match_` and `Js.String2.match_`
  - Fix: follow the type errors
- GenType is now vendored in the compiler and drops support for the Flow and untyped back-ends to focus on providing a better experience for TypeScript.
  - Fix: keep on using the older version of the compiler and the separate genType package if Flow support is required. Migrate to TS if one wants to upgrade the compiler version.

#### :rocket: New Feature

- New records with optional fields e.g. `type opt = {x: int, y?: string}` were added as an experimental feature [#5423](https://github.com/rescript-lang/rescript-compiler/pull/5423) [#5452](https://github.com/rescript-lang/rescript-compiler/issues/5452) [New Syntax](https://github.com/rescript-lang/syntax/pull/589/files)
- Add support for `@new @variadic` (see https://github.com/rescript-lang/rescript-compiler/pull/5364)

#### :bug: Bug Fix

- Classify bigint correctly [#5351](https://github.com/rescript-lang/rescript-compiler/pull/5351)
- Fixed crash in `rescript build` on Windows [#5516](https://github.com/rescript-lang/rescript-compiler/pull/5516)
- Fixed `rescript init` command not working [#5526](https://github.com/rescript-lang/rescript-compiler/pull/5526)
- Fix issue with compiler log not terminated that causes problems with editor extension not clearing issues when fixed [#5545](https://github.com/rescript-lang/rescript-compiler/issues/5545)

##### :nail_care: Polish

- Changed Linux build to depend on GLIBC 2.28 again for compatibility with Debian 10.

- Proper M1 support (CI now supports M1 native builds)

**Syntax**

#### :boom: Breaking Change

- `@bs.send.pipe` is now removed. Earlier it was deprecated.
- Missing labels in function application is now an error (https://forum.rescript-lang.org/t/ann-more-strict-checks-in-missed-labels/2117).
  - Example: `let f = (x, ~z) => x + z; f(1, 2)`
  - Fix: do `let f = (x, ~z) => x + z; f(1, ~z=2)` instead
- Externals without `@val` annotations do not work anymore, and externals with `= ""` give an error.
  - Example: `external setTimeout: (unit => unit, int) => float = "setTimeout"` is not supported anymore.
  - Fix: use `@val external setTimeout: (unit => unit, int) => float = "setTimeout"` instead.
  - Example2: `@val external setTimeout: (unit => unit, int) => float = ""` is not supported anymore.
  - Fix2: use `@val external setTimeout: (unit => unit, int) => float = "setTimeout"` instead.
- Strings processed at compile-time don't need escaping anymore.
  - Example: `let blockCommentsRe = %re("/\\/\\*([^*]|[\\r\\n]|(\\*+([^*/]|[\\r\\n])))*\\*+\\//g")`.
  - Fix: use `let blockCommentsRe = %re("/\/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+\//g")` instead.
- Remove parsing of "import" and "export" which was never officially supported https://github.com/rescript-lang/syntax/pull/597 https://github.com/rescript-lang/syntax/pull/599
  - Example: `export type t = int`
  - Fix: `@genType type t = int`
  - Example2: `import realValue: complexNumber => float from "./MyMath"`
  - Fix2: `@genType.import("./MyMath") external realValue: complexNumber => float = "realValue"`

#### :rocket: New Feature

- Unicode is now supported in regular strings and chars (when the symbol fits). This is now going to work: `let str = "Σ"`. And, you'll be able to pattern match on unicode chars: `switch c { | 'Σ' => "what a fine unicode char" | _ => "unicode is fun" }`
- Doc comments `/** ... */` are now supported. Inernally, they are attributes, so are only valid at positions where `@foo` is allowed, or a syntax error is given. Similarly for module-level `/*** comments */` that can go where `@@attributes` go.

#### :bug: Bug Fix

- Fix printing for inline nullary functor types [#477](https://github.com/rescript-lang/syntax/pull/477)
- Fix stripping of quotes for empty poly variants [#474](https://github.com/rescript-lang/syntax/pull/474)
- Implement syntax for arity zero vs arity one in uncurried application in [#139](https://github.com/rescript-lang/syntax/pull/139)
- Fix parsing of first class module exprs as part of binary/ternary expr in [#256](https://github.com/rescript-lang/syntax/pull/256)
- Fix formatter hanging on deeply nested function calls [#261](https://github.com/rescript-lang/syntax/issues/261)

**Libraries**

#### :boom: Breaking Change

- **"Attributes not allowed here"**. If you see this error chances are you're using a ppx that needs updating to a new version.
  See an exampe of how to [update a ppx](https://github.com/zth/rescript-relay/pull/372)
  - Example: for `rescript-relay` 0.23.0 is not supported.
  - Fix: use `rescript-relay@beta` or the new version when released.
- Removed printing modules (Printf, Format etc) and related functions. Details of files added/removed: https://github.com/rescript-lang/rescript-compiler/commit/0fd8bb0e77c4b0e96a9647ac8af614305057003f.

#### :bug: Bug Fix

- Fix library issue with missing `bytes_to_string` https://github.com/rescript-lang/rescript-compiler/issues/5573 https://github.com/rescript-lang/rescript-compiler/pull/5589

#### :nail_care: Polish

- Several Belt / Js libraries are now converted to ReScript syntax, with corresponding comments in Markdown format suitable for hovering. See [#5361](https://github.com/rescript-lang/rescript-compiler/pull/5361).

**Playground**

#### :house: Internal

- Added `jsoo_playground_main.ml` as the rescript-lang.org playground bundle entrypoint

#### :boom: Breaking Change

- Removed Reason syntax support for the playground experience. See https://github.com/rescript-lang/rescript-compiler/pull/5375

---

You can find more old changelog from [docs/changelog](docs/changelog)
