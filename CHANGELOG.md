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

# 11.1.5 (Unreleased)

#### :boom: Breaking Change

- Deprecate JSX 3. https://github.com/rescript-lang/rescript-compiler/pull/7042
- Deprecate js_cast.res. https://github.com/rescript-lang/rescript-compiler/pull/7074
- Deprecate top-level `"suffix"` option in `rescript.json`. https://github.com/rescript-lang/rescript-compiler/pull/7056

#### :bug: Bug Fix

- Handle absolute file paths in gentype. https://github.com/rescript-lang/rescript-compiler/pull/7111
- Fix "rescript format" with many files. https://github.com/rescript-lang/rescript-compiler/pull/7081
- Fix exponential notation syntax. https://github.com/rescript-lang/rescript/pull/7174

#### :house: Internal

- Playground: Bundle and upload stdlib runtime so that the playground can execute functions from Core/Belt/Js. https://github.com/rescript-lang/rescript/pull/7268

# 11.1.4

- Fix issue where long layout break added a trailing comma in partial application `...`. https://github.com/rescript-lang/rescript-compiler/pull/6949
- Fix incorrect format of function under unary operator. https://github.com/rescript-lang/rescript-compiler/pull/6953
- Fix incorrect incorrect printing of module binding with signature. https://github.com/rescript-lang/rescript-compiler/pull/6963
- Disallow spreading anything but regular variants inside of other variants. https://github.com/rescript-lang/rescript-compiler/pull/6980
- Fix comment removed when function signature has `type` keyword. https://github.com/rescript-lang/rescript-compiler/pull/6997
- Fix parse error on doc comment before "and" in type def. https://github.com/rescript-lang/rescript-compiler/pull/7001
- Fix tuple coercion. https://github.com/rescript-lang/rescript-compiler/pull/7024

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

- Experimental support of tagged template literals, e.g. ```sql`select * from ${table}```. https://github.com/rescript-lang/rescript-compiler/pull/6250
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

- Stop mangling object field names. If you had objects with field names containing "__" or leading "_", they won't be mangled in the compiled JavaScript and represented as it is without changes. https://github.com/rescript-lang/rescript-compiler/pull/6354

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

- Introduced a new  `%ffi` extension (*experimental* - not for production use!) that provides a more robust mechanism for JavaScript function interoperation by considering function arity in type constraints. This enhancement improves safety when dealing with JavaScript functions by enforcing type constraints based on the arity of the function. https://github.com/rescript-lang/rescript-compiler/pull/6251
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

- Fix broken formatting in uncurried mode for functions with _ placeholder args. https://github.com/rescript-lang/rescript-compiler/pull/6148
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
- Made pinned dependencies transitive: if *a* is a pinned dependency of *b* and *b* is a pinned dependency of *c*, then *a* is implicitly a pinned dependency of *c*. This change is only breaking if your build process assumes non-transitivity.
- Curried after uncurried is not fused anymore: `(. x) => y => 3` is not equivalent to `(. x, y) => 3` anymore. It's instead equivalent to `(. x) => { y => 3 }`.
Also, `(. int) => string => bool` is not equivalen to `(. int, string) => bool` anymore.
These are only breaking changes for unformatted code.
- Exponentiation operator `**` is now right-associative. `2. ** 3. ** 2.` now compile to `Math.pow(2, Math.pow(3, 2))` and not anymore `Math.pow(Math.pow(2, 3), 2)`. Parentheses can be used to change precedence.
- Remove unsafe ``` j`$(a)$(b)` ``` interpolation deprecated in compiler version 10 https://github.com/rescript-lang/rescript-compiler/pull/6068
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
- Deprecate unsafe ``` j`$(a)$(b)` ``` interpolation: use string templates ``` `${a}${b}` ``` instead https://github.com/rescript-lang/rescript-compiler/pull/6067

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

# 9.1.4

## Build

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

## Syntax

- #432 bad error message for unterminated quote

## Compiler

- #5165 bad error message for uncurried type mistmatch
- #5169 fix a code gen issue with user defined `None`

# 9.1.3 (bug fix release)

## Build

- #5154 when toplevel package-specs get changed, its dependencies should be rebuilt
- #5152 Rebuild not triggered when deletion with nested modules
- #5153 when Different compiler version triggered,
  it should clean the whole dependencies instead of just the repo itself
- #5080 Add back -ws option for the build

## Syntax

- #425 fix CRLF handling for windows
- #414 Fix printing of underscore Pexp_fun sugar in context of Array.get
- #408 Don't parse Int token with suffices as hash ident for poly variants
- #410 Fix parsing of arrow type with objects in constructor declaration args
- #404 fix printing of Osig_module in outcome printer
- #402 Implement printing of `Otyp_module` in outcome printer

# 9.1

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

# 9.0.2

- #4990 Fix an optimization bug introduced in 9.0

- #4982 pattern match over modules

Provide user with a sugar to pattern match over modules:

```res
let {length, cons} = module(List)
```

More discussions can be found [here](https://forum.rescript-lang.org/t/introducing-an-extension-to-make-pattern-match-works-on-modules/1196)

# 9.0

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

# 8.4.2

- #4864 #4865
  Not install dev directory for pinned dependencies
- #4863
  for a package, only cmi/cmj artifact changes will trigger a rebuild

# 8.4.1

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

# 8.3.3

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

# 8.3.1

This is a minor bug fix release for 8.3.0

- capture warnings when rebuild without enforce warn-as-error
- #4716 internal, make ninja a submodule in dev process
- #4722 better dataflow for cases like `let {a;b} as obj = ...`
- no need call `caml_enter_blocking_section` for single threaded compiler
- #4739 fix the interaction of exotic filenames like `[id]` with the build system.

# 8.3

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

# 8.2

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

# 8.0

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

# 7.3.2

- #4315 synup super_errors for better uncurry error message
- #4335 fix js_date.setUTCTime
- #4343 fix regex syntax checking for empty or comment string
- #4351 precie binding location for reasonreact jsx

# 7.3

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

# 7.2

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

# 7.1.0

(it was 7.0.2 but bumped into 7.1.0 due to a breaking change introduced in 7.0.2)

- #4131 support `bstracing -C`

# 7.0.2

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

# 7.0.1

- #4009 #4011 fix ocaml type checking bug (cherry pick)
- #4000 #4010 add react-starter theme
- #4005 fix windows installing issues

# 7.0.0

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

# 5.2.1 (2019/10/16)

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

# 5.2.0 (2019/09/23)

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

# 5.1.0 (2019/08/15)

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

# 5.0.6

Fixes

- #3648, #3647, #3645 make sure bsppx.exe (used by editor tools/Merlin, Reason Language service) behaves consistently with bsc.exe

- #3643 fix a hidden out of bounds bug

- #3642 pass down ninja internal flags in combination of `bsb -make-world`, for example `bsb -make-world -- -d explain`

- #3641 fix pnpm install
- #3635 fix debug mode runtime crash
- #3633 fix bs dev dependency issue

# 5.0.5

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

# 5.0.4

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

# 5.0.1

Features

- #3479 add a theme named react-hooks for the new ppx
- #3476 add bs.inline support for literals (int, string, bool) so that it gets a stronger guarantee for inlining
- #3473 upstream reason@3c6a9ca98
- #3470,#3466 ract jsx ppx

Fixes

- #3455 fix polymorphic comparison and equality for js date
- #3465 fix brutal console.clear
- #3468 add BS_VSCODE to disable -super-errors, which works better with vscode problem matcher

# 5.0.0

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

# 4.0.17

Features

- #3229 true seperate compilation, incredible perf for incremental build

Fixes

- #3226, #3223 absolute path generated in recursive module path and `assert false`
- #3220 ppx-flags & scoped packages
- #3214 shadowing of js Promise constructor
- #3213 Allow build to be re-entrant to deal with yarn issues

# 4.0.10

Fixes

- upstream a bug fix for refmt
- Fix installation issues

# 4.0.8

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

# 4.0.7

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

# 4.0.6

## ReasonML synced 76b27

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

# 4.0.4

Fixes

- #3001 fix regressios in refmt
- #2986 #2973 #2974 fix bsb websocket exit error
- #2983 #2988 determinsic behavior
  when NINJA_ANSI_FORCED=0 no color
  when NINJA_ANSI_FORCED=1 yes color

# 4.0.3

Fixes

- #2956 clean re.js for genFlow proj
- #2970 remove one obsolte error
- #2970 address one regression from refmt

# 4.0.2

Fixes

- #2963 fix ppx-flags quoting issue

Features

- #2951 sync up with reason
- #2964 customize ninja to make output less verbose
  Add NINJA_ANSI_FORCE env variable support so that third party tools running bsb can still preserve colors
- #2960 add tea theme support
- #2959 less verbose bsb output
- #2958 make `bsb -init` more forgiving

# 4.0.1

Fixes:

- #2949 fix optional regression
- #2946 fix react-lite theme on Linux

# 4.0.0

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

# 3.1.4

Fixes:

- Put back the deprecated `Js.to_bool` and `Js.Boolean.*` for a little bit longer to avoid breackage in userland. These functions are all deprecated and don't do anythinng anymore, since we compile `bool` to `Js.boolean` directly.

# 3.1.0

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

# 3.0.0

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

# 2.2.3

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

# 2.2.2

Features:

- Upgrade with latest reason syntax (native unucurry support etc)
- #2531 add missing functions in Js.Nullable fromoption/toOption
- #2527 Belt.List.shuffle

Fixes:

- #2503 bs.string and bs.obj interaction
- #2549, #2548, $2548, #2542, #2541 improve error message

# 2.2.0

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

# 2.1.0

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

# 2.0

Features

- update reason3 syntax

# 1.10.3

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

# 1.9.3

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

# 1.9.2

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

# 1.9.1 (Recovery 1.9.0)

Fixes

- #1933 hyphen directory name fixes

# 1.9.0

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

# 1.8.2

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

# 1.8.1

Fixes:

- #1762, discard effectful unused code
- #1760, div by zero effect analysis

# 1.8.0

Fixes:

- #1573, don't include `-bs` flags in `.merlin`
- #1716, fix wrong optimization of recursive values
- #1728, bad inlining
- #1409, make sure when optional is None, bs.obj will not introduce such label
- #1737, same as #1409
- #1746, a corner case when mixing recursive value and functions, should make markup.ml work
- #1749, fix nested if, code block discarded issue

# 1.7.5

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

# 1.7.4

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

# 1.7.3

Fixes:

- #1556, fix duplicated requires of runtime (report by Cheng Lou)
- #1568, internal compiler error

Features:

- #1564: scoped values in FF, see `bs.scope` in the Manual

# 1.3.2

Features:

- Significantly improve bsb experience

## 1.2.1 + dev

Features:

- #861, add `-bs-assume-no-mli` and `-bs-no-implicit-include` for deterministic build
- #851, add -bs-D -bs-list-conditionals flags
- add `-bs-syntax-only`
- #854, add `-bs-binary-ast`

# 1.1.2

Fixes:

- #831, bug fix with opam issues

Features:

- Provide `bspp.exe` for the official compiler

# 1.1.1

Features:

- #822, add `bsdep.exe`
- #820, conditional compilation support
- #793, relax syntactic restrictions for all extension point so that `bs.obj`, `obj`, `bs.raw`, `raw`, etc. will both work. Note that all attributes will still be qualified
- #793, support `bs.splice` in `bs.new`
- #798, complete `bs.splice` support and documentation

# 1.0.3

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

# 1.0.2

Fixes:

- #743, Fix Bytes.blit when `src == dst`

Features:

- #783, by default, `bsc.exe` will warn when it detect some OCaml data types are passed from/to external FFI
- #784, add an option `-bs-eval`

# 1.0.1

Fixes:

- #718, Enforce `#=` always return unit for better error messages

Features:

- FFI
  - #694, support fields and mutable fields in JS object creation and private method
  - #686, introduce phantom arguments (`bs.ignore`) for ad-hoc polymorphism

# 1.0.0

Initial release
