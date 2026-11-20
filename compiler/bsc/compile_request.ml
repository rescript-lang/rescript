(* A compiler request's options as a record, read from and written as bsc's
   command-line flags. A field holds what the flags gave: a flag that was not
   given leaves its default, and repeatable flags keep their order. [apply]
   applies the options on top of the request's state as the flags did, so the
   flags of a file's @@config are applied on top of the command line's. *)

(** Who sets a flag: the build system for every module, anyone (users through
    rescript.json or @@config, and the build system for its own settings),
    compiler developers inspecting a compile, or a command that does something
    other than compile. *)
type kind = Build_system | Public | Debug | Command

type jsx = {
  version: Js_config.jsx_version option;
  module_: string option;
  preserve: bool;
}

type source_map = {
  mode: Js_config.source_map option;
  sources_content: bool option;
  root: string option;
}

type gentype = {
  enabled: bool;
  module_: Gentype_config.module_ option;
  module_resolution: Gentype_config.module_resolution option;
  export_interfaces: bool;
  generated_extension: string option;
  suffix: string option;
  shims: (string * string) list;
  debug_items: string list;
  dependencies: string list;
  source_dirs: string list;
  dep_paths: (string * string) list;
  bsb_project_root: string option;
}

type codegen = {
  debug: bool;
  no_version_header: bool;
  cross_module_opt: bool option;
  no_check_div_by_zero: bool;
  no_assert_false: bool;
  noassert: bool;
  no_bin_annot: bool;
  keep_locs: bool option;
  short_paths: bool;
  no_builtin_ppx: bool;
  force_cmi: bool;
  force_cmj: bool;
  check_lam: bool;
  debug_ir: bool;
}

type stdlib = {nostdlib: bool; nopervasives: bool; make_runtime: bool}

type debug = {
  dparsetree: bool;
  dtypedtree: bool;
  drawlambda: bool;
  draw_coercions: bool;
  dsource: bool;
  locations: bool;
  only_parse: bool;
  syntax_only: bool;
  cmi_only: bool;
  ignore_parse_errors: bool;
  editor_mode: bool;
  as_pp: bool;
  test_ast_conversion: bool;
  verbose: bool;
}

type warning = {as_error: bool; spec: string}

type t = {
  include_dirs: string list;
  output: string option;
  read_cmi: bool;
  namespace: string option;
  package_name: string option;
  package_outputs: Js_packages_info.package_info list;
  project_root: string option;
  runtime_path: string option;
  ast_only: bool;
  transparent_modules: bool;
  absname: bool;
  color: Misc.Color.setting option;
  open_modules: string list;
  warnings: warning list;
  ppx: string list;
  experimental: Experimental_features.feature list;
  jsx: jsx;
  source_map: source_map;
  gentype: gentype;
  codegen: codegen;
  stdlib: stdlib;
  debug: debug;
}

let empty =
  {
    include_dirs = [];
    output = None;
    read_cmi = false;
    namespace = None;
    package_name = None;
    package_outputs = [];
    project_root = None;
    runtime_path = None;
    ast_only = false;
    transparent_modules = false;
    absname = false;
    color = None;
    open_modules = [];
    warnings = [];
    ppx = [];
    experimental = [];
    jsx = {version = None; module_ = None; preserve = false};
    source_map = {mode = None; sources_content = None; root = None};
    gentype =
      {
        enabled = false;
        module_ = None;
        module_resolution = None;
        export_interfaces = false;
        generated_extension = None;
        suffix = None;
        shims = [];
        debug_items = [];
        dependencies = [];
        source_dirs = [];
        dep_paths = [];
        bsb_project_root = None;
      };
    codegen =
      {
        debug = false;
        no_version_header = false;
        cross_module_opt = None;
        no_check_div_by_zero = false;
        no_assert_false = false;
        noassert = false;
        no_bin_annot = false;
        keep_locs = None;
        short_paths = false;
        no_builtin_ppx = false;
        force_cmi = false;
        force_cmj = false;
        check_lam = false;
        debug_ir = false;
      };
    stdlib = {nostdlib = false; nopervasives = false; make_runtime = false};
    debug =
      {
        dparsetree = false;
        dtypedtree = false;
        drawlambda = false;
        draw_coercions = false;
        dsource = false;
        locations = false;
        only_parse = false;
        syntax_only = false;
        cmi_only = false;
        ignore_parse_errors = false;
        editor_mode = false;
        as_pp = false;
        test_ast_conversion = false;
        verbose = false;
      };
  }

type command =
  | Eval of string
  | Print_version
  | Warn_help
  | Format of string
  | Reprint_source of string
  | Help

(* ----- Values --------------------------------------------------------- *)

exception Invalid_value of string

let invalid message = raise_notrace (Invalid_value message)

let bool_value value =
  match String.lowercase_ascii value with
  | "true" -> true
  | "false" -> false
  | value -> invalid ("Expected true or false, got: " ^ value)

let string_of_bool_value = function
  | true -> "true"
  | false -> "false"

let source_map_value value =
  match String.lowercase_ascii value with
  | "linked" -> Js_config.Linked
  | "inline" -> Inline
  | "hidden" -> Hidden
  | "false" | "none" -> No_source_map
  | value ->
    invalid
      ("Unsupported sourceMap value: " ^ value
     ^ ". Expected linked, inline, hidden, false, or none")

let string_of_source_map = function
  | Js_config.Linked -> "linked"
  | Inline -> "inline"
  | Hidden -> "hidden"
  | No_source_map -> "false"

let jsx_version_value = function
  | "4" -> Js_config.Jsx_v4
  | value -> invalid ("Unsupported jsx version: " ^ value)

let string_of_jsx_version = function
  | Js_config.Jsx_v4 -> "4"

let color_value value =
  match Clflags.parse_color_setting value with
  | Some setting -> setting
  | None -> invalid ("Unsupported color setting: " ^ value)

let string_of_color = function
  | Misc.Color.Auto -> "auto"
  | Always -> "always"
  | Never -> "never"

let gentype_module_value value =
  match Gentype_config.module_of_string value with
  | Some module_ -> module_
  | None -> invalid ("Unsupported gentype module: " ^ value)

let string_of_gentype_module = function
  | Gentype_config.CommonJS -> "commonjs"
  | ESModule -> "esmodule"

let gentype_module_resolution_value value =
  match Gentype_config.module_resolution_of_string value with
  | Some resolution -> resolution
  | None -> invalid ("Unsupported gentype module resolution: " ^ value)

let string_of_gentype_module_resolution = function
  | Gentype_config.Node -> "node"
  | Node16 -> "node16"
  | Bundler -> "bundler"

let experimental_value value =
  match Experimental_features.from_string value with
  | Some feature -> feature
  | None -> invalid ("Unknown experimental feature: " ^ value)

(* "From=To" of a gentype shim, "name=path" of a dependency path *)
let pair_value value =
  match String.split_on_char '=' value with
  | [first; second] -> (first, second)
  | _ -> invalid ("Expected <name>=<value>, got: " ^ value)

let string_of_pair (first, second) = first ^ "=" ^ second

let package_output_value value =
  try Js_packages_info.package_info_of_string value
  with Bsc_args.Bad message -> invalid message

let warning_value ~as_error spec =
  (* Checked by the parser itself, on state restored afterwards. *)
  let state = Warnings.backup () in
  Fun.protect
    ~finally:(fun () -> Warnings.restore state)
    (fun () ->
      try Warnings.parse_options as_error spec
      with Arg.Bad message -> invalid message);
  {as_error; spec}

(* ----- The flag table ---------------------------------------------------- *)

type action =
  | Flag of (t -> t)
  | Value of (t -> string -> t)
  | Command_flag of command
  | Command_value of (string -> command)
  | No_op
  | No_op_value

type entry = {name: string; kind: kind; action: action; doc: string}

let flag name kind doc update = {name; kind; action = Flag update; doc}
let value name kind doc update = {name; kind; action = Value update; doc}

let warning_doc =
  "<list>  Enable or disable warnings according to <list>:\n\
   +<spec>   enable warnings in <spec>\n\
   -<spec>   disable warnings in <spec>\n\
   @<spec>   enable warnings in <spec> and treat them as errors\n\
   <spec> can be:\n\
   <num>             a single warning number\n\
   <num1>..<num2>    a range of consecutive warning numbers\n\
   default setting is " ^ Bsc_warnings.defaults_w

let with_codegen t f = {t with codegen = f t.codegen}
let with_stdlib t f = {t with stdlib = f t.stdlib}
let with_debug t f = {t with debug = f t.debug}
let with_gentype t f = {t with gentype = f t.gentype}
let with_jsx t f = {t with jsx = f t.jsx}
let with_source_map t f = {t with source_map = f t.source_map}

(* Most used first, as the table is searched in order. *)
let table =
  [
    value "-I" Build_system
      "*internal* <dir>  Add <dir> to the list of include directories"
      (fun t dir -> {t with include_dirs = t.include_dirs @ [dir]});
    value "-w" Public warning_doc (fun t spec ->
        {t with warnings = t.warnings @ [warning_value ~as_error:false spec]});
    value "-o" Build_system "*internal* <file>  set output file name to <file>"
      (fun t output -> {t with output = Some output});
    flag "-bs-read-cmi" Build_system "*internal* Assume mli always exist "
      (fun t -> {t with read_cmi = true});
    value "-ppx" Public
      "*internal* <command>  Pipe abstract syntax trees through preprocessor \
       <command>" (fun t command -> {t with ppx = t.ppx @ [command]});
    value "-open" Public
      "*internal* <module>  Opens the module <module> before typing"
      (fun t module_name ->
        {t with open_modules = t.open_modules @ [module_name]});
    value "-bs-jsx" Public "*internal* Set jsx version" (fun t version ->
        with_jsx t (fun jsx ->
            {jsx with version = Some (jsx_version_value version)}));
    value "-bs-jsx-module" Public "*internal* Set jsx module" (fun t module_ ->
        with_jsx t (fun jsx -> {jsx with module_ = Some module_}));
    {
      name = "-bs-jsx-mode";
      kind = Public;
      action = No_op_value;
      doc = "*internal* Set jsx mode, this is no longer used and is a no-op.";
    };
    flag "-bs-jsx-preserve" Public "*internal* Preserve jsx" (fun t ->
        with_jsx t (fun jsx -> {jsx with preserve = true}));
    value "-bs-source-map" Public "*internal* Configure source map output"
      (fun t mode ->
        with_source_map t (fun source_map ->
            {source_map with mode = Some (source_map_value mode)}));
    value "-bs-source-map-sources-content" Public
      "*internal* Include original source text in source maps" (fun t value ->
        with_source_map t (fun source_map ->
            {source_map with sources_content = Some (bool_value value)}));
    value "-bs-source-map-root" Public
      "*internal* Set sourceRoot in source maps" (fun t root ->
        with_source_map t (fun source_map -> {source_map with root = Some root}));
    value "-bs-package-output" Build_system
      "*internal* Set npm-output-path: [opt_module]:path, for example: \
       'lib/cjs', 'amdjs:lib/amdjs', 'es6:lib/es6' " (fun t output ->
        {
          t with
          package_outputs = t.package_outputs @ [package_output_value output];
        });
    value "-bs-project-root" Build_system
      "*internal* Set the project root directory" (fun t root ->
        {t with project_root = Some root});
    flag "-bs-ast" Build_system
      "*internal* Generate binary .mli_ast and ml_ast and stop" (fun t ->
        {t with ast_only = true});
    flag "-bs-test-ast-conversion" Debug
      "*internal* Roundtrip the parsed AST through Parsetree0 before continuing"
      (fun t ->
        with_debug t (fun debug -> {debug with test_ast_conversion = true}));
    flag "-bs-syntax-only" Debug "*internal* Only check syntax" (fun t ->
        with_debug t (fun debug -> {debug with syntax_only = true}));
    flag "-bs-g" Public "Debug mode" (fun t ->
        with_codegen t (fun codegen -> {codegen with debug = true}));
    value "-bs-package-name" Build_system
      "*internal* Set package name, useful when you want to produce npm \
       packages" (fun t name ->
        match t.package_name with
        | Some _ -> invalid "duplicated flag for -bs-package-name"
        | None -> {t with package_name = Some name});
    value "-bs-ns" Build_system
      "*internal* Set package map, not only set package name but also use it \
       as a namespace" (fun t namespace -> {t with namespace = Some namespace});
    flag "-as-pp" Debug "*internal*As pp to interact with native tools"
      (fun t -> with_debug t (fun debug -> {debug with as_pp = true}));
    flag "-no-alias-deps" Build_system
      "*internal*Do not record dependencies for module aliases" (fun t ->
        {t with transparent_modules = true});
    flag "-bs-gentype" Public "*internal* Pass gentype command" (fun t ->
        with_gentype t (fun gentype -> {gentype with enabled = true}));
    value "-bs-gentype-module" Build_system
      "*internal* Set gentype module system: commonjs|esmodule" (fun t value ->
        with_gentype t (fun gentype ->
            {gentype with module_ = Some (gentype_module_value value)}));
    value "-bs-gentype-module-resolution" Build_system
      "*internal* Set gentype module resolution strategy: node|node16|bundler"
      (fun t value ->
        with_gentype t (fun gentype ->
            {
              gentype with
              module_resolution = Some (gentype_module_resolution_value value);
            }));
    flag "-bs-gentype-export-interfaces" Build_system
      "*internal* Emit gentype interface files" (fun t ->
        with_gentype t (fun gentype -> {gentype with export_interfaces = true}));
    value "-bs-gentype-generated-extension" Build_system
      "*internal* Set gentype generated-file extension (e.g. .gen.tsx)"
      (fun t extension ->
        with_gentype t (fun gentype ->
            {gentype with generated_extension = Some extension}));
    value "-bs-gentype-suffix" Build_system
      "*internal* Set gentype import-path suffix (e.g. .bs.js, .mjs)"
      (fun t suffix ->
        with_gentype t (fun gentype -> {gentype with suffix = Some suffix}));
    value "-bs-gentype-shim" Build_system
      "*internal* Register a gentype shim mapping: From=To (repeatable)"
      (fun t shim ->
        with_gentype t (fun gentype ->
            {gentype with shims = gentype.shims @ [pair_value shim]}));
    value "-bs-gentype-debug" Build_system
      "*internal* Enable a gentype debug category (repeatable): \
       all|basic|codeItems|config|converter|dependencies|moduleResolution|notImplemented|translation|typeEnv|typeResolution"
      (fun t item ->
        with_gentype t (fun gentype ->
            {gentype with debug_items = gentype.debug_items @ [item]}));
    value "-bs-gentype-dep" Build_system
      "*internal* Register a gentype bsb dependency (repeatable)"
      (fun t dependency ->
        with_gentype t (fun gentype ->
            {gentype with dependencies = gentype.dependencies @ [dependency]}));
    value "-bs-gentype-source-dir" Build_system
      "*internal* Register a gentype source directory relative to the project \
       root (repeatable)" (fun t dir ->
        with_gentype t (fun gentype ->
            {gentype with source_dirs = gentype.source_dirs @ [dir]}));
    value "-bs-gentype-dep-path" Build_system
      "*internal* Register a gentype dependency install path: \
       <name>=<absolute-path> (repeatable)" (fun t dep_path ->
        with_gentype t (fun gentype ->
            {gentype with dep_paths = gentype.dep_paths @ [pair_value dep_path]}));
    value "-bs-gentype-bsb-project-root" Build_system
      "*internal* Set gentype bsb project root (workspace root containing \
       .sourcedirs.json)" (fun t root ->
        with_gentype t (fun gentype ->
            {gentype with bsb_project_root = Some root}));
    flag "-nostdlib" Public "*internal* Don't use stdlib" (fun t ->
        with_stdlib t (fun stdlib -> {stdlib with nostdlib = true}));
    value "-color" Build_system
      "*internal* Enable or disable colors in compiler messages\n\
       The following settings are supported:\n\
       auto    use heuristics to enable colors only if supported\n\
       always  enable colors\n\
       never   disable colors\n\
       The default setting is 'always'\n\
       The current heuristic for 'auto'\n\
       checks that the TERM environment variable exists and is\n\
       not empty or \"dumb\", and that isatty(stderr) holds." (fun t color ->
        {t with color = Some (color_value color)});
    {
      name = "-e";
      kind = Command;
      action = Command_value (fun source -> Eval source);
      doc = "(experimental) set the string to be evaluated in ReScript syntax";
    };
    flag "-bs-cmi-only" Debug "*internal* Stop after generating cmi file"
      (fun t -> with_debug t (fun debug -> {debug with cmi_only = true}));
    flag "-bs-cmi" Public
      "*internal*  Not using cached cmi, always generate cmi" (fun t ->
        with_codegen t (fun codegen -> {codegen with force_cmi = true}));
    flag "-bs-cmj" Public
      "*internal*  Not using cached cmj, always generate cmj" (fun t ->
        with_codegen t (fun codegen -> {codegen with force_cmj = true}));
    flag "-bs-no-version-header" Public "*internal*Don't print version header"
      (fun t ->
        with_codegen t (fun codegen -> {codegen with no_version_header = true}));
    flag "-bs-no-builtin-ppx" Public "*internal* Disable built-in ppx" (fun t ->
        with_codegen t (fun codegen -> {codegen with no_builtin_ppx = true}));
    flag "-bs-cross-module-opt" Public
      "*internal* Enable cross module inlining(experimental), default(false)"
      (fun t ->
        with_codegen t (fun codegen ->
            {codegen with cross_module_opt = Some true}));
    flag "-bs-no-cross-module-opt" Public
      "*internal* Disable cross module inlining(experimental)" (fun t ->
        with_codegen t (fun codegen ->
            {codegen with cross_module_opt = Some false}));
    flag "-debug-ir" Public
      "*internal* Dump compiler IR and enable Lambda invariant checks" (fun t ->
        with_codegen t (fun codegen -> {codegen with debug_ir = true}));
    flag "-check-lam" Public
      "*internal* Check Lambda invariants after optimization passes" (fun t ->
        with_codegen t (fun codegen -> {codegen with check_lam = true}));
    flag "-bs-no-check-div-by-zero" Public
      "*internal* unsafe mode, don't check div by zero and mod by zero"
      (fun t ->
        with_codegen t (fun codegen ->
            {codegen with no_check_div_by_zero = true}));
    flag "-bs-noassertfalse" Public "*internal*  no code for assert false"
      (fun t ->
        with_codegen t (fun codegen -> {codegen with no_assert_false = true}));
    flag "-noassert" Public "*internal* Do not compile assertion checks"
      (fun t -> with_codegen t (fun codegen -> {codegen with noassert = true}));
    flag "-bs-loc" Debug
      "*internal*  dont display location with -dtypedtree, -dparsetree"
      (fun t -> with_debug t (fun debug -> {debug with locations = true}));
    flag "-dtypedtree" Debug "*internal* debug typedtree" (fun t ->
        with_debug t (fun debug -> {debug with dtypedtree = true}));
    flag "-dparsetree" Debug "*internal* debug parsetree" (fun t ->
        with_debug t (fun debug -> {debug with dparsetree = true}));
    flag "-drawlambda" Debug "*internal* debug raw lambda" (fun t ->
        with_debug t (fun debug -> {debug with drawlambda = true}));
    flag "-draw-coercions" Debug
      "*internal* debug module coercions with raw internal types" (fun t ->
        with_debug t (fun debug -> {debug with draw_coercions = true}));
    flag "-dsource" Debug "*internal* print source" (fun t ->
        with_debug t (fun debug -> {debug with dsource = true}));
    {
      name = "-reprint-source";
      kind = Command;
      action = Command_value (fun file -> Reprint_source file);
      doc =
        "*internal* transform the target ReScript file using PPXes provided, \
         and print the transformed ReScript code to stdout";
    };
    {
      name = "-format";
      kind = Command;
      action = Command_value (fun file -> Format file);
      doc = "*internal* Format as Res syntax";
    };
    flag "-only-parse" Debug "*internal* stop after parsing" (fun t ->
        with_debug t (fun debug -> {debug with only_parse = true}));
    flag "-editor-mode" Debug "*internal* Enable editor mode." (fun t ->
        with_debug t (fun debug -> {debug with editor_mode = true}));
    flag "-ignore-parse-errors" Debug "*internal* continue after parse errors"
      (fun t ->
        with_debug t (fun debug -> {debug with ignore_parse_errors = true}));
    flag "-verbose" Debug "*internal* Print calls to external commands"
      (fun t -> with_debug t (fun debug -> {debug with verbose = true}));
    flag "-keep-locs" Public "*internal* Keep locations in .cmi files" (fun t ->
        with_codegen t (fun codegen -> {codegen with keep_locs = Some true}));
    flag "-no-keep-locs" Public "*internal* Do not keep locations in .cmi files"
      (fun t ->
        with_codegen t (fun codegen -> {codegen with keep_locs = Some false}));
    flag "-nopervasives" Public "*internal*" (fun t ->
        with_stdlib t (fun stdlib -> {stdlib with nopervasives = true}));
    {
      name = "-v";
      kind = Command;
      action = Command_flag Print_version;
      doc = "Print compiler version and location of standard library and exit";
    };
    {
      name = "-version";
      kind = Command;
      action = Command_flag Print_version;
      doc = "Print version and exit";
    };
    flag "-absname" Build_system
      "*internal* Show absolute filenames in error messages" (fun t ->
        {t with absname = true});
    value "-enable-experimental" Public
      "Enable experimental features: repeatable, e.g. -enable-experimental \
       LetUnwrap" (fun t feature ->
        {t with experimental = t.experimental @ [experimental_value feature]});
    flag "-bs-no-bin-annot" Public
      "*internal* Disable binary annotations (by default on)" (fun t ->
        with_codegen t (fun codegen -> {codegen with no_bin_annot = true}));
    flag "-short-paths" Public "*internal* Shorten paths in types" (fun t ->
        with_codegen t (fun codegen -> {codegen with short_paths = true}));
    {
      name = "-unsafe";
      kind = Public;
      action = No_op;
      doc = "*internal* No longer used and is a no-op.";
    };
    value "-runtime-path" Build_system
      "*internal* Set the path of the runtime package (@rescript/runtime)"
      (fun t path -> {t with runtime_path = Some path});
    {
      name = "-warn-help";
      kind = Command;
      action = Command_flag Warn_help;
      doc = "Show description of warning numbers";
    };
    value "-warn-error" Public
      ("<list>  Enable or disable error status for warnings according\n\
        to <list>.  See option -w for the syntax of <list>.\n\
        Default setting is " ^ Bsc_warnings.defaults_warn_error) (fun t spec ->
        {t with warnings = t.warnings @ [warning_value ~as_error:true spec]});
    flag "-make-runtime" Public "*internal* make runtime library" (fun t ->
        with_stdlib t (fun stdlib -> {stdlib with make_runtime = true}));
  ]

let find name = List.find_opt (fun entry -> entry.name = name) table

let flag_names = List.map (fun entry -> entry.name) table

let kind_of name = Option.map (fun entry -> entry.kind) (find name)

let takes_value name =
  match find name with
  | Some {action = Value _ | Command_value _ | No_op_value} -> Some true
  | Some {action = Flag _ | Command_flag _ | No_op} -> Some false
  | None -> None

(* ----- Reading flags ------------------------------------------------------- *)

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_flag_value of {flag: string; value: string; message: string}

type parsed = {request: t; commands: command list; arguments: string list}

let of_argv ?(base = empty) argv =
  let rec read parsed = function
    | [] ->
      Ok
        {
          parsed with
          commands = List.rev parsed.commands;
          arguments = List.rev parsed.arguments;
        }
    | ("-help" | "--help" | "-h") :: rest ->
      read {parsed with commands = Help :: parsed.commands} rest
    | argument :: rest when argument = "" || argument.[0] <> '-' ->
      read {parsed with arguments = argument :: parsed.arguments} rest
    | flag :: rest -> (
      match find flag with
      | None -> Error (Unknown_flag flag)
      | Some entry -> (
        match (entry.action, rest) with
        | (Value _ | Command_value _ | No_op_value), [] ->
          Error (Missing_value flag)
        | Flag update, rest ->
          read {parsed with request = update parsed.request} rest
        | No_op, rest -> read parsed rest
        | Command_flag command, rest ->
          read {parsed with commands = command :: parsed.commands} rest
        | No_op_value, _ :: rest -> read parsed rest
        | Command_value command, value :: rest ->
          read {parsed with commands = command value :: parsed.commands} rest
        | Value update, value :: rest -> (
          match update parsed.request value with
          | request -> read {parsed with request} rest
          | exception Invalid_value message ->
            Error (Invalid_flag_value {flag; value; message}))))
  in
  read {request = base; commands = []; arguments = []} argv

(* ----- Writing flags ------------------------------------------------------- *)

let to_argv t =
  let flag name enabled = if enabled then [name] else [] in
  let option name to_string = function
    | Some value -> [name; to_string value]
    | None -> []
  in
  let each name to_string values =
    List.concat_map (fun value -> [name; to_string value]) values
  in
  let pair_flag ~yes ~no = function
    | Some true -> [yes]
    | Some false -> [no]
    | None -> []
  in
  let {codegen; stdlib; debug; gentype; jsx; source_map} = t in
  List.concat
    [
      flag "-make-runtime" stdlib.make_runtime;
      option "-bs-package-name" Fun.id t.package_name;
      each "-bs-package-output" Js_packages_info.string_of_package_info
        t.package_outputs;
      option "-bs-ns" Fun.id t.namespace;
      each "-I" Fun.id t.include_dirs;
      each "-open" Fun.id t.open_modules;
      List.concat_map
        (fun {as_error; spec} ->
          [(if as_error then "-warn-error" else "-w"); spec])
        t.warnings;
      each "-ppx" Fun.id t.ppx;
      option "-bs-jsx" string_of_jsx_version jsx.version;
      option "-bs-jsx-module" Fun.id jsx.module_;
      flag "-bs-jsx-preserve" jsx.preserve;
      option "-bs-source-map" string_of_source_map source_map.mode;
      option "-bs-source-map-sources-content" string_of_bool_value
        source_map.sources_content;
      option "-bs-source-map-root" Fun.id source_map.root;
      each "-enable-experimental" Experimental_features.to_string t.experimental;
      option "-bs-project-root" Fun.id t.project_root;
      option "-runtime-path" Fun.id t.runtime_path;
      option "-o" Fun.id t.output;
      flag "-bs-read-cmi" t.read_cmi;
      flag "-bs-ast" t.ast_only;
      flag "-no-alias-deps" t.transparent_modules;
      flag "-absname" t.absname;
      option "-color" string_of_color t.color;
      flag "-bs-gentype" gentype.enabled;
      option "-bs-gentype-module" string_of_gentype_module gentype.module_;
      option "-bs-gentype-module-resolution" string_of_gentype_module_resolution
        gentype.module_resolution;
      flag "-bs-gentype-export-interfaces" gentype.export_interfaces;
      option "-bs-gentype-generated-extension" Fun.id
        gentype.generated_extension;
      option "-bs-gentype-suffix" Fun.id gentype.suffix;
      each "-bs-gentype-shim" string_of_pair gentype.shims;
      each "-bs-gentype-debug" Fun.id gentype.debug_items;
      each "-bs-gentype-dep" Fun.id gentype.dependencies;
      each "-bs-gentype-source-dir" Fun.id gentype.source_dirs;
      each "-bs-gentype-dep-path" string_of_pair gentype.dep_paths;
      option "-bs-gentype-bsb-project-root" Fun.id gentype.bsb_project_root;
      flag "-bs-g" codegen.debug;
      flag "-bs-no-version-header" codegen.no_version_header;
      pair_flag ~yes:"-bs-cross-module-opt" ~no:"-bs-no-cross-module-opt"
        codegen.cross_module_opt;
      flag "-bs-no-check-div-by-zero" codegen.no_check_div_by_zero;
      flag "-bs-noassertfalse" codegen.no_assert_false;
      flag "-noassert" codegen.noassert;
      flag "-bs-no-bin-annot" codegen.no_bin_annot;
      pair_flag ~yes:"-keep-locs" ~no:"-no-keep-locs" codegen.keep_locs;
      flag "-short-paths" codegen.short_paths;
      flag "-bs-no-builtin-ppx" codegen.no_builtin_ppx;
      flag "-bs-cmi" codegen.force_cmi;
      flag "-bs-cmj" codegen.force_cmj;
      flag "-check-lam" codegen.check_lam;
      flag "-debug-ir" codegen.debug_ir;
      flag "-nostdlib" stdlib.nostdlib;
      flag "-nopervasives" stdlib.nopervasives;
      flag "-dparsetree" debug.dparsetree;
      flag "-dtypedtree" debug.dtypedtree;
      flag "-drawlambda" debug.drawlambda;
      flag "-draw-coercions" debug.draw_coercions;
      flag "-dsource" debug.dsource;
      flag "-bs-loc" debug.locations;
      flag "-only-parse" debug.only_parse;
      flag "-bs-syntax-only" debug.syntax_only;
      flag "-bs-cmi-only" debug.cmi_only;
      flag "-ignore-parse-errors" debug.ignore_parse_errors;
      flag "-editor-mode" debug.editor_mode;
      flag "-as-pp" debug.as_pp;
      flag "-bs-test-ast-conversion" debug.test_ast_conversion;
      flag "-verbose" debug.verbose;
    ]

(* ----- Applying a request -------------------------------------------------- *)

let absname_key = Request_local.new_key (fun () -> ref false)
let absname () = Domain.DLS.get absname_key

(* Applies [t] on top of the request's state, in the order of [to_argv]: the
   runtime and the package name before the package outputs, the namespace
   before the other opened modules. *)
let apply t =
  let clflags = Clflags.current () in
  let js_config = Js_config.current () in
  let set flag target = if flag then target := true in
  let set_option value target =
    Option.iter (fun value -> target := value) value
  in
  let prepend values target =
    List.iter (fun value -> target := value :: !target) values
  in
  let {codegen; stdlib; debug; gentype; jsx; source_map} = t in
  if stdlib.make_runtime then Js_packages_state.make_runtime ();
  Option.iter Js_packages_state.set_package_name t.package_name;
  List.iter Js_packages_state.add_package_output t.package_outputs;
  Option.iter Js_packages_state.set_package_map t.namespace;
  prepend t.include_dirs clflags.include_dirs;
  prepend t.open_modules clflags.open_modules;
  List.iter
    (fun {as_error; spec} -> Warnings.parse_options as_error spec)
    t.warnings;
  prepend t.ppx clflags.all_ppx;
  Option.iter (fun version -> js_config.jsx_version := Some version) jsx.version;
  Option.iter
    (fun module_ ->
      js_config.jsx_module := Js_config.jsx_module_of_string module_;
      (* A JSX module other than React implies JSX 4. *)
      if String.lowercase_ascii module_ <> "react" then
        js_config.jsx_version := Some Jsx_v4)
    jsx.module_;
  set jsx.preserve clflags.jsx_preserve;
  set_option source_map.mode js_config.source_map;
  set_option source_map.sources_content js_config.source_map_sources_content;
  set_option source_map.root js_config.source_map_root;
  List.iter
    (fun feature ->
      Experimental_features.enable_from_string
        (Experimental_features.to_string feature))
    t.experimental;
  Option.iter
    (fun root ->
      (* Writers open paths under the root without resolving them against the
         request's working directory, so make it absolute here. *)
      let root = Ext_path.absolute_cwd_path root in
      Ext_path.set_project_root root;
      Gentype_config.project_root () := root)
    t.project_root;
  Option.iter Runtime_package.set_path t.runtime_path;
  set_option (Option.map Option.some t.output) clflags.output_name;
  if t.read_cmi then clflags.assume_no_mli := Mli_exists;
  if t.ast_only then (
    js_config.binary_ast := true;
    js_config.syntax_only := true);
  set t.transparent_modules clflags.transparent_modules;
  set t.absname (absname ());
  set_option (Option.map Option.some t.color) clflags.color;
  set gentype.enabled clflags.bs_gentype;
  set_option
    (Option.map Option.some gentype.module_)
    (Gentype_config.module_flag ());
  set_option
    (Option.map Option.some gentype.module_resolution)
    (Gentype_config.module_resolution_flag ());
  set gentype.export_interfaces (Gentype_config.export_interfaces_flag ());
  set_option
    (Option.map Option.some gentype.generated_extension)
    (Gentype_config.generated_file_extension_flag ());
  set_option
    (Option.map Option.some gentype.suffix)
    (Gentype_config.suffix_flag ());
  List.iter
    (fun shim -> Gentype_config.add_shim (string_of_pair shim))
    gentype.shims;
  List.iter Debug.set_item gentype.debug_items;
  List.iter Gentype_config.add_bs_dependency gentype.dependencies;
  List.iter Gentype_config.add_source_dir gentype.source_dirs;
  List.iter
    (fun dep_path -> Gentype_config.add_dep_path (string_of_pair dep_path))
    gentype.dep_paths;
  set_option gentype.bsb_project_root (Gentype_config.bsb_project_root ());
  set codegen.debug js_config.debug;
  set codegen.no_version_header js_config.no_version_header;
  set_option codegen.cross_module_opt js_config.cross_module_inline;
  if codegen.no_check_div_by_zero then js_config.check_div_by_zero := false;
  set codegen.no_assert_false clflags.no_assert_false;
  set codegen.noassert clflags.noassert;
  if codegen.no_bin_annot then clflags.binary_annotations := false;
  set_option codegen.keep_locs clflags.keep_locs;
  if codegen.short_paths then clflags.real_paths := false;
  set codegen.no_builtin_ppx js_config.no_builtin_ppx;
  set codegen.force_cmi js_config.force_cmi;
  set codegen.force_cmj js_config.force_cmj;
  set codegen.check_lam js_config.check_lam;
  set codegen.debug_ir js_config.debug_ir;
  set stdlib.nostdlib js_config.no_stdlib;
  set stdlib.nopervasives clflags.nopervasives;
  set debug.dparsetree clflags.dump_parsetree;
  set debug.dtypedtree clflags.dump_typedtree;
  set debug.drawlambda clflags.dump_rawlambda;
  set debug.draw_coercions clflags.dump_coercions;
  set debug.dsource clflags.dump_source;
  set debug.locations clflags.dump_location;
  set debug.only_parse clflags.only_parse;
  set debug.syntax_only js_config.syntax_only;
  set debug.cmi_only js_config.cmi_only;
  set debug.ignore_parse_errors clflags.ignore_parse_errors;
  if debug.editor_mode then (
    clflags.ignore_parse_errors := true;
    js_config.cmi_only := true);
  if debug.as_pp then (
    js_config.as_pp := true;
    js_config.syntax_only := true);
  set debug.test_ast_conversion js_config.test_ast_conversion;
  set debug.verbose clflags.verbose

(* ----- Usage --------------------------------------------------------------- *)

let usage_entries () =
  table |> List.map (fun entry -> (entry.name, entry.doc)) |> Array.of_list
