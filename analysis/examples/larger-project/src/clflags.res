/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* Command-line parameters */

module Int_arg_helper = {}
module Float_arg_helper = {

}

let objfiles = ref((list{}: list<string>)) /* .cmo and .cma files */
and ccobjs = ref((list{}: list<string>)) /* .o, .a, .so and -cclib -lxxx */
and dllibs = ref((list{}: list<string>)) /* .so and -dllib -lxxx */

let compile_only = ref(false) /* -c */
and output_name = ref((None: option<string>)) /* -o */
and include_dirs = ref((list{}: list<string>)) /* -I */
and no_std_include = ref(false) /* -nostdlib */
and print_types = ref(false) /* -i */
and make_archive = ref(false) /* -a */
and debug = ref(false) /* -g */
and fast = ref(false) /* -unsafe */
and use_linscan = ref(false) /* -linscan */
and link_everything = ref(false) /* -linkall */
and custom_runtime = ref(false) /* -custom */
and no_check_prims = ref(false) /* -no-check-prims */
and bytecode_compatible_32 = ref(false) /* -compat-32 */
and output_c_object = ref(false) /* -output-obj */
and output_complete_object = ref(false) /* -output-complete-obj */
and all_ccopts = ref((list{}: list<string>)) /* -ccopt */
and classic = ref(false) /* -nolabels */
and nopervasives = ref(false) /* -nopervasives */
and preprocessor = ref((None: option<string>)) /* -pp */
and all_ppx = ref((list{}: list<string>)) /* -ppx */
let annotations = ref(false) /* -annot */
let binary_annotations = ref(false) /* -annot */
and use_threads = ref(false) /* -thread */
and use_vmthreads = ref(false) /* -vmthread */
and noassert = ref(false) /* -noassert */
and verbose = ref(false) /* -verbose */
and noversion = ref(false) /* -no-version */
and noprompt = ref(false) /* -noprompt */
and nopromptcont = ref(false) /* -nopromptcont */
and init_file = ref((None: option<string>)) /* -init */
and noinit = ref(false) /* -noinit */
and open_modules: ref<list<string>> = ref(list{}) /* -open */
and use_prims = ref("") /* -use-prims ... */
and use_runtime = ref("") /* -use-runtime ... */
and principal = ref(false) /* -principal */
and real_paths = ref(true) /* -short-paths */
and recursive_types = ref(false) /* -rectypes */
and strict_sequence = ref(false) /* -strict-sequence */
and strict_formats = ref(false) /* -strict-formats */
and applicative_functors = ref(true) /* -no-app-funct */
and make_runtime = ref(false) /* -make-runtime */
and gprofile = ref(false) /* -p */
and c_compiler = ref((None: option<string>)) /* -cc */
and no_auto_link = ref(false) /* -noautolink */
and dllpaths = ref((list{}: list<string>)) /* -dllpath */
and make_package = ref(false) /* -pack */
and for_package = ref((None: option<string>)) /* -for-pack */
and error_size = ref(500) /* -error-size */
and float_const_prop = ref(true) /* -no-float-const-prop */
and transparent_modules = ref(false) /* -trans-mod */
let dump_source = ref(false) /* -dsource */
let dump_parsetree = ref(false) /* -dparsetree */
and dump_typedtree = ref(false) /* -dtypedtree */
and dump_rawlambda = ref(false) /* -drawlambda */
and dump_lambda = ref(false) /* -dlambda */
and dump_rawclambda = ref(false) /* -drawclambda */
and dump_clambda = ref(false) /* -dclambda */
and dump_rawflambda = ref(false) /* -drawflambda */
and dump_flambda = ref(false) /* -dflambda */
and dump_flambda_let = ref((None: option<int>)) /* -dflambda-let=... */
and dump_flambda_verbose = ref(false) /* -dflambda-verbose */
and dump_instr = ref(false) /* -dinstr */

let keep_asm_file = ref(false) /* -S */
let optimize_for_speed = ref(true) /* -compact */
and opaque = ref(false) /* -opaque */

and dump_cmm = ref(false) /* -dcmm */
let dump_selection = ref(false) /* -dsel */
let dump_cse = ref(false) /* -dcse */
let dump_live = ref(false) /* -dlive */
let dump_avail = ref(false) /* -davail */
let dump_spill = ref(false) /* -dspill */
let dump_split = ref(false) /* -dsplit */
let dump_interf = ref(false) /* -dinterf */
let dump_prefer = ref(false) /* -dprefer */
let dump_regalloc = ref(false) /* -dalloc */
let dump_reload = ref(false) /* -dreload */
let dump_scheduling = ref(false) /* -dscheduling */
let dump_linear = ref(false) /* -dlinear */
let dump_interval = ref(false) /* -dinterval */
let keep_startup_file = ref(false) /* -dstartup */
let dump_combine = ref(false) /* -dcombine */

let debug_runavail = ref(false) /* -drunavail */

let native_code = ref(false) /* set to true under ocamlopt */

let force_slash = ref(false) /* for ocamldep */
let clambda_checks = ref(false) /* -clambda-checks */

let flambda_invariant_checks = ref(true) /* -flambda-invariants */

let dont_write_files = ref(false) /* set to true under ocamldoc */

let std_include_flag = prefix =>
  if no_std_include.contents {
    ""
  } else {
    prefix ++ Filename.quote(Config.standard_library)
  }

let std_include_dir = () =>
  if no_std_include.contents {
    list{}
  } else {
    list{Config.standard_library}
  }

let shared = ref(false) /* -shared */
let dlcode = ref(true) /* not -nodynlink */

let pic_code = ref(
  switch Config.architecture {
  /* -fPIC */
  | "amd64" => true
  | _ => false
  },
)

let runtime_variant = ref("") /* -runtime-variant */

let keep_docs = ref(false) /* -keep-docs */
let keep_locs = ref(true) /* -keep-locs */
let unsafe_string = if Config.safe_string {
  ref(false)
} else {
  ref(!Config.default_safe_string)
}
/* -safe-string / -unsafe-string */

let classic_inlining = ref(false) /* -Oclassic */
let inlining_report = ref(false) /* -inlining-report */

let afl_instrument = ref(Config.afl_instrument) /* -afl-instrument */
let afl_inst_ratio = ref(100) /* -afl-inst-ratio */

let simplify_rounds = ref(None) /* -rounds */
let default_simplify_rounds = ref(1) /* -rounds */
let rounds = () =>
  switch simplify_rounds.contents {
  | None => default_simplify_rounds.contents
  | Some(r) => r
  }

let default_inline_threshold = if Config.flambda {
  10.
} else {
  10. /. 8.
}
let inline_toplevel_multiplier = 16
let default_inline_toplevel_threshold = int_of_float(
  float(inline_toplevel_multiplier) *. default_inline_threshold,
)
let default_inline_call_cost = 5
let default_inline_alloc_cost = 7
let default_inline_prim_cost = 3
let default_inline_branch_cost = 5
let default_inline_indirect_cost = 4
let default_inline_branch_factor = 0.1
let default_inline_lifting_benefit = 1300
let default_inline_max_unroll = 0
let default_inline_max_depth = 1

let unbox_specialised_args = ref(true) /* -no-unbox-specialised-args */
let unbox_free_vars_of_closures = ref(true)
let unbox_closures = ref(false) /* -unbox-closures */
let default_unbox_closures_factor = 10
let unbox_closures_factor = ref(default_unbox_closures_factor) /* -unbox-closures-factor */
let remove_unused_arguments = ref(false) /* -remove-unused-arguments */

type inlining_arguments = {
  inline_call_cost: option<int>,
  inline_alloc_cost: option<int>,
  inline_prim_cost: option<int>,
  inline_branch_cost: option<int>,
  inline_indirect_cost: option<int>,
  inline_lifting_benefit: option<int>,
  inline_branch_factor: option<float>,
  inline_max_depth: option<int>,
  inline_max_unroll: option<int>,
  inline_threshold: option<float>,
  inline_toplevel_threshold: option<int>,
}

/* o1 is the default */
let o1_arguments = {
  inline_call_cost: None,
  inline_alloc_cost: None,
  inline_prim_cost: None,
  inline_branch_cost: None,
  inline_indirect_cost: None,
  inline_lifting_benefit: None,
  inline_branch_factor: None,
  inline_max_depth: None,
  inline_max_unroll: None,
  inline_threshold: None,
  inline_toplevel_threshold: None,
}

let classic_arguments = {
  inline_call_cost: None,
  inline_alloc_cost: None,
  inline_prim_cost: None,
  inline_branch_cost: None,
  inline_indirect_cost: None,
  inline_lifting_benefit: None,
  inline_branch_factor: None,
  inline_max_depth: None,
  inline_max_unroll: None,
  /* [inline_threshold] matches the current compiler's default.
     Note that this particular fraction can be expressed exactly in
     floating point. */
  inline_threshold: Some(10. /. 8.),
  /* [inline_toplevel_threshold] is not used in classic mode. */
  inline_toplevel_threshold: Some(1),
}

let o2_arguments = {
  inline_call_cost: Some(2 * default_inline_call_cost),
  inline_alloc_cost: Some(2 * default_inline_alloc_cost),
  inline_prim_cost: Some(2 * default_inline_prim_cost),
  inline_branch_cost: Some(2 * default_inline_branch_cost),
  inline_indirect_cost: Some(2 * default_inline_indirect_cost),
  inline_lifting_benefit: None,
  inline_branch_factor: None,
  inline_max_depth: Some(2),
  inline_max_unroll: None,
  inline_threshold: Some(25.),
  inline_toplevel_threshold: Some(25 * inline_toplevel_multiplier),
}

let o3_arguments = {
  inline_call_cost: Some(3 * default_inline_call_cost),
  inline_alloc_cost: Some(3 * default_inline_alloc_cost),
  inline_prim_cost: Some(3 * default_inline_prim_cost),
  inline_branch_cost: Some(3 * default_inline_branch_cost),
  inline_indirect_cost: Some(3 * default_inline_indirect_cost),
  inline_lifting_benefit: None,
  inline_branch_factor: Some(0.),
  inline_max_depth: Some(3),
  inline_max_unroll: Some(1),
  inline_threshold: Some(50.),
  inline_toplevel_threshold: Some(50 * inline_toplevel_multiplier),
}

let all_passes: ref<list<string>> = ref(list{})
let dumped_passes_list = ref(list{})
let dumped_pass = s => {
  assert List.mem(s, all_passes.contents)
  List.mem(s, dumped_passes_list.contents)
}

let set_dumped_pass = (s, enabled) =>
  if List.mem(s, all_passes.contents) {
    let passes_without_s = List.filter(\"<>"(s), dumped_passes_list.contents)
    let dumped_passes = if enabled {
      list{s, ...passes_without_s}
    } else {
      passes_without_s
    }

    dumped_passes_list := dumped_passes
  }

let parse_color_setting = x =>
  switch x {
  | "auto" => Some(Misc.Color.Auto)
  | "always" => Some(Misc.Color.Always)
  | "never" => Some(Misc.Color.Never)
  | _ => None
  }
let color: ref<option<string>> = ref(None) /* -color */

let unboxed_types = ref(false)

let arg_spec = ref(list{})
let arg_names : ref<Misc.StringMap.t<int>> = ref(Misc.StringMap.empty)

let reset_arguments = () => {
  arg_spec := list{}
  arg_names := Misc.StringMap.empty
}

let add_arguments = (loc, args) => List.iter(x =>
    switch x {
    | (arg_name, _, _) as arg =>
      try {
        let loc2 = Misc.StringMap.find(arg_name, arg_names.contents)
        Printf.eprintf("Warning: plugin argument %s is already defined:\n", arg_name)
        Printf.eprintf("   First definition: %s\n", loc2)
        Printf.eprintf("   New definition: %s\n", loc)
      } catch {
      | Not_found =>
        arg_spec := \"@"(arg_spec.contents, list{arg})
        arg_names := Misc.StringMap.add(arg_name, loc, arg_names.contents)
      }
    }
  , args)

let print_arguments = usage => Arg.usage(arg_spec.contents, usage)

/* This function is almost the same as [Arg.parse_expand], except
   that [Arg.parse_expand] could not be used because it does not take a
   reference for [arg_spec].*/
@raises(exit)
let parse_arguments = (f, msg) =>
  try {
    let argv = ref(Sys.argv)
    let current = ref(Arg.current.contents)
    Arg.parse_and_expand_argv_dynamic(current, argv, arg_spec, f, msg)
  } catch {
  | Arg.Bad(msg) =>
    Printf.eprintf("%s", msg)
    exit(2)
  | Arg.Help(msg) =>
    Printf.printf("%s", msg)
    exit(0)
  }

