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

/* The main OCaml version string has moved to ../VERSION */
let version = Sys.ocaml_version

let standard_library_default = "/usr/local/lib/ocaml"

let standard_library = try Sys.getenv("OCAMLLIB") catch {
| Not_found =>
  try Sys.getenv("CAMLLIB") catch {
  | Not_found => standard_library_default
  }
}

let standard_runtime = "/usr/local/bin/ocamlrun"
let ccomp_type = "cc"
let c_compiler = "gcc"
let c_output_obj = "-o "
let ocamlc_cflags = "-O2 -fno-strict-aliasing -fwrapv "
let ocamlc_cppflags = "-D_FILE_OFFSET_BITS=64 -D_REENTRANT"
let ocamlopt_cflags = "-O2 -fno-strict-aliasing -fwrapv"
let ocamlopt_cppflags = "-D_FILE_OFFSET_BITS=64 -D_REENTRANT"
let bytecomp_c_libraries = "-lpthread                  "
/* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*/
let bytecomp_c_compiler = c_compiler ++ (" " ++ (ocamlc_cflags ++ (" " ++ ocamlc_cppflags)))
let native_c_compiler = c_compiler ++ (" " ++ (ocamlopt_cflags ++ (" " ++ ocamlopt_cppflags)))
let native_c_libraries = ""
let native_pack_linker = "ld -r -arch x86_64 -o\ "
let ranlib = "ranlib"
let ar = "ar"
let cc_profile = "-pg"
let (mkdll, mkexe, mkmaindll) = /* @@DRA Cygwin - but only if shared libraries are enabled, which we
 should be able to detect? */
if Sys.os_type == "Win32" {
  try {
    @raises(Invalid_argument)
    let flexlink = {
      let flexlink = Sys.getenv("OCAML_FLEXLINK")

      @raises(Invalid_argument)
      let f = i => {
        let c = String.get(flexlink, i)
        if c == '/' {
          '\\'
        } else {
          c
        }
      }
      String.init(String.length(flexlink), f) ++ " "
    }
    (flexlink, flexlink ++ " -exe", flexlink ++ " -maindll")
  } catch {
  | Not_found => (
      "gcc -shared -flat_namespace -undefined suppress                    -Wl,-no_compact_unwind",
      "gcc -O2 -fno-strict-aliasing -fwrapv -Wall -Werror -D_FILE_OFFSET_BITS=64 -D_REENTRANT -DCAML_NAME_SPACE   -Wl,-no_compact_unwind",
      "gcc -shared -flat_namespace -undefined suppress                    -Wl,-no_compact_unwind",
    )
  }
} else {
  (
    "gcc -shared -flat_namespace -undefined suppress                    -Wl,-no_compact_unwind",
    "gcc -O2 -fno-strict-aliasing -fwrapv -Wall -Werror -D_FILE_OFFSET_BITS=64 -D_REENTRANT -DCAML_NAME_SPACE   -Wl,-no_compact_unwind",
    "gcc -shared -flat_namespace -undefined suppress                    -Wl,-no_compact_unwind",
  )
}

let profiling = true
let flambda = false
let safe_string = false
let default_safe_string = true
let windows_unicode = 0 !== 0

let flat_float_array = true

let afl_instrument = false

let exec_magic_number = "Caml1999X011"
and cmi_magic_number = "Caml1999I022"
and cmo_magic_number = "Caml1999O022"
and cma_magic_number = "Caml1999A022"
and cmx_magic_number = if flambda {
  "Caml1999y022"
} else {
  "Caml1999Y022"
}
and cmxa_magic_number = if flambda {
  "Caml1999z022"
} else {
  "Caml1999Z022"
}
and ast_impl_magic_number = "Caml1999M022"
and ast_intf_magic_number = "Caml1999N022"
and cmxs_magic_number = "Caml1999D022"
/* cmxs_magic_number is duplicated in otherlibs/dynlink/natdynlink.ml */
and cmt_magic_number = "Caml1999T022"

let load_path = ref((list{}: list<string>))

let interface_suffix = ref(".mli")

let max_tag = 245
/* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. */
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 /* see byterun/config.h */
let stack_safety_margin = 60

let architecture = "amd64"
let model = "default"
let system = "macosx"

let asm = "clang -arch x86_64 -Wno-trigraphs -c"
let asm_cfi_supported = true
let with_frame_pointers = false
let spacetime = false
let enable_call_counts = true
let libunwind_available = false
let libunwind_link_flags = ""
let profinfo = false
let profinfo_width = 0

let ext_exe = ""
let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
let ext_dll = ".so"

let host = "x86_64-apple-darwin21.4.0"
let target = "x86_64-apple-darwin21.4.0"

let default_executable_name = switch Sys.os_type {
| "Unix" => "a.out"
| "Win32" | "Cygwin" => "camlprog.exe"
| _ => "camlprog"
}

let systhread_supported = true

let flexdll_dirs = list{}

let print_config = oc => {
  let p = (name, valu) => Printf.fprintf(oc, "%s: %s\n", name, valu)
  let p_int = (name, valu) => Printf.fprintf(oc, "%s: %d\n", name, valu)
  let p_bool = (name, valu) => Printf.fprintf(oc, "%s: %B\n", name, valu)
  p("version", version)
  p("standard_library_default", standard_library_default)
  p("standard_library", standard_library)
  p("standard_runtime", standard_runtime)
  p("ccomp_type", ccomp_type)
  p("c_compiler", c_compiler)
  p("ocamlc_cflags", ocamlc_cflags)
  p("ocamlc_cppflags", ocamlc_cppflags)
  p("ocamlopt_cflags", ocamlopt_cflags)
  p("ocamlopt_cppflags", ocamlopt_cppflags)
  p("bytecomp_c_compiler", bytecomp_c_compiler)
  p("native_c_compiler", native_c_compiler)
  p("bytecomp_c_libraries", bytecomp_c_libraries)
  p("native_c_libraries", native_c_libraries)
  p("native_pack_linker", native_pack_linker)
  p("ranlib", ranlib)
  p("cc_profile", cc_profile)
  p("architecture", architecture)
  p("model", model)
  p_int("int_size", Sys.int_size)
  p_int("word_size", Sys.word_size)
  p("system", system)
  p("asm", asm)
  p_bool("asm_cfi_supported", asm_cfi_supported)
  p_bool("with_frame_pointers", with_frame_pointers)
  p("ext_exe", ext_exe)
  p("ext_obj", ext_obj)
  p("ext_asm", ext_asm)
  p("ext_lib", ext_lib)
  p("ext_dll", ext_dll)
  p("os_type", Sys.os_type)
  p("default_executable_name", default_executable_name)
  p_bool("systhread_supported", systhread_supported)
  p("host", host)
  p("target", target)
  p_bool("profiling", profiling)
  p_bool("flambda", flambda)
  p_bool("spacetime", spacetime)
  p_bool("safe_string", safe_string)
  p_bool("default_safe_string", default_safe_string)
  p_bool("flat_float_array", flat_float_array)
  p_bool("afl_instrument", afl_instrument)
  p_bool("windows_unicode", windows_unicode)

  /* print the magic number */
  p("exec_magic_number", exec_magic_number)
  p("cmi_magic_number", cmi_magic_number)
  p("cmo_magic_number", cmo_magic_number)
  p("cma_magic_number", cma_magic_number)
  p("cmx_magic_number", cmx_magic_number)
  p("cmxa_magic_number", cmxa_magic_number)
  p("ast_impl_magic_number", ast_impl_magic_number)
  p("ast_intf_magic_number", ast_intf_magic_number)
  p("cmxs_magic_number", cmxs_magic_number)
  p("cmt_magic_number", cmt_magic_number)

}
