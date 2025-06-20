/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Pierre Weis && Damien Doligez, INRIA Rocquencourt */
/*  */
/* Copyright 1998 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* When you change this, you need to update the documentation:
   - man/ocamlc.m
   - man/ocamlopt.m
   - manual/manual/cmds/comp.etex
   - manual/manual/cmds/native.etex
*/

type loc = {
  loc_start: Lexing.position,
  loc_end: Lexing.position,
  loc_ghost: bool,
}

type t =
  | Comment_start /* 1 */
  | Comment_not_end /* 2 */
  | Deprecated(string, loc, loc) /* 3 */
  | Fragile_match(string) /* 4 */
  | Partial_application /* 5 */
  | Labels_omitted(list<string>) /* 6 */
  | Method_override(list<string>) /* 7 */
  | Partial_match(string) /* 8 */
  | Non_closed_record_pattern(string) /* 9 */
  | Statement_type /* 10 */
  | Unused_match /* 11 */
  | Unused_pat /* 12 */
  | Instance_variable_override(list<string>) /* 13 */
  | Illegal_backslash /* 14 */
  | Implicit_public_methods(list<string>) /* 15 */
  | Unerasable_optional_argument /* 16 */
  | Undeclared_virtual_method(string) /* 17 */
  | Not_principal(string) /* 18 */
  | Without_principality(string) /* 19 */
  | Unused_argument /* 20 */
  | Nonreturning_statement /* 21 */
  | Preprocessor(string) /* 22 */
  | Useless_record_with /* 23 */
  | Bad_module_name(string) /* 24 */
  | All_clauses_guarded /* 8, used to be 25 */
  | Unused_var(string) /* 26 */
  | Unused_var_strict(string) /* 27 */
  | Wildcard_arg_to_constant_constr /* 28 */
  | Eol_in_string /* 29 */
  | Duplicate_definitions(string, string, string, string) /* 30 */
  | Multiple_definition(string, string, string) /* 31 */
  | Unused_value_declaration(string) /* 32 */
  | Unused_open(string) /* 33 */
  | Unused_type_declaration(string) /* 34 */
  | Unused_for_index(string) /* 35 */
  | Unused_ancestor(string) /* 36 */
  | Unused_constructor(string, bool, bool) /* 37 */
  | Unused_extension(string, bool, bool, bool) /* 38 */
  | Unused_rec_flag /* 39 */
  | Name_out_of_scope(string, list<string>, bool) /* 40 */
  | Ambiguous_name(list<string>, list<string>, bool) /* 41 */
  | Disambiguated_name(string) /* 42 */
  | Nonoptional_label(string) /* 43 */
  | Open_shadow_identifier(string, string) /* 44 */
  | Open_shadow_label_constructor(string, string) /* 45 */
  | Bad_env_variable(string, string) /* 46 */
  | Attribute_payload(string, string) /* 47 */
  | Eliminated_optional_arguments(list<string>) /* 48 */
  | No_cmi_file(string, option<string>) /* 49 */
  | Bad_docstring(bool) /* 50 */
  | Expect_tailcall /* 51 */
  | Fragile_literal_pattern /* 52 */
  | Misplaced_attribute(string) /* 53 */
  | Duplicated_attribute(string) /* 54 */
  | Inlining_impossible(string) /* 55 */
  | Unreachable_case /* 56 */
  | Ambiguous_pattern(list<string>) /* 57 */
  | No_cmx_file(string) /* 58 */
  | Assignment_to_non_mutable_value /* 59 */
  | Unused_module(string) /* 60 */
  | Unboxable_type_in_prim_decl(string) /* 61 */
  | Constraint_on_gadt /* 62 */

/* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*/

let number = x =>
  switch x {
  | Comment_start => 1
  | Comment_not_end => 2
  | Deprecated(_) => 3
  | Fragile_match(_) => 4
  | Partial_application => 5
  | Labels_omitted(_) => 6
  | Method_override(_) => 7
  | Partial_match(_) => 8
  | Non_closed_record_pattern(_) => 9
  | Statement_type => 10
  | Unused_match => 11
  | Unused_pat => 12
  | Instance_variable_override(_) => 13
  | Illegal_backslash => 14
  | Implicit_public_methods(_) => 15
  | Unerasable_optional_argument => 16
  | Undeclared_virtual_method(_) => 17
  | Not_principal(_) => 18
  | Without_principality(_) => 19
  | Unused_argument => 20
  | Nonreturning_statement => 21
  | Preprocessor(_) => 22
  | Useless_record_with => 23
  | Bad_module_name(_) => 24
  | All_clauses_guarded => 8 /* used to be 25 */
  | Unused_var(_) => 26
  | Unused_var_strict(_) => 27
  | Wildcard_arg_to_constant_constr => 28
  | Eol_in_string => 29
  | Duplicate_definitions(_) => 30
  | Multiple_definition(_) => 31
  | Unused_value_declaration(_) => 32
  | Unused_open(_) => 33
  | Unused_type_declaration(_) => 34
  | Unused_for_index(_) => 35
  | Unused_ancestor(_) => 36
  | Unused_constructor(_) => 37
  | Unused_extension(_) => 38
  | Unused_rec_flag => 39
  | Name_out_of_scope(_) => 40
  | Ambiguous_name(_) => 41
  | Disambiguated_name(_) => 42
  | Nonoptional_label(_) => 43
  | Open_shadow_identifier(_) => 44
  | Open_shadow_label_constructor(_) => 45
  | Bad_env_variable(_) => 46
  | Attribute_payload(_) => 47
  | Eliminated_optional_arguments(_) => 48
  | No_cmi_file(_) => 49
  | Bad_docstring(_) => 50
  | Expect_tailcall => 51
  | Fragile_literal_pattern => 52
  | Misplaced_attribute(_) => 53
  | Duplicated_attribute(_) => 54
  | Inlining_impossible(_) => 55
  | Unreachable_case => 56
  | Ambiguous_pattern(_) => 57
  | No_cmx_file(_) => 58
  | Assignment_to_non_mutable_value => 59
  | Unused_module(_) => 60
  | Unboxable_type_in_prim_decl(_) => 61
  | Constraint_on_gadt => 62
  }

let last_warning_number = 62

/* Must be the max number returned by the [number] function. */

let letter = x =>
  switch x {
  | 'a' =>
    let rec loop = i =>
      if i == 0 {
        list{}
      } else {
        list{i, ...loop(i - 1)}
      }
    loop(last_warning_number)
  | 'b' => list{}
  | 'c' => list{1, 2}
  | 'd' => list{3}
  | 'e' => list{4}
  | 'f' => list{5}
  | 'g' => list{}
  | 'h' => list{}
  | 'i' => list{}
  | 'j' => list{}
  | 'k' => list{32, 33, 34, 35, 36, 37, 38, 39}
  | 'l' => list{6}
  | 'm' => list{7}
  | 'n' => list{}
  | 'o' => list{}
  | 'p' => list{8}
  | 'q' => list{}
  | 'r' => list{9}
  | 's' => list{10}
  | 't' => list{}
  | 'u' => list{11, 12}
  | 'v' => list{13}
  | 'w' => list{}
  | 'x' => list{14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30}
  | 'y' => list{26}
  | 'z' => list{27}
  | _ => assert false
  }

type state = {
  active: array<bool>,
  error: array<bool>,
}

let current = ref({
  active: Array.make(last_warning_number + 1, true),
  error: Array.make(last_warning_number + 1, false),
})

let disabled = ref(false)

let without_warnings = f => Misc.protect_refs(list{Misc.R(disabled, true)}, f)

let backup = () => current.contents

let restore = x => current := x

let is_active = x => !disabled.contents && current.contents.active[number(x)]
let is_error = x => !disabled.contents && current.contents.error[number(x)]

let mk_lazy = f => {
  let state = backup()
  lazy {
    let prev = backup()
    restore(state)
    try {
      let r = f()
      restore(prev)
      r
    } catch {
    | exn =>
      restore(prev)
      throw(exn)
    }
  }
}

let parse_opt = (error, active, flags, s) => {
  let set = i => flags[i] = true
  let clear = i => flags[i] = false
  let set_all = i => {
    active[i] = true
    error[i] = true
  }
  let error = () => throw(Arg.Bad("Ill-formed list of warnings"))
  let rec get_num = (n, i) =>
    if i >= String.length(s) {
      (i, n)
    } else {
      switch String.get(s, i) {
      | '0' .. '9' => get_num(10 * n + Char.code(String.get(s, i)) - Char.code('0'), i + 1)
      | _ => (i, n)
      }
    }

  let get_range = i => {
    let (i, n1) = get_num(0, i)
    if i + 2 < String.length(s) && (String.get(s, i) == '.' && String.get(s, i + 1) == '.') {
      let (i, n2) = get_num(0, i + 2)
      if n2 < n1 {
        error()
      }
      (i, n1, n2)
    } else {
      (i, n1, n1)
    }
  }

  let rec loop = i =>
    if i >= String.length(s) {
      ()
    } else {
      switch String.get(s, i) {
      | 'A' .. 'Z' =>
        List.iter(set, letter(Char.lowercase_ascii(String.get(s, i))))
        loop(i + 1)
      | 'a' .. 'z' =>
        List.iter(clear, letter(String.get(s, i)))
        loop(i + 1)
      | '+' => loop_letter_num(set, i + 1)
      | '-' => loop_letter_num(clear, i + 1)
      | '@' => loop_letter_num(set_all, i + 1)
      | _ => error()
      }
    }
  and loop_letter_num = (myset, i) =>
    if i >= String.length(s) {
      error()
    } else {
      switch String.get(s, i) {
      | '0' .. '9' =>
        let (i, n1, n2) = get_range(i)
        for n in n1 to min(n2, last_warning_number) {
          myset(n)
        }
        loop(i)
      | 'A' .. 'Z' =>
        List.iter(myset, letter(Char.lowercase_ascii(String.get(s, i))))
        loop(i + 1)
      | 'a' .. 'z' =>
        List.iter(myset, letter(String.get(s, i)))
        loop(i + 1)
      | _ => error()
      }
    }

  loop(0)
}

let parse_options = (errflag, s) => {
  let error = Array.copy(current.contents.error)
  let active = Array.copy(current.contents.active)
  parse_opt(
    error,
    active,
    if errflag {
      error
    } else {
      active
    },
    s,
  )
  current := {error: error, active: active}
}

/* If you change these, don't forget to change them in man/ocamlc.m */
let defaults_w = "+a-4-6-7-9-27-29-32..42-44-45-48-50-60"
let defaults_warn_error = "-a+31"

let () = parse_options(false, defaults_w)
let () = parse_options(true, defaults_warn_error)

let message = x =>
  switch x {
  | Comment_start => "this is the start of a comment."
  | Comment_not_end => "this is not the end of a comment."
  | Deprecated(s, _, _) =>
    /* Reduce \r\n to \n:
           - Prevents any \r characters being printed on Unix when processing
             Windows sources
           - Prevents \r\r\n being generated on Windows, which affects the
             testsuite
 */
    "deprecated: " ++ Misc.normalise_eol(s)
  | Fragile_match("") => "this pattern-matching is fragile."
  | Fragile_match(s) =>
    "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ++
    (s ++
    ".")
  | Partial_application => "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted(list{}) => assert false
  | Labels_omitted(list{l}) =>
    "label " ++ (l ++ " was omitted in the application of this function.")
  | Labels_omitted(ls) =>
    "labels " ++ (String.concat(", ", ls) ++ " were omitted in the application of this function.")
  | Method_override(list{lab}) => "the method " ++ (lab ++ " is overridden.")
  | Method_override(list{cname, ...slist}) =>
    String.concat(
      " ",
      list{"the following methods are overridden by the class", cname, ":\n ", ...slist},
    )
  | Method_override(list{}) => assert false
  | Partial_match("") => "this pattern-matching is not exhaustive."
  | Partial_match(s) =>
    "this pattern-matching is not exhaustive.\n\
       Here is an example of a case that is not matched:\n" ++
    s
  | Non_closed_record_pattern(s) =>
    "the following labels are not bound in this record pattern:\n" ++
    (s ++
    "\nEither bind these labels explicitly or add '; _' to the pattern.")
  | Statement_type => "this expression should have type unit."
  | Unused_match => "this match case is unused."
  | Unused_pat => "this sub-pattern is unused."
  | Instance_variable_override(list{lab}) =>
    "the instance variable " ++
    (lab ++
    (" is overridden.\n" ++ "The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"))
  | Instance_variable_override(list{cname, ...slist}) =>
    String.concat(
      " ",
      list{"the following instance variables are overridden by the class", cname, ":\n ", ...slist},
    ) ++ "\nThe behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override(list{}) => assert false
  | Illegal_backslash => "illegal backslash escape in string."
  | Implicit_public_methods(l) =>
    "the following private methods were made public implicitly:\n " ++
    (String.concat(" ", l) ++
    ".")
  | Unerasable_optional_argument => "this optional argument cannot be erased."
  | Undeclared_virtual_method(m) => "the virtual method " ++ (m ++ " is not declared.")
  | Not_principal(s) => s ++ " is not principal."
  | Without_principality(s) => s ++ " without principality."
  | Unused_argument => "this argument will not be used by the function."
  | Nonreturning_statement => "this statement never returns (or has an unsound type.)"
  | Preprocessor(s) => s
  | Useless_record_with => "all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name(modname) =>
    "bad source file name: \"" ++ (modname ++ "\" is not a valid module name.")
  | All_clauses_guarded => "this pattern-matching is not exhaustive.\n\
       All clauses in this pattern-matching are guarded."
  | Unused_var(v) | Unused_var_strict(v) => "unused variable " ++ (v ++ ".")
  | Wildcard_arg_to_constant_constr => "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string => "unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions(kind, cname, tc1, tc2) =>
    Printf.sprintf("the %s %s is defined in both types %s and %s.", kind, cname, tc1, tc2)
  | Multiple_definition(modname, file1, file2) =>
    Printf.sprintf("files %s and %s both define a module named %s", file1, file2, modname)
  | Unused_value_declaration(v) => "unused value " ++ (v ++ ".")
  | Unused_open(s) => "unused open " ++ (s ++ ".")
  | Unused_type_declaration(s) => "unused type " ++ (s ++ ".")
  | Unused_for_index(s) => "unused for-loop index " ++ (s ++ ".")
  | Unused_ancestor(s) => "unused ancestor variable " ++ (s ++ ".")
  | Unused_constructor(s, false, false) => "unused constructor " ++ (s ++ ".")
  | Unused_constructor(s, true, _) =>
    "constructor " ++
    (s ++
    " is never used to build values.\n\
        (However, this constructor appears in patterns.)")
  | Unused_constructor(s, false, true) =>
    "constructor " ++
    (s ++
    " is never used to build values.\n\
        Its type is exported as a private type.")
  | Unused_extension(s, is_exception, cu_pattern, cu_privatize) =>
    let kind = if is_exception {
      "exception"
    } else {
      "extension constructor"
    }
    let name = kind ++ (" " ++ s)
    switch (cu_pattern, cu_privatize) {
    | (false, false) => "unused " ++ name
    | (true, _) =>
      name ++ " is never used to build values.\n\
           (However, this constructor appears in patterns.)"
    | (false, true) =>
      name ++ " is never used to build values.\n\
            It is exported or rebound as a private extension."
    }
  | Unused_rec_flag => "unused rec flag."
  | Name_out_of_scope(ty, list{nm}, false) =>
    nm ++
    (" was selected from type " ++
    (ty ++ ".\nIt is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown."))
  | Name_out_of_scope(_, _, false) => assert false
  | Name_out_of_scope(ty, slist, true) =>
    "this record of type " ++
    (ty ++
    (" contains fields that are \n\
       not visible in the current scope: " ++
    (String.concat(" ", slist) ++
    ".\n\
       They will not be selected if the type becomes unknown.")))
  | Ambiguous_name(list{s}, tl, false) =>
    s ++
    (" belongs to several types: " ++
    (String.concat(
      " ",
      tl,
    ) ++ "\nThe first one was selected. Please disambiguate if this is wrong."))
  | Ambiguous_name(_, _, false) => assert false
  | Ambiguous_name(_slist, tl, true) =>
    "these field labels belong to several types: " ++
    (String.concat(" ", tl) ++
    "\nThe first one was selected. Please disambiguate if this is wrong.")
  | Disambiguated_name(s) =>
    "this use of " ++
    (s ++
    " relies on type-directed disambiguation,\n\
       it will not compile with OCaml 4.00 or earlier.")
  | Nonoptional_label(s) => "the label " ++ (s ++ " is not optional.")
  | Open_shadow_identifier(kind, s) =>
    Printf.sprintf(
      "this open statement shadows the %s identifier %s (which is later used)",
      kind,
      s,
    )
  | Open_shadow_label_constructor(kind, s) =>
    Printf.sprintf("this open statement shadows the %s %s (which is later used)", kind, s)
  | Bad_env_variable(var, s) => Printf.sprintf("illegal environment variable %s : %s", var, s)
  | Attribute_payload(a, s) => Printf.sprintf("illegal payload for attribute '%s'.\n%s", a, s)
  | Eliminated_optional_arguments(sl) =>
    Printf.sprintf(
      "implicit elimination of optional argument%s %s",
      if List.length(sl) == 1 {
        ""
      } else {
        "s"
      },
      String.concat(", ", sl),
    )
  | No_cmi_file(name, None) => "no cmi file was found in path for module " ++ name
  | No_cmi_file(name, Some(msg)) =>
    Printf.sprintf("no valid cmi file was found in path for module %s. %s", name, msg)
  | Bad_docstring(unattached) =>
    if unattached {
      "unattached documentation comment (ignored)"
    } else {
      "ambiguous documentation comment"
    }
  | Expect_tailcall => Printf.sprintf("expected tailcall")
  | Fragile_literal_pattern =>
    Printf.sprintf(
      "Code should not depend on the actual values of\n\
         this constructor's arguments. They are only for information\n\
         and may change in future versions. (See manual section 8.5)",
    )
  | Unreachable_case => "this match case is unreachable.\n\
       Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute(attr_name) =>
    Printf.sprintf("the %S attribute cannot appear in this context", attr_name)
  | Duplicated_attribute(attr_name) =>
    Printf.sprintf(
      "the %S attribute is used more than once on this \
          expression",
      attr_name,
    )
  | Inlining_impossible(reason) => Printf.sprintf("Cannot inline: %s", reason)
  | Ambiguous_pattern(vars) =>
    let msg = {
      let vars = List.sort(String.compare, vars)
      switch vars {
      | list{} => assert false
      | list{x} => "variable " ++ x
      | list{_, ..._} => "variables " ++ String.concat(",", vars)
      }
    }
    Printf.sprintf(
      "Ambiguous or-pattern variables under guard;\n\
         %s may match different arguments. (See manual section 8.5)",
      msg,
    )
  | No_cmx_file(name) =>
    Printf.sprintf(
      "no cmx file was found in path for module %s, \
         and its interface was not compiled with -opaque",
      name,
    )
  | Assignment_to_non_mutable_value => "A potential assignment to a non-mutable value was detected \n\
        in this source file.  Such assignments may generate incorrect code \n\
        when using Flambda."
  | Unused_module(s) => "unused module " ++ (s ++ ".")
  | Unboxable_type_in_prim_decl(t) =>
    Printf.sprintf(
      "This primitive declaration uses type %s, which is unannotated and\n\
         unboxable. The representation of such types may change in future\n\
         versions. You should annotate the declaration of %s with [@@boxed]\n\
         or [@@unboxed].",
      t,
      t,
    )
  | Constraint_on_gadt => "Type constraints do not apply to GADT cases of variant types."
  }

let sub_locs = x =>
  switch x {
  | Deprecated(_, def, use) => list{(def, "Definition"), (use, "Expected signature")}
  | _ => list{}
  }

let nerrors = ref(0)

type reporting_information = {
  number: int,
  message: string,
  is_error: bool,
  sub_locs: list<(loc, string)>,
}

let report = w =>
  switch is_active(w) {
  | false => #Inactive
  | true =>
    if is_error(w) {
      incr(nerrors)
    }
    #Active({
      number: number(w),
      message: message(w),
      is_error: is_error(w),
      sub_locs: sub_locs(w),
    })
  }

exception Errors

let reset_fatal = () => nerrors := 0

let check_fatal = () =>
  if nerrors.contents > 0 {
    nerrors := 0
    throw(Errors)
  }

let descriptions = list{
  (1, "Suspicious-looking start-of-comment mark."),
  (2, "Suspicious-looking end-of-comment mark."),
  (3, "Deprecated feature."),
  (
    4,
    "Fragile pattern matching: matching that will remain complete even\n\
   \    if additional constructors are added to one of the variant types\n\
   \    matched.",
  ),
  (
    5,
    "Partially applied function: expression whose result has function\n\
   \    type and is ignored.",
  ),
  (6, "Label omitted in function application."),
  (7, "Method overridden."),
  (8, "Partial match: missing cases in pattern-matching."),
  (9, "Missing fields in a record pattern."),
  (
    10,
    "Expression on the left-hand side of a sequence that doesn't have \
      type\n\
   \    \"unit\" (and that is not a function, see warning number 5).",
  ),
  (11, "Redundant case in a pattern matching (unused match case)."),
  (12, "Redundant sub-pattern in a pattern-matching."),
  (13, "Instance variable overridden."),
  (14, "Illegal backslash escape in a string constant."),
  (15, "Private method made public implicitly."),
  (16, "Unerasable optional argument."),
  (17, "Undeclared virtual method."),
  (18, "Non-principal type."),
  (19, "Type without principality."),
  (20, "Unused function argument."),
  (21, "Non-returning statement."),
  (22, "Preprocessor warning."),
  (23, "Useless record \"with\" clause."),
  (
    24,
    "Bad module name: the source file name is not a valid OCaml module \
        name.",
  ),
  (25, "Deprecated: now part of warning 8."),
  (
    26,
    "Suspicious unused variable: unused variable that is bound\n\
   \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.",
  ),
  (
    27,
    "Innocuous unused variable: unused variable that is not bound with\n\
   \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.",
  ),
  (28, "Wildcard pattern given as argument to a constant constructor."),
  (29, "Unescaped end-of-line in a string constant (non-portable code)."),
  (
    30,
    "Two labels or constructors of the same name are defined in two\n\
   \    mutually recursive types.",
  ),
  (31, "A module is linked twice in the same executable."),
  (32, "Unused value declaration."),
  (33, "Unused open statement."),
  (34, "Unused type declaration."),
  (35, "Unused for-loop index."),
  (36, "Unused ancestor variable."),
  (37, "Unused constructor."),
  (38, "Unused extension constructor."),
  (39, "Unused rec flag."),
  (40, "Constructor or label name used out of scope."),
  (41, "Ambiguous constructor or label name."),
  (42, "Disambiguated constructor or label name (compatibility warning)."),
  (43, "Nonoptional label applied as optional."),
  (44, "Open statement shadows an already defined identifier."),
  (45, "Open statement shadows an already defined label or constructor."),
  (46, "Error in environment variable."),
  (47, "Illegal attribute payload."),
  (48, "Implicit elimination of optional arguments."),
  (49, "Absent cmi file when looking up module alias."),
  (50, "Unexpected documentation comment."),
  (51, "Warning on non-tail calls if @tailcall present."),
  (52, "Fragile constant pattern."),
  (53, "Attribute cannot appear in this context"),
  (54, "Attribute used more than once on an expression"),
  (55, "Inlining impossible"),
  (56, "Unreachable case in a pattern-matching (based on type information)."),
  (57, "Ambiguous or-pattern variables under guard"),
  (58, "Missing cmx file"),
  (59, "Assignment to non-mutable value"),
  (60, "Unused module declaration"),
  (61, "Unboxable type in primitive declaration"),
  (62, "Type constraint on GADT type declaration"),
}

let help_warnings = () => {
  List.iter(((i, s)) => Printf.printf("%3i %s\n", i, s), descriptions)
  print_endline("  A all warnings")
  for i in Char.code('b') to Char.code('z') {
    let c = Char.chr(i)
    switch letter(c) {
    | list{} => ()
    | list{n} => Printf.printf("  %c Alias for warning %i.\n", Char.uppercase_ascii(c), n)
    | l =>
      Printf.printf(
        "  %c warnings %s.\n",
        Char.uppercase_ascii(c),
        String.concat(", ", List.map(string_of_int, l)),
      )
    }
  }
  exit(0)
}

