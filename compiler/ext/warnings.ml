(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* When you change this, you need to update the documentation:
   - man/ocamlc.m
   - man/ocamlopt.m
   - manual/manual/cmds/comp.etex
   - manual/manual/cmds/native.etex
*)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type top_level_unit_help = FunctionCall | Other

type t =
  | Comment_start (*  1 *)
  | Comment_not_end (*  2 *)
  | Deprecated of string * loc * loc (*  3 *)
  | Fragile_match of string (*  4 *)
  | Partial_application (*  5 *)
  | Method_override of string list (*  7 *)
  | Partial_match of string (*  8 *)
  | Non_closed_record_pattern of string (*  9 *)
  | Statement_type (* 10 *)
  | Unused_match (* 11 *)
  | Unused_pat (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash (* 14 *)
  | Implicit_public_methods of string list (* 15 *)
  | Unerasable_optional_argument (* 16 *)
  | Unused_argument (* 20 *)
  | Nonreturning_statement (* 21 *)
  | Preprocessor of string (* 22 *)
  | Useless_record_with (* 23 *)
  | Bad_module_name of string (* 24 *)
  | All_clauses_guarded (* 8, used to be 25 *)
  | Unused_var of string (* 26 *)
  | Unused_var_strict of string (* 27 *)
  | Wildcard_arg_to_constant_constr (* 28 *)
  | Eol_in_string (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  | Unused_value_declaration of string (* 32 *)
  | Unused_open of string (* 33 *)
  | Unused_type_declaration of string (* 34 *)
  | Unused_for_index of string (* 35 *)
  | Unused_constructor of string * bool * bool (* 37 *)
  | Unused_extension of string * bool * bool * bool (* 38 *)
  | Unused_rec_flag (* 39 *)
  | Ambiguous_name of string list * string list * bool (* 41 *)
  | Nonoptional_label of string (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Attribute_payload of string * string (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string * string option (* 49 *)
  | Bad_docstring of bool (* 50 *)
  | Fragile_literal_pattern (* 52 *)
  | Misplaced_attribute of string (* 53 *)
  | Duplicated_attribute of string (* 54 *)
  | Unreachable_case (* 56 *)
  | Ambiguous_pattern of string list (* 57 *)
  | Unused_module of string (* 60 *)
  | Constraint_on_gadt (* 62 *)
  | Bs_unused_attribute of string (* 101 *)
  | Bs_polymorphic_comparison (* 102 *)
  | Bs_ffi_warning of string (* 103 *)
  | Bs_derive_warning of string (* 104 *)
  | Bs_fragile_external of string (* 105 *)
  | Bs_unimplemented_primitive of string (* 106 *)
  | Bs_integer_literal_overflow (* 107 *)
  | Bs_uninterpreted_delimiters of string (* 108 *)
  | Bs_toplevel_expression_unit of
      (string * top_level_unit_help) option (* 109 *)
  | Bs_todo of string option (* 110 *)

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Deprecated _ -> 3
  | Fragile_match _ -> 4
  | Partial_application -> 5
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Non_closed_record_pattern _ -> 9
  | Statement_type -> 10
  | Unused_match -> 11
  | Unused_pat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Unused_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 8 (* used to be 25 *)
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Ambiguous_name _ -> 41
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Bad_docstring _ -> 50
  | Fragile_literal_pattern -> 52
  | Misplaced_attribute _ -> 53
  | Duplicated_attribute _ -> 54
  | Unreachable_case -> 56
  | Ambiguous_pattern _ -> 57
  | Unused_module _ -> 60
  | Constraint_on_gadt -> 62
  | Bs_unused_attribute _ -> 101
  | Bs_polymorphic_comparison -> 102
  | Bs_ffi_warning _ -> 103
  | Bs_derive_warning _ -> 104
  | Bs_fragile_external _ -> 105
  | Bs_unimplemented_primitive _ -> 106
  | Bs_integer_literal_overflow -> 107
  | Bs_uninterpreted_delimiters _ -> 108
  | Bs_toplevel_expression_unit _ -> 109
  | Bs_todo _ -> 110

let last_warning_number = 110

let letter_all =
  let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
  loop last_warning_number

(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' -> letter_all
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false

type state = {active: bool array; error: bool array}

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
    }

let disabled = ref false

let without_warnings f = Misc.protect_refs [Misc.R (disabled, true)] f

let backup () = !current

let restore x = current := x

let is_active x = (not !disabled) && !current.active.(number x)

let is_error x = (not !disabled) && !current.error.(number x)

let mk_lazy f =
  let state = backup () in
  lazy
    (let prev = backup () in
     restore state;
     try
       let r = f () in
       restore prev;
       r
     with exn ->
       restore prev;
       raise exn)

let parse_opt error active flags s =
  let set i = flags.(i) <- true in
  let clear i = flags.(i) <- false in
  let set_all i =
    active.(i) <- true;
    error.(i) <- true
  in
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then (i, n)
    else
      match s.[i] with
      | '0' .. '9' ->
        get_num ((10 * n) + Char.code s.[i] - Char.code '0') (i + 1)
      | _ -> (i, n)
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then (
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      (i, n1, n2))
    else (i, n1, n1)
  in
  let rec loop i =
    if i >= String.length s then ()
    else
      match s.[i] with
      | 'A' .. 'Z' ->
        List.iter set (letter (Char.lowercase_ascii s.[i]));
        loop (i + 1)
      | 'a' .. 'z' ->
        List.iter clear (letter s.[i]);
        loop (i + 1)
      | '+' -> loop_letter_num set (i + 1)
      | '-' -> loop_letter_num clear (i + 1)
      | '@' -> loop_letter_num set_all (i + 1)
      | _ -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error ()
    else
      match s.[i] with
      | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to Ext_pervasives.min_int n2 last_warning_number do
          myset n
        done;
        loop i
      | 'A' .. 'Z' ->
        List.iter myset (letter (Char.lowercase_ascii s.[i]));
        loop (i + 1)
      | 'a' .. 'z' ->
        List.iter myset (letter s.[i]);
        loop (i + 1)
      | _ -> error ()
  in
  loop 0

let parse_options errflag s =
  let error = Array.copy !current.error in
  let active = Array.copy !current.active in
  parse_opt error active (if errflag then error else active) s;
  current := {error; active}

let reset () =
  parse_options false Bsc_warnings.defaults_w;
  parse_options true Bsc_warnings.defaults_warn_error

let () = reset ()

let message = function
  | Comment_start -> "this is the start of a comment."
  | Comment_not_end -> "this is not the end of a comment."
  | Deprecated (s, _, _) ->
    (* Reduce \r\n to \n:
         - Prevents any \r characters being printed on Unix when processing
           Windows sources
         - Prevents \r\r\n being generated on Windows, which affects the
           testsuite
    *)
    "deprecated: " ^ Misc.normalise_eol s
  | Fragile_match "" -> "this pattern-matching is fragile."
  | Fragile_match s ->
    "this pattern-matching is fragile.\n\
     It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Partial_application ->
    "this function application is partial,\nmaybe some arguments are missing."
  | Method_override [lab] -> "the method " ^ lab ^ " is overridden."
  | Method_override (cname :: slist) ->
    String.concat " "
      ("the following methods are overridden by the class" :: cname :: ":\n "
     :: slist)
  | Method_override [] -> assert false
  | Partial_match "" ->
    "You forgot to handle a possible case here, though we don't have more \
     information on the value."
  | Partial_match s ->
    "You forgot to handle a possible case here, for example: \n  " ^ s
  | Non_closed_record_pattern s ->
    "the following labels are not bound in this record pattern: " ^ s
    ^ "\nEither bind these labels explicitly or add ', _' to the pattern."
  | Statement_type ->
    "This expression returns a value, but you're not doing anything with it. \
     If this is on purpose, wrap it with `ignore`."
  | Unused_match -> "this match case is unused."
  | Unused_pat -> "this sub-pattern is unused."
  | Instance_variable_override [lab] ->
    "the instance variable " ^ lab ^ " is overridden.\n"
    ^ "The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override (cname :: slist) ->
    String.concat " "
      ("the following instance variables are overridden by the class" :: cname
     :: ":\n " :: slist)
    ^ "\nThe behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override [] -> assert false
  | Illegal_backslash -> "illegal backslash escape in string."
  | Implicit_public_methods l ->
    "the following private methods were made public implicitly:\n "
    ^ String.concat " " l ^ "."
  | Unerasable_optional_argument ->
    String.concat ""
      [
        "This optional parameter in final position will, in practice, not be \
         optional.\n";
        "  Reorder the parameters so that at least one non-optional one is in \
         final position or, if all parameters are optional, insert a final \
         ().\n\n";
        "  Explanation: If the final parameter is optional, it'd be unclear \
         whether a function application that omits it should be considered \
         fully applied, or partially applied. Imagine writing `let title = \
         display(\"hello!\")`, only to realize `title` isn't your desired \
         result, but a curried call that takes a final optional argument, e.g. \
         `~showDate`.\n\n";
        "  Formal rule: an optional argument is considered intentionally \
         omitted when the 1st positional (i.e. neither labeled nor optional) \
         argument defined after it is passed in.";
      ]
  | Unused_argument -> "this argument will not be used by the function."
  | Nonreturning_statement ->
    "this statement never returns (or has an unsound type.)"
  | Preprocessor s -> s
  | Useless_record_with ->
    "All the fields are already explicitly listed in this record. You can \
     remove the `...` spread."
  | Bad_module_name modname ->
    "This file's name is potentially invalid. The build systems conventionally \
     turn a file name into a module name by upper-casing the first letter. "
    ^ modname ^ " isn't a valid module name.\n"
    ^ "Note: some build systems might e.g. turn kebab-case into CamelCase \
       module, which is why this isn't a hard error."
  | All_clauses_guarded ->
    "this pattern-matching is not exhaustive.\n\
     All clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v ->
    Format.sprintf
      "unused variable %s.\n\n\
       Fix this by:\n\
       - Deleting the variable if it's not used anymore.\n\
       - Prepending the variable name with `_` (like `_%s`) to ignore that the \
       variable is unused.\n\
       - Using the variable somewhere."
      v v
  | Wildcard_arg_to_constant_constr ->
    "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
    "unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
    Printf.sprintf "the %s %s is defined in both types %s and %s." kind cname
      tc1 tc2
  | Unused_value_declaration v -> "unused value " ^ v ^ "."
  | Unused_open s -> "unused open " ^ s ^ "."
  | Unused_type_declaration s -> "unused type " ^ s ^ "."
  | Unused_for_index s -> "unused for-loop index " ^ s ^ "."
  | Unused_constructor (s, false, false) -> "unused constructor " ^ s ^ "."
  | Unused_constructor (s, true, _) ->
    "constructor " ^ s
    ^ " is never used to build values.\n\
       (However, this constructor appears in patterns.)"
  | Unused_constructor (s, false, true) ->
    "constructor " ^ s
    ^ " is never used to build values.\nIts type is exported as a private type."
  | Unused_extension (s, is_exception, cu_pattern, cu_privatize) -> (
    let kind = if is_exception then "exception" else "extension constructor" in
    let name = kind ^ " " ^ s in
    match (cu_pattern, cu_privatize) with
    | false, false -> "unused " ^ name
    | true, _ ->
      name
      ^ " is never used to build values.\n\
         (However, this constructor appears in patterns.)"
    | false, true ->
      name
      ^ " is never used to build values.\n\
         It is exported or rebound as a private extension.")
  | Unused_rec_flag -> "unused rec flag."
  | Ambiguous_name ([s], tl, false) ->
    s ^ " belongs to several types: " ^ String.concat " " tl
    ^ "\nThe first one was selected. Please disambiguate if this is wrong."
  | Ambiguous_name (_, _, false) -> assert false
  | Ambiguous_name (_slist, tl, true) ->
    "these field labels belong to several types: " ^ String.concat " " tl
    ^ "\nThe first one was selected. Please disambiguate if this is wrong."
  | Nonoptional_label s -> "the label " ^ s ^ " is not optional."
  | Open_shadow_identifier (kind, s) ->
    Printf.sprintf
      "this open statement shadows the %s identifier %s (which is later used)"
      kind s
  | Open_shadow_label_constructor (kind, s) ->
    Printf.sprintf "this open statement shadows the %s %s (which is later used)"
      kind s
  | Attribute_payload (a, s) ->
    Printf.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
    Printf.sprintf "implicit elimination of optional argument%s %s"
      (if List.length sl = 1 then "" else "s")
      (String.concat ", " sl)
  | No_cmi_file (name, None) ->
    "no cmi file was found in path for module " ^ name
  | No_cmi_file (name, Some msg) ->
    Printf.sprintf "no valid cmi file was found in path for module %s. %s" name
      msg
  | Bad_docstring unattached ->
    if unattached then "unattached documentation comment (ignored)"
    else "ambiguous documentation comment"
  | Fragile_literal_pattern ->
    Printf.sprintf
      "Code should not depend on the actual values of\n\
       this constructor's arguments. They are only for information\n\
       and may change in future versions. (See manual section 8.5)"
  | Unreachable_case ->
    "this match case is unreachable.\n\
     Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute attr_name ->
    Printf.sprintf "the %S attribute cannot appear in this context" attr_name
  | Duplicated_attribute attr_name ->
    Printf.sprintf "the %S attribute is used more than once on this expression"
      attr_name
  | Ambiguous_pattern vars ->
    let msg =
      let vars = List.sort String.compare vars in
      match vars with
      | [] -> assert false
      | [x] -> "variable " ^ x
      | _ :: _ -> "variables " ^ String.concat "," vars
    in
    Printf.sprintf
      "Ambiguous or-pattern variables under guard;\n\
       %s may match different arguments. (See manual section 8.5)"
      msg
  | Unused_module s -> "unused module " ^ s ^ "."
  | Constraint_on_gadt ->
    "Type constraints do not apply to GADT cases of variant types."
  | Bs_unused_attribute s ->
    "Unused attribute: @" ^ s
    ^ "\n\
       This attribute has no effect here.\n\
       For example, some attributes are only meaningful in externals.\n"
  | Bs_polymorphic_comparison ->
    "Polymorphic comparison introduced (maybe unsafe)"
  | Bs_ffi_warning s -> "FFI warning: " ^ s
  | Bs_derive_warning s -> "@deriving warning: " ^ s
  | Bs_fragile_external s ->
    s
    ^ " : using an empty string as a shorthand to infer the external's name \
       from the value's name is dangerous when refactoring, and therefore \
       deprecated"
  | Bs_unimplemented_primitive s -> "Unimplemented primitive used: " ^ s
  | Bs_integer_literal_overflow ->
    "Integer literal exceeds the range of representable integers of type int"
  | Bs_uninterpreted_delimiters s -> "Uninterpreted delimiters " ^ s
  | Bs_toplevel_expression_unit help ->
    Printf.sprintf
      "This%sis at the top level and is expected to return `unit`. But it's \
       returning %s.\n\n\
      \  In ReScript, anything at the top level must evaluate to `unit`. You \
       can fix this by assigning the expression to a value, or piping it into \
       the `ignore` function.%s"
      (match help with
      | Some (_, FunctionCall) -> " function call "
      | _ -> " ")
      (match help with
      | Some (return_type, _) -> Printf.sprintf "`%s`" return_type
      | None -> "something that is not `unit`")
      (match help with
      | Some (_, help_typ) ->
        let help_text =
          match help_typ with
          | FunctionCall -> "yourFunctionCall()"
          | Other -> "yourExpression"
        in
        Printf.sprintf
          "\n\n\
          \  Possible solutions:\n\
          \  - Assigning to a value that is then ignored: `let _ = %s`\n\
          \  - Piping into the built-in ignore function to ignore the result: \
           `%s->ignore`"
          help_text help_text
      | _ -> "")
  | Bs_todo maybe_text ->
    (match maybe_text with
    | None -> "Todo found."
    | Some todo -> "Todo found: " ^ todo)
    ^ "\n\n\
      \  This code is not implemented yet and will crash at runtime. Make sure \
       you implement this before running the code."

let sub_locs = function
  | Deprecated (_, def, use) ->
    [(def, "Definition"); (use, "Expected signature")]
  | _ -> []

let has_warnings = ref false

let nerrors = ref 0

type reporting_information = {
  number: int;
  message: string;
  is_error: bool;
  sub_locs: (loc * string) list;
}

let report w =
  match is_active w with
  | false -> `Inactive
  | true ->
    has_warnings := true;
    if is_error w then incr nerrors;
    `Active
      {
        number = number w;
        message = message w;
        is_error = is_error w;
        sub_locs = sub_locs w;
      }

exception Errors

let reset_fatal () = nerrors := 0

let check_fatal () =
  if !nerrors > 0 then (
    nerrors := 0;
    raise Errors)

let descriptions =
  [
    (1, "Suspicious-looking start-of-comment mark.");
    (2, "Suspicious-looking end-of-comment mark.");
    (3, "Deprecated feature.");
    ( 4,
      "Fragile pattern matching: matching that will remain complete even\n\
      \    if additional constructors are added to one of the variant types\n\
      \    matched." );
    ( 5,
      "Partially applied function: expression whose result has function\n\
      \    type and is ignored." );
    (6, "Label omitted in function application.");
    (7, "Method overridden.");
    (8, "Partial match: missing cases in pattern-matching.");
    (9, "Missing fields in a record pattern.");
    ( 10,
      "Expression on the left-hand side of a sequence that doesn't have type\n\
      \    \"unit\" (and that is not a function, see warning number 5)." );
    (11, "Redundant case in a pattern matching (unused match case).");
    (12, "Redundant sub-pattern in a pattern-matching.");
    (13, "Instance variable overridden.");
    (14, "Illegal backslash escape in a string constant.");
    (15, "Private method made public implicitly.");
    (16, "Unerasable optional argument.");
    (17, "Undeclared virtual method.");
    (18, "Non-principal type.");
    (19, "Type without principality.");
    (20, "Unused function argument.");
    (21, "Non-returning statement.");
    (22, "Preprocessor warning.");
    (23, "Useless record \"with\" clause.");
    ( 24,
      "Bad module name: the source file name is not a valid OCaml module name."
    );
    (25, "Deprecated: now part of warning 8.");
    ( 26,
      "Suspicious unused variable: unused variable that is bound\n\
      \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
      \    character." );
    ( 27,
      "Innocuous unused variable: unused variable that is not bound with\n\
      \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
      \    character." );
    (28, "Wildcard pattern given as argument to a constant constructor.");
    (29, "Unescaped end-of-line in a string constant (non-portable code).");
    ( 30,
      "Two labels or constructors of the same name are defined in two\n\
      \    mutually recursive types." );
    (31, "A module is linked twice in the same executable.");
    (32, "Unused value declaration.");
    (33, "Unused open statement.");
    (34, "Unused type declaration.");
    (35, "Unused for-loop index.");
    (36, "Unused ancestor variable.");
    (37, "Unused constructor.");
    (38, "Unused extension constructor.");
    (39, "Unused rec flag.");
    (41, "Ambiguous constructor or label name.");
    (43, "Nonoptional label applied as optional.");
    (44, "Open statement shadows an already defined identifier.");
    (45, "Open statement shadows an already defined label or constructor.");
    (46, "Error in environment variable.");
    (47, "Illegal attribute payload.");
    (48, "Implicit elimination of optional arguments.");
    (49, "Absent cmi file when looking up module alias.");
    (50, "Unexpected documentation comment.");
    (51, "Warning on non-tail calls if @tailcall present.");
    (52, "Fragile constant pattern.");
    (53, "Attribute cannot appear in this context");
    (54, "Attribute used more than once on an expression");
    (55, "Inlining impossible");
    (56, "Unreachable case in a pattern-matching (based on type information).");
    (57, "Ambiguous or-pattern variables under guard");
    (59, "Assignment to non-mutable value");
    (60, "Unused module declaration");
    (61, "Unboxable type in primitive declaration");
    (62, "Type constraint on GADT type declaration");
    (101, "Unused bs attributes");
    (102, "Polymorphic comparison introduced (maybe unsafe)");
    (103, "Fragile FFI definitions");
    (104, "@deriving warning with customized message ");
    ( 105,
      "External name is inferred from val name is unsafe from refactoring when \
       changing value name" );
    (106, "Unimplemented primitive used:");
    ( 107,
      "Integer literal exceeds the range of representable integers of type int"
    );
    (108, "Uninterpreted delimiters (for unicode)");
    (109, "Toplevel expression has unit type");
    (110, "Todo found");
  ]

let help_warnings () =
  List.iter (fun (i, s) -> Printf.printf "%3i %s\n" i s) descriptions;
  print_endline "  A all warnings";
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
      Printf.printf "  %c Alias for warning %i.\n" (Char.uppercase_ascii c) n
    | l ->
      Printf.printf "  %c warnings %s.\n" (Char.uppercase_ascii c)
        (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
