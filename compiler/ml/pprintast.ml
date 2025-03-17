(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Thomas Gazagnaire, OCamlPro                       *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*               Hongbo Zhang, University of Pennsylvania                 *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Original Code from Ber-metaocaml, modified for 3.12.0 and fixed *)
(* Printing code expressions *)
(* Authors:  Ed Pizzi, Fabrice Le Fessant *)
(* Extensive Rewrite: Hongbo Zhang: University of Pennsylvania *)
(* TODO more fine-grained precedence pretty-printing *)

open Asttypes
open Format
open Location
open Longident
open Parsetree
open Ast_helper

let prefix_symbols = ['!'; '?'; '~']
let infix_symbols =
  ['='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '#']

(* type fixity = Infix| Prefix  *)
let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::"]

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string = function
  | s when List.mem s special_infix_strings -> `Infix s
  | s when List.mem s.[0] infix_symbols -> `Infix s
  | s when List.mem s.[0] prefix_symbols -> `Prefix s
  | s when s.[0] = '.' -> `Mixfix s
  | _ -> `Normal

let view_fixity_of_exp = function
  | {pexp_desc = Pexp_ident {txt = Lident l; _}; pexp_attributes = []} ->
    fixity_of_string l
  | _ -> `Normal

let is_infix = function
  | `Infix _ -> true
  | _ -> false
let is_mixfix = function
  | `Mixfix _ -> true
  | _ -> false

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  let fix = fixity_of_string txt in
  is_infix fix || is_mixfix fix || List.mem txt.[0] prefix_symbols

(* some infixes need spaces around parens to avoid clashes with comment
   syntax *)
let needs_spaces txt = txt.[0] = '*' || txt.[String.length txt - 1] = '*'

(* add parentheses to binders when they are in fact infix or prefix operators *)
let protect_ident ppf txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%s"
    else if needs_spaces txt then "(@;%s@;)"
    else "(%s)"
  in
  fprintf ppf format txt

let protect_longident ppf print_longident longprefix txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%a.%s"
    else if needs_spaces txt then "%a.(@;%s@;)"
    else "%a.(%s)"
  in
  fprintf ppf format print_longident longprefix txt

type space_formatter = (unit, Format.formatter, unit) format

let override = function
  | Override -> "!"
  | Fresh -> ""

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function
  | Invariant -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t
  | `tuple ]

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ({txt = Lident "()"; _}, _) -> `tuple
  | Pexp_construct ({txt = Lident "[]"; _}, _) -> `nil
  | Pexp_construct ({txt = Lident "::"; _}, Some _) ->
    let rec loop exp acc =
      match exp with
      | {
       pexp_desc = Pexp_construct ({txt = Lident "[]"; _}, _);
       pexp_attributes = [];
      } ->
        (List.rev acc, true)
      | {
       pexp_desc =
         Pexp_construct
           ( {txt = Lident "::"; _},
             Some {pexp_desc = Pexp_tuple [e1; e2]; pexp_attributes = []} );
       pexp_attributes = [];
      } ->
        loop e2 (e1 :: acc)
      | e -> (List.rev (e :: acc), false)
    in
    let ls, b = loop x [] in
    if b then `list ls else `cons ls
  | Pexp_construct (x, None) -> `simple x.txt
  | _ -> `normal

let is_simple_construct : construct -> bool = function
  | `nil | `tuple | `list _ | `simple _ -> true
  | `cons _ | `normal -> false

let pp = fprintf

type ctxt = {pipe: bool; semi: bool; ifthenelse: bool}

let reset_ctxt = {pipe = false; semi = false; ifthenelse = false}
let under_pipe ctxt = {ctxt with pipe = true}
let under_semi ctxt = {ctxt with semi = true}
let under_ifthenelse ctxt = {ctxt with ifthenelse = true}
(*
let reset_semi ctxt = { ctxt with semi=false }
let reset_ifthenelse ctxt = { ctxt with ifthenelse=false }
let reset_pipe ctxt = { ctxt with pipe=false }
*)

let list :
    'a.
    ?sep:space_formatter ->
    ?first:space_formatter ->
    ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a list ->
    unit =
 fun ?sep ?first ?last fu f xs ->
  let first =
    match first with
    | Some x -> x
    | None -> ("" : _ format6)
  and last =
    match last with
    | Some x -> x
    | None -> ("" : _ format6)
  and sep =
    match sep with
    | Some x -> x
    | None -> ("@ " : _ format6)
  in
  let aux f = function
    | [] -> ()
    | [x] -> fu f x
    | xs ->
      let rec loop f = function
        | [x] -> fu f x
        | x :: xs ->
          fu f x;
          pp f sep;
          loop f xs
        | _ -> assert false
      in
      pp f first;
      loop f xs;
      pp f last
  in
  aux f xs

let option :
    'a.
    ?first:space_formatter ->
    ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a option ->
    unit =
 fun ?first ?last fu f a ->
  let first =
    match first with
    | Some x -> x
    | None -> ("" : _ format6)
  and last =
    match last with
    | Some x -> x
    | None -> ("" : _ format6)
  in
  match a with
  | None -> ()
  | Some x ->
    pp f first;
    fu f x;
    pp f last

let paren :
    'a.
    ?first:space_formatter ->
    ?last:space_formatter ->
    bool ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a ->
    unit =
 fun ?(first = ("" : _ format6)) ?(last = ("" : _ format6)) b fu f x ->
  if b then (
    pp f "(";
    pp f first;
    fu f x;
    pp f last;
    pp f ")")
  else fu f x

let rec longident f = function
  | Lident s -> protect_ident f s
  | Ldot (y, s) -> protect_longident f longident y s
  | Lapply (y, s) -> pp f "%a(%a)" longident y longident s

let longident_loc f x = pp f "%a" longident x.txt

let string_of_int_as_char i = Ext_util.string_of_int_as_char i

let constant f = function
  | Pconst_char i -> pp f "%s" (string_of_int_as_char i)
  | Pconst_string (i, None) -> pp f "%S" i
  | Pconst_string (i, Some delim) -> pp f "{%s|%s|%s}" delim i delim
  | Pconst_integer (i, None) -> paren (i.[0] = '-') (fun f -> pp f "%s") f i
  | Pconst_integer (i, Some m) ->
    paren (i.[0] = '-') (fun f (i, m) -> pp f "%s%c" i m) f (i, m)
  | Pconst_float (i, None) -> paren (i.[0] = '-') (fun f -> pp f "%s") f i
  | Pconst_float (i, Some m) ->
    paren (i.[0] = '-') (fun f (i, m) -> pp f "%s%c" i m) f (i, m)

(* trailing space*)
let mutable_flag f = function
  | Immutable -> ()
  | Mutable -> pp f "mutable@;"

let optional_flag f = function
  | false -> ()
  | true -> pp f "?"

(* trailing space added *)
let rec_flag f rf =
  match rf with
  | Nonrecursive -> ()
  | Recursive -> pp f "rec "
let nonrec_flag f rf =
  match rf with
  | Nonrecursive -> pp f "nonrec "
  | Recursive -> ()
let direction_flag f = function
  | Upto -> pp f "to@ "
  | Downto -> pp f "downto@ "
let private_flag f = function
  | Public -> ()
  | Private -> pp f "private@ "

let constant_string f s = pp f "%S" s
let tyvar f str = pp f "'%s" str
let tyvar_loc f str = pp f "'%s" str.txt
let string_quot f x = pp f "`%s" x

let rec type_with_label ctxt f (label, c) =
  match label with
  | Nolabel -> core_type1 ctxt f c (* otherwise parenthesize *)
  | Labelled {txt = s} -> pp f "%s:%a" s (core_type1 ctxt) c
  | Optional {txt = s} -> pp f "?%s:%a" s (core_type1 ctxt) c

and core_type ctxt f x =
  if x.ptyp_attributes <> [] then
    pp f "((%a)%a)" (core_type ctxt)
      {x with ptyp_attributes = []}
      (attributes ctxt) x.ptyp_attributes
  else
    match x.ptyp_desc with
    | Ptyp_arrow {lbl = l; arg; ret; arity} ->
      pp f "@[<2>%a@;->@;%a%s@]" (* FIXME remove parens later *)
        (type_with_label ctxt) (l, arg) (core_type ctxt) ret
        (match arity with
        | None -> ""
        | Some n -> " (a:" ^ string_of_int n ^ ")")
    | Ptyp_alias (ct, s) -> pp f "@[<2>%a@;as@;'%s@]" (core_type1 ctxt) ct s
    | Ptyp_poly ([], ct) -> core_type ctxt f ct
    | Ptyp_poly (sl, ct) ->
      pp f "@[<2>%a%a@]"
        (fun f l ->
          pp f "%a"
            (fun f l ->
              match l with
              | [] -> ()
              | _ -> pp f "%a@;.@;" (list tyvar_loc ~sep:"@;") l)
            l)
        sl (core_type ctxt) ct
    | _ -> pp f "@[<2>%a@]" (core_type1 ctxt) x

and core_type1 ctxt f x =
  if x.ptyp_attributes <> [] then core_type ctxt f x
  else
    match x.ptyp_desc with
    | Ptyp_any -> pp f "_"
    | Ptyp_var s -> tyvar f s
    | Ptyp_tuple l -> pp f "(%a)" (list (core_type1 ctxt) ~sep:"@;*@;") l
    | Ptyp_constr (li, l) ->
      pp f (* "%a%a@;" *) "%a%a"
        (fun f l ->
          match l with
          | [] -> ()
          | [x] -> pp f "%a@;" (core_type1 ctxt) x
          | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l)
        l longident_loc li
    | Ptyp_variant (l, closed, low) ->
      let type_variant_helper f x =
        match x with
        | Rtag (l, attrs, _, ctl) ->
          pp f "@[<2>%a%a@;%a@]" string_quot l.txt
            (fun f l ->
              match l with
              | [] -> ()
              | _ -> pp f "@;of@;%a" (list (core_type ctxt) ~sep:"&") ctl)
            ctl (attributes ctxt) attrs
        | Rinherit ct -> core_type ctxt f ct
      in
      pp f "@[<2>[%a%a]@]"
        (fun f l ->
          match (l, closed) with
          | [], Closed -> ()
          | [], Open -> pp f ">" (* Cf #7200: print [>] correctly *)
          | _ ->
            pp f "%s@;%a"
              (match (closed, low) with
              | Closed, None -> ""
              | Closed, Some _ -> "<" (* FIXME desugar the syntax sugar*)
              | Open, _ -> ">")
              (list type_variant_helper ~sep:"@;<1 -2>| ")
              l)
        l
        (fun f low ->
          match low with
          | Some [] | None -> ()
          | Some xs -> pp f ">@ %a" (list string_quot) xs)
        low
    | Ptyp_object (l, o) ->
      let core_field_type f = function
        | Otag (l, attrs, ct) ->
          pp f "@[<hov2>%s: %a@ %a@ @]" l.txt (core_type ctxt) ct
            (attributes ctxt) attrs (* Cf #7200 *)
        | Oinherit ct -> pp f "@[<hov2>%a@ @]" (core_type ctxt) ct
      in
      let field_var f = function
        | Asttypes.Closed -> ()
        | Asttypes.Open -> (
          match l with
          | [] -> pp f ".."
          | _ -> pp f " ;..")
      in
      pp f "@[<hov2><@ %a%a@ > @]"
        (list core_field_type ~sep:";")
        l field_var o (* Cf #7200 *)
    | Ptyp_package (lid, cstrs) -> (
      let aux f (s, ct) =
        pp f "type %a@ =@ %a" longident_loc s (core_type ctxt) ct
      in
      match cstrs with
      | [] -> pp f "@[<hov2>(module@ %a)@]" longident_loc lid
      | _ ->
        pp f "@[<hov2>(module@ %a@ with@ %a)@]" longident_loc lid
          (list aux ~sep:"@ and@ ") cstrs)
    | Ptyp_extension e -> extension ctxt f e
    | _ -> paren true (core_type ctxt) f x

(********************pattern********************)
(* be cautious when use [pattern], [pattern1] is preferred *)
and pattern ctxt f x =
  let rec list_of_pattern acc = function
    (* only consider ((A|B)|C)*)
    | {ppat_desc = Ppat_or (p1, p2); ppat_attributes = []} ->
      list_of_pattern (p2 :: acc) p1
    | x -> x :: acc
  in
  if x.ppat_attributes <> [] then
    pp f "((%a)%a)" (pattern ctxt)
      {x with ppat_attributes = []}
      (attributes ctxt) x.ppat_attributes
  else
    match x.ppat_desc with
    | Ppat_alias (p, s) ->
      pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p protect_ident s.txt (* RA*)
    | Ppat_or _ ->
      (* *)
      pp f "@[<hov0>%a@]"
        (list ~sep:"@,|" (pattern ctxt))
        (list_of_pattern [] x)
    | _ -> pattern1 ctxt f x

and pattern1 ctxt (f : Format.formatter) (x : pattern) : unit =
  let rec pattern_list_helper f = function
    | {
        ppat_desc =
          Ppat_construct
            ( {txt = Lident "::"; _},
              Some {ppat_desc = Ppat_tuple [pat1; pat2]; _} );
        ppat_attributes = [];
      } ->
      pp f "%a::%a" (simple_pattern ctxt) pat1 pattern_list_helper pat2 (*RA*)
    | p -> pattern1 ctxt f p
  in
  if x.ppat_attributes <> [] then pattern ctxt f x
  else
    match x.ppat_desc with
    | Ppat_variant (l, Some p) ->
      pp f "@[<2>`%s@;%a@]" l (simple_pattern ctxt) p
    | Ppat_construct ({txt = Lident ("()" | "[]"); _}, _) ->
      simple_pattern ctxt f x
    | Ppat_construct (({txt; _} as li), po) -> (
      if
        (* FIXME The third field always false *)
        txt = Lident "::"
      then pp f "%a" pattern_list_helper x
      else
        match po with
        | Some x -> pp f "%a@;%a" longident_loc li (simple_pattern ctxt) x
        | None -> pp f "%a" longident_loc li)
    | _ -> simple_pattern ctxt f x

and simple_pattern ctxt (f : Format.formatter) (x : pattern) : unit =
  if x.ppat_attributes <> [] then pattern ctxt f x
  else
    match x.ppat_desc with
    | Ppat_construct ({txt = Lident (("()" | "[]") as x); _}, _) -> pp f "%s" x
    | Ppat_any -> pp f "_"
    | Ppat_var {txt; _} -> protect_ident f txt
    | Ppat_array l -> pp f "@[<2>[|%a|]@]" (list (pattern1 ctxt) ~sep:";") l
    | Ppat_unpack s -> pp f "(module@ %s)@ " s.txt
    | Ppat_type li -> pp f "#%a" longident_loc li
    | Ppat_record (l, closed) -> (
      let longident_x_pattern f (li, p, opt) =
        let opt_str = if opt then "?" else "" in
        match (li, p) with
        | ( {txt = Lident s; _},
            {ppat_desc = Ppat_var {txt; _}; ppat_attributes = []; _} )
          when s = txt ->
          pp f "@[<2>%a%s@]" longident_loc li opt_str
        | _ ->
          pp f "@[<2>%a%s@;=@;%a@]" longident_loc li opt_str (pattern1 ctxt) p
      in
      match closed with
      | Closed -> pp f "@[<2>{@;%a@;}@]" (list longident_x_pattern ~sep:";@;") l
      | _ -> pp f "@[<2>{@;%a;_}@]" (list longident_x_pattern ~sep:";@;") l)
    | Ppat_tuple l ->
      pp f "@[<1>(%a)@]" (list ~sep:",@;" (pattern1 ctxt)) l (* level1*)
    | Ppat_constant c -> pp f "%a" constant c
    | Ppat_interval (c1, c2) -> pp f "%a..%a" constant c1 constant c2
    | Ppat_variant (l, None) -> pp f "`%s" l
    | Ppat_constraint (p, ct) ->
      pp f "@[<2>(%a@;:@;%a)@]" (pattern1 ctxt) p (core_type ctxt) ct
    | Ppat_lazy p -> pp f "@[<2>(lazy@;%a)@]" (pattern1 ctxt) p
    | Ppat_exception p -> pp f "@[<2>exception@;%a@]" (pattern1 ctxt) p
    | Ppat_extension e -> extension ctxt f e
    | Ppat_open (lid, p) ->
      let with_paren =
        match p.ppat_desc with
        | Ppat_array _ | Ppat_record _
        | Ppat_construct ({txt = Lident ("()" | "[]"); _}, _) ->
          false
        | _ -> true
      in
      pp f "@[<2>%a.%a @]" longident_loc lid
        (paren with_paren @@ pattern1 ctxt)
        p
    | _ -> paren true (pattern ctxt) f x

and label_exp ctxt f (l, opt, p) =
  match l with
  | Nolabel ->
    (* single case pattern parens needed here *)
    pp f "%a@ " (simple_pattern ctxt) p
  | Optional {txt = rest} -> (
    match p with
    | {ppat_desc = Ppat_var {txt; _}; ppat_attributes = []} when txt = rest -> (
      match opt with
      | Some o -> pp f "?(%s=@;%a)@;" rest (expression ctxt) o
      | None -> pp f "?%s@ " rest)
    | _ -> (
      match opt with
      | Some o ->
        pp f "?%s:(%a=@;%a)@;" rest (pattern1 ctxt) p (expression ctxt) o
      | None -> pp f "?%s:%a@;" rest (simple_pattern ctxt) p))
  | Labelled {txt = l} -> (
    match p with
    | {ppat_desc = Ppat_var {txt; _}; ppat_attributes = []} when txt = l ->
      pp f "~%s@;" l
    | _ -> pp f "~%s:%a@;" l (simple_pattern ctxt) p)

and sugar_expr ctxt f e =
  if e.pexp_attributes <> [] then false
  else
    match e.pexp_desc with
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = id; _}; pexp_attributes = []; _};
          args;
        }
      when List.for_all (fun (lab, _) -> lab = Nolabel) args -> (
      let print_indexop a path_prefix assign left right print_index indices
          rem_args =
        let print_path ppf = function
          | None -> ()
          | Some m -> pp ppf ".%a" longident m
        in
        match (assign, rem_args) with
        | false, [] ->
          pp f "@[%a%a%s%a%s@]" (simple_expr ctxt) a print_path path_prefix left
            (list ~sep:"," print_index)
            indices right;
          true
        | true, [v] ->
          pp f "@[%a%a%s%a%s@ <-@;<1 2>%a@]" (simple_expr ctxt) a print_path
            path_prefix left
            (list ~sep:"," print_index)
            indices right (simple_expr ctxt) v;
          true
        | _ -> false
      in
      match (id, List.map snd args) with
      | Lident "!", [e] ->
        pp f "@[<hov>!%a@]" (simple_expr ctxt) e;
        true
      | Ldot (path, (("get" | "set") as func)), a :: other_args -> (
        let assign = func = "set" in
        let print = print_indexop a None assign in
        match (path, other_args) with
        | Lident "Array", i :: rest -> print ".(" ")" (expression ctxt) [i] rest
        | Lident "String", i :: rest ->
          print ".[" "]" (expression ctxt) [i] rest
        | Ldot (Lident "Bigarray", "Array1"), i1 :: rest ->
          print ".{" "}" (simple_expr ctxt) [i1] rest
        | Ldot (Lident "Bigarray", "Array2"), i1 :: i2 :: rest ->
          print ".{" "}" (simple_expr ctxt) [i1; i2] rest
        | Ldot (Lident "Bigarray", "Array3"), i1 :: i2 :: i3 :: rest ->
          print ".{" "}" (simple_expr ctxt) [i1; i2; i3] rest
        | ( Ldot (Lident "Bigarray", "Genarray"),
            {pexp_desc = Pexp_array indexes; pexp_attributes = []} :: rest ) ->
          print ".{" "}" (simple_expr ctxt) indexes rest
        | _ -> false)
      | (Lident s | Ldot (_, s)), a :: i :: rest when s.[0] = '.' ->
        let n = String.length s in
        (* extract operator:
           assignment operators end with [right_bracket ^ "<-"],
           access operators end with [right_bracket] directly
        *)
        let assign = s.[n - 1] = '-' in
        let kind =
          (* extract the right end bracket *)
          if assign then s.[n - 3] else s.[n - 1]
        in
        let left, right =
          match kind with
          | ')' -> ('(', ")")
          | ']' -> ('[', "]")
          | '}' -> ('{', "}")
          | _ -> assert false
        in
        let path_prefix =
          match id with
          | Ldot (m, _) -> Some m
          | _ -> None
        in
        let left = String.sub s 0 (1 + String.index s left) in
        print_indexop a path_prefix assign left right (expression ctxt) [i] rest
      | _ -> false)
    | _ -> false

and expression ctxt f x =
  if x.pexp_attributes <> [] then
    pp f "((%a)@,%a)" (expression ctxt)
      {x with pexp_attributes = []}
      (attributes ctxt) x.pexp_attributes
  else
    match x.pexp_desc with
    | (Pexp_fun _ | Pexp_match _ | Pexp_try _ | Pexp_sequence _)
      when ctxt.pipe || ctxt.semi ->
      paren true (expression reset_ctxt) f x
    | (Pexp_ifthenelse _ | Pexp_sequence _) when ctxt.ifthenelse ->
      paren true (expression reset_ctxt) f x
    | (Pexp_let _ | Pexp_letmodule _ | Pexp_open _ | Pexp_letexception _)
      when ctxt.semi ->
      paren true (expression reset_ctxt) f x
    | Pexp_fun {arg_label = l; default = e0; lhs = p; rhs = e; arity; async} ->
      let arity_str =
        match arity with
        | None -> ""
        | Some arity -> "[arity:" ^ string_of_int arity ^ "]"
      in
      let async_str = if async then "async " else "" in
      pp f "@[<2>%sfun@;%s%a->@;%a@]" async_str arity_str (label_exp ctxt)
        (l, e0, p) (expression ctxt) e
    | Pexp_match (e, l) ->
      pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]" (expression reset_ctxt) e
        (case_list ctxt) l
    | Pexp_try (e, l) ->
      pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
        (* "try@;@[<2>%a@]@\nwith@\n%a"*)
        (expression reset_ctxt)
        e (case_list ctxt) l
    | Pexp_let (rf, l, e) ->
      (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
         (*no indentation here, a new line*) *)
      (*   rec_flag rf *)
      pp f "@[<2>%a in@;<1 -2>%a@]" (bindings reset_ctxt) (rf, l)
        (expression ctxt) e
    | Pexp_apply {funct = e; args = l; partial} -> (
      if not (sugar_expr ctxt f x) then
        match view_fixity_of_exp e with
        | `Infix s -> (
          match l with
          | [((Nolabel, _) as arg1); ((Nolabel, _) as arg2)] ->
            (* FIXME associativity label_x_expression_param *)
            pp f "@[<2>%a@;%s@;%a@]"
              (label_x_expression_param reset_ctxt)
              arg1 s
              (label_x_expression_param ctxt)
              arg2
          | _ ->
            pp f "@[<2>%a %a@]" (simple_expr ctxt) e
              (list (label_x_expression_param ctxt))
              l)
        | `Prefix s -> (
          let s =
            if
              List.mem s ["~+"; "~-"; "~+."; "~-."]
              &&
              match l with
              (* See #7200: avoid turning (~- 1) into (- 1) which is
                 parsed as an int literal *)
              | [(_, {pexp_desc = Pexp_constant _})] -> false
              | _ -> true
            then String.sub s 1 (String.length s - 1)
            else s
          in
          match l with
          | [(Nolabel, x)] -> pp f "@[<2>%s@;%a@]" s (simple_expr ctxt) x
          | _ ->
            pp f "@[<2>%a %a@]" (simple_expr ctxt) e
              (list (label_x_expression_param ctxt))
              l)
        | _ ->
          let partial_str = if partial then " ..." else "" in
          pp f "@[<hov2>%a%s@]"
            (fun f (e, l) ->
              pp f "%a@ %a" (expression2 ctxt) e
                (list (label_x_expression_param reset_ctxt))
                l)
              (* reset here only because [function,match,try,sequence]
                 are lower priority *)
            (e, l) partial_str)
    | Pexp_construct (li, Some eo) when not (is_simple_construct (view_expr x))
      -> (
      (* Not efficient FIXME*)
      match view_expr x with
      | `cons ls -> list (simple_expr ctxt) f ls ~sep:"@;::@;"
      | `normal -> pp f "@[<2>%a@;%a@]" longident_loc li (simple_expr ctxt) eo
      | _ -> assert false)
    | Pexp_setfield (e1, li, e2) ->
      pp f "@[<2>%a.%a@ <-@ %a@]" (simple_expr ctxt) e1 longident_loc li
        (simple_expr ctxt) e2
    | Pexp_ifthenelse (e1, e2, eo) ->
      (* @;@[<2>else@ %a@]@] *)
      let fmt : (_, _, _) format =
        "@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]"
      in
      let expression_under_ifthenelse = expression (under_ifthenelse ctxt) in
      pp f fmt expression_under_ifthenelse e1 expression_under_ifthenelse e2
        (fun f eo ->
          match eo with
          | Some x -> pp f "@;@[<2>else@;%a@]" (expression (under_semi ctxt)) x
          | None -> () (* pp f "()" *))
        eo
    | Pexp_sequence _ ->
      let rec sequence_helper acc = function
        | {pexp_desc = Pexp_sequence (e1, e2); pexp_attributes = []} ->
          sequence_helper (e1 :: acc) e2
        | v -> List.rev (v :: acc)
      in
      let lst = sequence_helper [] x in
      pp f "@[<hv>%a@]" (list (expression (under_semi ctxt)) ~sep:";@;") lst
    | Pexp_letmodule (s, me, e) ->
      pp f "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]" s.txt
        (module_expr reset_ctxt) me (expression ctxt) e
    | Pexp_letexception (cd, e) ->
      pp f "@[<hov2>let@ exception@ %a@ in@ %a@]"
        (extension_constructor ctxt)
        cd (expression ctxt) e
    | Pexp_assert e -> pp f "@[<hov2>assert@ %a@]" (simple_expr ctxt) e
    | Pexp_lazy e -> pp f "@[<hov2>lazy@ %a@]" (simple_expr ctxt) e
    | Pexp_open (ovf, lid, e) ->
      pp f "@[<2>let open%s %a in@;%a@]" (override ovf) longident_loc lid
        (expression ctxt) e
    | Pexp_variant (l, Some eo) -> pp f "@[<2>`%s@;%a@]" l (simple_expr ctxt) eo
    | Pexp_extension e -> extension ctxt f e
    | _ -> expression1 ctxt f x

and expression1 ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x else expression2 ctxt f x
(* used in [Pexp_apply] *)

and expression2 ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x
  else
    match x.pexp_desc with
    | Pexp_field (e, li) ->
      pp f "@[<hov2>%a.%a@]" (simple_expr ctxt) e longident_loc li
    | Pexp_send (e, s) -> pp f "@[<hov2>%a#%s@]" (simple_expr ctxt) e s.txt
    | _ -> simple_expr ctxt f x

and simple_expr ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x
  else
    match x.pexp_desc with
    | Pexp_construct _ when is_simple_construct (view_expr x) -> (
      match view_expr x with
      | `nil -> pp f "[]"
      | `tuple -> pp f "()"
      | `list xs ->
        pp f "@[<hv0>[%a]@]" (list (expression (under_semi ctxt)) ~sep:";@;") xs
      | `simple x -> longident f x
      | _ -> assert false)
    | Pexp_ident li -> longident_loc f li
    (* (match view_fixity_of_exp x with *)
    (* |`Normal -> longident_loc f li *)
    (* | `Prefix _ | `Infix _ -> pp f "( %a )" longident_loc li) *)
    | Pexp_constant c -> constant f c
    | Pexp_pack me -> pp f "(module@;%a)" (module_expr ctxt) me
    | Pexp_newtype (lid, e) ->
      pp f "fun@;(type@;%s)@;->@;%a" lid.txt (expression ctxt) e
    | Pexp_tuple l ->
      pp f "@[<hov2>(%a)@]" (list (simple_expr ctxt) ~sep:",@;") l
    | Pexp_constraint (e, ct) ->
      pp f "(%a : %a)" (expression ctxt) e (core_type ctxt) ct
    | Pexp_coerce (e, (), ct) ->
      pp f "(%a :> %a)" (expression ctxt) e (core_type ctxt) ct
    | Pexp_variant (l, None) -> pp f "`%s" l
    | Pexp_record (l, eo) ->
      let longident_x_expression f (li, e, opt) =
        let opt_str = if opt then "?" else "" in
        match e with
        | {pexp_desc = Pexp_ident {txt; _}; pexp_attributes = []; _}
          when li.txt = txt ->
          pp f "@[<hov2>%a%s@]" longident_loc li opt_str
        | _ ->
          pp f "@[<hov2>%a@;=@;%s%a@]" longident_loc li opt_str
            (simple_expr ctxt) e
      in
      pp f "@[<hv0>@[<hv2>{@;%a%a@]@;}@]" (* "@[<hov2>{%a%a}@]" *)
        (option ~last:" with@;" (simple_expr ctxt))
        eo
        (list longident_x_expression ~sep:";@;")
        l
    | Pexp_array l ->
      pp f "@[<0>@[<2>[|%a|]@]@]"
        (list (simple_expr (under_semi ctxt)) ~sep:";")
        l
    | Pexp_while (e1, e2) ->
      let fmt : (_, _, _) format = "@[<2>while@;%a@;do@;%a@;done@]" in
      pp f fmt (expression ctxt) e1 (expression ctxt) e2
    | Pexp_for (s, e1, e2, df, e3) ->
      let fmt : (_, _, _) format =
        "@[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]"
      in
      let expression = expression ctxt in
      pp f fmt (pattern ctxt) s expression e1 direction_flag df expression e2
        expression e3
    | _ -> paren true (expression ctxt) f x

and attributes ctxt f l = List.iter (attribute ctxt f) l

and item_attributes ctxt f l = List.iter (item_attribute ctxt f) l

and attribute ctxt f (s, e) = pp f "@[<2>[@@%s@ %a]@]" s.txt (payload ctxt) e

and item_attribute ctxt f (s, e) =
  pp f "@[<2>[@@@@%s@ %a]@]" s.txt (payload ctxt) e

and floating_attribute ctxt f (s, e) =
  pp f "@[<2>[@@@@@@%s@ %a]@]" s.txt (payload ctxt) e

and value_description ctxt f x =
  (* note: value_description has an attribute field,
           but they're already printed by the callers this method *)
  pp f "@[<hov2>%a%a@]" (core_type ctxt) x.pval_type
    (fun f x ->
      if x.pval_prim <> [] then
        pp f "@ =@ %a" (list constant_string) x.pval_prim)
    x

and extension ctxt f (s, e) = pp f "@[<2>[%%%s@ %a]@]" s.txt (payload ctxt) e

and item_extension ctxt f (s, e) =
  pp f "@[<2>[%%%%%s@ %a]@]" s.txt (payload ctxt) e

and exception_declaration ctxt f ext =
  pp f "@[<hov2>exception@ %a@]" (extension_constructor ctxt) ext

and module_type ctxt f x =
  if x.pmty_attributes <> [] then
    pp f "((%a)%a)" (module_type ctxt)
      {x with pmty_attributes = []}
      (attributes ctxt) x.pmty_attributes
  else
    match x.pmty_desc with
    | Pmty_ident li -> pp f "%a" longident_loc li
    | Pmty_alias li -> pp f "(module %a)" longident_loc li
    | Pmty_signature s ->
      pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" (* "@[<hov>sig@ %a@ end@]" *)
        (list (signature_item ctxt))
        s (* FIXME wrong indentation*)
    | Pmty_functor (_, None, mt2) ->
      pp f "@[<hov2>functor () ->@ %a@]" (module_type ctxt) mt2
    | Pmty_functor (s, Some mt1, mt2) ->
      if s.txt = "_" then
        pp f "@[<hov2>%a@ ->@ %a@]" (module_type ctxt) mt1 (module_type ctxt)
          mt2
      else
        pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" s.txt (module_type ctxt)
          mt1 (module_type ctxt) mt2
    | Pmty_with (mt, l) -> (
      let with_constraint f = function
        | Pwith_type (li, ({ptype_params = ls; _} as td)) ->
          let ls = List.map fst ls in
          pp f "type@ %a %a =@ %a"
            (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
            ls longident_loc li (type_declaration ctxt) td
        | Pwith_module (li, li2) ->
          pp f "module %a =@ %a" longident_loc li longident_loc li2
        | Pwith_typesubst (li, ({ptype_params = ls; _} as td)) ->
          let ls = List.map fst ls in
          pp f "type@ %a %a :=@ %a"
            (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
            ls longident_loc li (type_declaration ctxt) td
        | Pwith_modsubst (li, li2) ->
          pp f "module %a :=@ %a" longident_loc li longident_loc li2
      in
      match l with
      | [] -> pp f "@[<hov2>%a@]" (module_type ctxt) mt
      | _ ->
        pp f "@[<hov2>(%a@ with@ %a)@]" (module_type ctxt) mt
          (list with_constraint ~sep:"@ and@ ")
          l)
    | Pmty_typeof me ->
      pp f "@[<hov2>module@ type@ of@ %a@]" (module_expr ctxt) me
    | Pmty_extension e -> extension ctxt f e

and signature ctxt f x = list ~sep:"@\n" (signature_item ctxt) f x

and signature_item ctxt f x : unit =
  match x.psig_desc with
  | Psig_type (rf, l) -> type_def_list ctxt f (rf, l)
  | Psig_value vd ->
    let intro = if vd.pval_prim = [] then "val" else "external" in
    pp f "@[<2>%s@ %a@ :@ %a@]%a" intro protect_ident vd.pval_name.txt
      (value_description ctxt) vd (item_attributes ctxt) vd.pval_attributes
  | Psig_typext te -> type_extension ctxt f te
  | Psig_exception ed -> exception_declaration ctxt f ed
  | Psig_module
      ({pmd_type = {pmty_desc = Pmty_alias alias; pmty_attributes = []; _}; _}
       as pmd) ->
    pp f "@[<hov>module@ %s@ =@ %a@]%a" pmd.pmd_name.txt longident_loc alias
      (item_attributes ctxt) pmd.pmd_attributes
  | Psig_module pmd ->
    pp f "@[<hov>module@ %s@ :@ %a@]%a" pmd.pmd_name.txt (module_type ctxt)
      pmd.pmd_type (item_attributes ctxt) pmd.pmd_attributes
  | Psig_open od ->
    pp f "@[<hov2>open%s@ %a@]%a"
      (override od.popen_override)
      longident_loc od.popen_lid (item_attributes ctxt) od.popen_attributes
  | Psig_include incl ->
    pp f "@[<hov2>include@ %a@]%a" (module_type ctxt) incl.pincl_mod
      (item_attributes ctxt) incl.pincl_attributes
  | Psig_modtype {pmtd_name = s; pmtd_type = md; pmtd_attributes = attrs} ->
    pp f "@[<hov2>module@ type@ %s%a@]%a" s.txt
      (fun f md ->
        match md with
        | None -> ()
        | Some mt ->
          pp_print_space f ();
          pp f "@ =@ %a" (module_type ctxt) mt)
      md (item_attributes ctxt) attrs
  | Psig_recmodule decls ->
    let rec string_x_module_type_list f ?(first = true) l =
      match l with
      | [] -> ()
      | pmd :: tl ->
        if not first then
          pp f "@ @[<hov2>and@ %s:@ %a@]%a" pmd.pmd_name.txt (module_type ctxt)
            pmd.pmd_type (item_attributes ctxt) pmd.pmd_attributes
        else
          pp f "@[<hov2>module@ rec@ %s:@ %a@]%a" pmd.pmd_name.txt
            (module_type ctxt) pmd.pmd_type (item_attributes ctxt)
            pmd.pmd_attributes;
        string_x_module_type_list f ~first:false tl
    in
    string_x_module_type_list f decls
  | Psig_attribute a -> floating_attribute ctxt f a
  | Psig_extension (e, a) ->
    item_extension ctxt f e;
    item_attributes ctxt f a

and module_expr ctxt f x =
  if x.pmod_attributes <> [] then
    pp f "((%a)%a)" (module_expr ctxt)
      {x with pmod_attributes = []}
      (attributes ctxt) x.pmod_attributes
  else
    match x.pmod_desc with
    | Pmod_structure s ->
      pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]"
        (list (structure_item ctxt) ~sep:"@\n")
        s
    | Pmod_constraint (me, mt) ->
      pp f "@[<hov2>(%a@ :@ %a)@]" (module_expr ctxt) me (module_type ctxt) mt
    | Pmod_ident li -> pp f "%a" longident_loc li
    | Pmod_functor (_, None, me) ->
      pp f "functor ()@;->@;%a" (module_expr ctxt) me
    | Pmod_functor (s, Some mt, me) ->
      pp f "functor@ (%s@ :@ %a)@;->@;%a" s.txt (module_type ctxt) mt
        (module_expr ctxt) me
    | Pmod_apply (me1, me2) ->
      pp f "(%a)(%a)" (module_expr ctxt) me1 (module_expr ctxt) me2
      (* Cf: #7200 *)
    | Pmod_unpack e -> pp f "(val@ %a)" (expression ctxt) e
    | Pmod_extension e -> extension ctxt f e

and structure ctxt f x = list ~sep:"@\n" (structure_item ctxt) f x

and payload ctxt f = function
  | PStr [{pstr_desc = Pstr_eval (e, attrs)}] ->
    pp f "@[<2>%a@]%a" (expression ctxt) e (item_attributes ctxt) attrs
  | PStr x -> structure ctxt f x
  | PTyp x ->
    pp f ":";
    core_type ctxt f x
  | PSig x ->
    pp f ":";
    signature ctxt f x
  | PPat (x, None) ->
    pp f "?";
    pattern ctxt f x
  | PPat (x, Some e) ->
    pp f "?";
    pattern ctxt f x;
    pp f " when ";
    expression ctxt f e

(* transform [f = fun g h -> ..] to [f g h = ... ] could be improved *)
and binding ctxt f {pvb_pat = p; pvb_expr = x; _} =
  (* .pvb_attributes have already been printed by the caller, #bindings *)
  let rec pp_print_pexp_function f x =
    if x.pexp_attributes <> [] then pp f "=@;%a" (expression ctxt) x
    else
      match x.pexp_desc with
      | Pexp_fun
          {arg_label = label; default = eo; lhs = p; rhs = e; arity; async} ->
        let arity_str =
          match arity with
          | None -> ""
          | Some arity -> "[arity:" ^ string_of_int arity ^ "]"
        in
        let async_str = if async then "async " else "" in
        if label = Nolabel then
          pp f "%s%s%a@ %a" async_str arity_str (simple_pattern ctxt) p
            pp_print_pexp_function e
        else
          pp f "%s%s%a@ %a" async_str arity_str (label_exp ctxt) (label, eo, p)
            pp_print_pexp_function e
      | Pexp_newtype (str, e) ->
        pp f "(type@ %s)@ %a" str.txt pp_print_pexp_function e
      | _ -> pp f "=@;%a" (expression ctxt) x
  in
  let tyvars_str tyvars = List.map (fun v -> v.txt) tyvars in
  let is_desugared_gadt p e =
    let gadt_pattern =
      match p with
      | {
       ppat_desc =
         Ppat_constraint
           ( ({ppat_desc = Ppat_var _} as pat),
             {ptyp_desc = Ptyp_poly (args_tyvars, rt)} );
       ppat_attributes = [];
      } ->
        Some (pat, args_tyvars, rt)
      | _ -> None
    in
    let rec gadt_exp tyvars e =
      match e with
      | {pexp_desc = Pexp_newtype (tyvar, e); pexp_attributes = []} ->
        gadt_exp (tyvar :: tyvars) e
      | {pexp_desc = Pexp_constraint (e, ct); pexp_attributes = []} ->
        Some (List.rev tyvars, e, ct)
      | _ -> None
    in
    let gadt_exp = gadt_exp [] e in
    match (gadt_pattern, gadt_exp) with
    | Some (p, pt_tyvars, pt_ct), Some (e_tyvars, e, e_ct)
      when tyvars_str pt_tyvars = tyvars_str e_tyvars ->
      let ety = Typ.varify_constructors e_tyvars e_ct in
      if ety = pt_ct then Some (p, pt_tyvars, e_ct, e) else None
    | _ -> None
  in
  if x.pexp_attributes <> [] then
    pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
  else
    match is_desugared_gadt p x with
    | Some (p, [], ct, e) ->
      pp f "%a@;: %a@;=@;%a" (simple_pattern ctxt) p (core_type ctxt) ct
        (expression ctxt) e
    | Some (p, tyvars, ct, e) ->
      pp f "%a@;: type@;%a.@;%a@;=@;%a" (simple_pattern ctxt) p
        (list pp_print_string ~sep:"@;")
        (tyvars_str tyvars) (core_type ctxt) ct (expression ctxt) e
    | None -> (
      match p with
      | {ppat_desc = Ppat_constraint (p, ty); ppat_attributes = []} -> (
        (* special case for the first*)
        match ty with
        | {ptyp_desc = Ptyp_poly _; ptyp_attributes = []} ->
          pp f "%a@;:@;%a@;=@;%a" (simple_pattern ctxt) p (core_type ctxt) ty
            (expression ctxt) x
        | _ ->
          pp f "(%a@;:@;%a)@;=@;%a" (simple_pattern ctxt) p (core_type ctxt) ty
            (expression ctxt) x)
      | {ppat_desc = Ppat_var _; ppat_attributes = []} ->
        pp f "%a@ %a" (simple_pattern ctxt) p pp_print_pexp_function x
      | _ -> pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x)

(* [in] is not printed *)
and bindings ctxt f (rf, l) =
  let binding kwd rf f x =
    pp f "@[<2>%s %a%a@]%a" kwd rec_flag rf (binding ctxt) x
      (item_attributes ctxt) x.pvb_attributes
  in
  match l with
  | [] -> ()
  | [x] -> binding "let" rf f x
  | x :: xs ->
    pp f "@[<v>%a@,%a@]" (binding "let" rf) x
      (list ~sep:"@," (binding "and" Nonrecursive))
      xs

and structure_item ctxt f x =
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
    pp f "@[<hov2>;;%a@]%a" (expression ctxt) e (item_attributes ctxt) attrs
  | Pstr_type (_, []) -> assert false
  | Pstr_type (rf, l) -> type_def_list ctxt f (rf, l)
  | Pstr_value (rf, l) ->
    (* pp f "@[<hov2>let %a%a@]"  rec_flag rf bindings l *)
    pp f "@[<2>%a@]" (bindings ctxt) (rf, l)
  | Pstr_typext te -> type_extension ctxt f te
  | Pstr_exception ed -> exception_declaration ctxt f ed
  | Pstr_module x ->
    let rec module_helper = function
      | {pmod_desc = Pmod_functor (s, mt, me'); pmod_attributes = []} ->
        if mt = None then pp f "()"
        else Misc.may (pp f "(%s:%a)" s.txt (module_type ctxt)) mt;
        module_helper me'
      | me -> me
    in
    pp f "@[<hov2>module %s%a@]%a" x.pmb_name.txt
      (fun f me ->
        let me = module_helper me in
        match me with
        | {
         pmod_desc =
           Pmod_constraint
             (me', ({pmty_desc = Pmty_ident _ | Pmty_signature _; _} as mt));
         pmod_attributes = [];
        } ->
          pp f " :@;%a@;=@;%a@;" (module_type ctxt) mt (module_expr ctxt) me'
        | _ -> pp f " =@ %a" (module_expr ctxt) me)
      x.pmb_expr (item_attributes ctxt) x.pmb_attributes
  | Pstr_open od ->
    pp f "@[<2>open%s@;%a@]%a"
      (override od.popen_override)
      longident_loc od.popen_lid (item_attributes ctxt) od.popen_attributes
  | Pstr_modtype {pmtd_name = s; pmtd_type = md; pmtd_attributes = attrs} ->
    pp f "@[<hov2>module@ type@ %s%a@]%a" s.txt
      (fun f md ->
        match md with
        | None -> ()
        | Some mt ->
          pp_print_space f ();
          pp f "@ =@ %a" (module_type ctxt) mt)
      md (item_attributes ctxt) attrs
  | Pstr_primitive vd ->
    pp f "@[<hov2>external@ %a@ :@ %a@]%a" protect_ident vd.pval_name.txt
      (value_description ctxt) vd (item_attributes ctxt) vd.pval_attributes
  | Pstr_include incl ->
    pp f "@[<hov2>include@ %a@]%a" (module_expr ctxt) incl.pincl_mod
      (item_attributes ctxt) incl.pincl_attributes
  | Pstr_recmodule decls -> (
    (* 3.07 *)
    let aux f = function
      | {pmb_expr = {pmod_desc = Pmod_constraint (expr, typ)}} as pmb ->
        pp f "@[<hov2>@ and@ %s:%a@ =@ %a@]%a" pmb.pmb_name.txt
          (module_type ctxt) typ (module_expr ctxt) expr (item_attributes ctxt)
          pmb.pmb_attributes
      | _ -> assert false
    in
    match decls with
    | ({pmb_expr = {pmod_desc = Pmod_constraint (expr, typ)}} as pmb) :: l2 ->
      pp f "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]%a@ %a@]" pmb.pmb_name.txt
        (module_type ctxt) typ (module_expr ctxt) expr (item_attributes ctxt)
        pmb.pmb_attributes
        (fun f l2 -> List.iter (aux f) l2)
        l2
    | _ -> assert false)
  | Pstr_attribute a -> floating_attribute ctxt f a
  | Pstr_extension (e, a) ->
    item_extension ctxt f e;
    item_attributes ctxt f a

and type_param ctxt f (ct, a) =
  pp f "%s%a" (type_variance a) (core_type ctxt) ct

and type_params ctxt f = function
  | [] -> ()
  | l -> pp f "%a " (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",@;") l

and type_def_list ctxt f (rf, l) =
  let type_decl kwd rf f x =
    let eq =
      if x.ptype_kind = Ptype_abstract && x.ptype_manifest = None then ""
      else " ="
    in
    pp f "@[<2>%s %a%a%s%s%a@]%a" kwd nonrec_flag rf (type_params ctxt)
      x.ptype_params x.ptype_name.txt eq (type_declaration ctxt) x
      (item_attributes ctxt) x.ptype_attributes
  in
  match l with
  | [] -> assert false
  | [x] -> type_decl "type" rf f x
  | x :: xs ->
    pp f "@[<v>%a@,%a@]" (type_decl "type" rf) x
      (list ~sep:"@," (type_decl "and" Recursive))
      xs

and record_declaration ctxt f lbls =
  let type_record_field f pld =
    pp f "@[<2>%a%s%a:@;%a@;%a@]" mutable_flag pld.pld_mutable pld.pld_name.txt
      optional_flag pld.pld_optional (core_type ctxt) pld.pld_type
      (attributes ctxt) pld.pld_attributes
  in
  pp f "{@\n%a}" (list type_record_field ~sep:";@\n") lbls

and type_declaration ctxt f x =
  (* type_declaration has an attribute field,
     but it's been printed by the caller of this method *)
  let priv f =
    match x.ptype_private with
    | Public -> ()
    | Private -> pp f "@;private"
  in
  let manifest f =
    match x.ptype_manifest with
    | None -> ()
    | Some y ->
      if x.ptype_kind = Ptype_abstract then
        pp f "%t@;%a" priv (core_type ctxt) y
      else pp f "@;%a" (core_type ctxt) y
  in
  let constructor_declaration f pcd =
    pp f "|@;";
    constructor_declaration ctxt f
      (pcd.pcd_name.txt, pcd.pcd_args, pcd.pcd_res, pcd.pcd_attributes)
  in
  let repr f =
    let intro f = if x.ptype_manifest = None then () else pp f "@;=" in
    match x.ptype_kind with
    | Ptype_variant xs ->
      pp f "%t%t@\n%a" intro priv (list ~sep:"@\n" constructor_declaration) xs
    | Ptype_abstract -> ()
    | Ptype_record l -> pp f "%t%t@;%a" intro priv (record_declaration ctxt) l
    | Ptype_open -> pp f "%t%t@;.." intro priv
  in
  let constraints f =
    List.iter
      (fun (ct1, ct2, _) ->
        pp f "@[<hov2>@ constraint@ %a@ =@ %a@]" (core_type ctxt) ct1
          (core_type ctxt) ct2)
      x.ptype_cstrs
  in
  pp f "%t%t%t" manifest repr constraints

and type_extension ctxt f x =
  let extension_constructor f x =
    pp f "@\n|@;%a" (extension_constructor ctxt) x
  in
  pp f "@[<2>type %a%a += %a@ %a@]%a"
    (fun f -> function
      | [] -> ()
      | l ->
        pp f "%a@;" (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",") l)
    x.ptyext_params longident_loc x.ptyext_path private_flag
    x.ptyext_private (* Cf: #7200 *)
    (list ~sep:"" extension_constructor)
    x.ptyext_constructors (item_attributes ctxt) x.ptyext_attributes

and constructor_declaration ctxt f (name, args, res, attrs) =
  let name =
    match name with
    | "::" -> "(::)"
    | s -> s
  in
  match res with
  | None ->
    pp f "%s%a@;%a" name
      (fun f -> function
        | Pcstr_tuple [] -> ()
        | Pcstr_tuple l ->
          pp f "@;of@;%a" (list (core_type1 ctxt) ~sep:"@;*@;") l
        | Pcstr_record l -> pp f "@;of@;%a" (record_declaration ctxt) l)
      args (attributes ctxt) attrs
  | Some r ->
    pp f "%s:@;%a@;%a" name
      (fun f -> function
        | Pcstr_tuple [] -> core_type1 ctxt f r
        | Pcstr_tuple l ->
          pp f "%a@;->@;%a"
            (list (core_type1 ctxt) ~sep:"@;*@;")
            l (core_type1 ctxt) r
        | Pcstr_record l ->
          pp f "%a@;->@;%a" (record_declaration ctxt) l (core_type1 ctxt) r)
      args (attributes ctxt) attrs

and extension_constructor ctxt f x =
  (* Cf: #7200 *)
  match x.pext_kind with
  | Pext_decl (l, r) ->
    constructor_declaration ctxt f (x.pext_name.txt, l, r, x.pext_attributes)
  | Pext_rebind li ->
    pp f "%s%a@;=@;%a" x.pext_name.txt (attributes ctxt) x.pext_attributes
      longident_loc li

and case_list ctxt f l : unit =
  let aux f {pc_lhs; pc_guard; pc_rhs} =
    pp f "@;| @[<2>%a%a@;->@;%a@]" (pattern ctxt) pc_lhs
      (option (expression ctxt) ~first:"@;when@;")
      pc_guard
      (expression (under_pipe ctxt))
      pc_rhs
  in
  list aux f l ~sep:""

and label_x_expression_param ctxt f (l, e) =
  let simple_name =
    match e with
    | {pexp_desc = Pexp_ident {txt = Lident l; _}; pexp_attributes = []} ->
      Some l
    | _ -> None
  in
  match l with
  | Nolabel -> expression2 ctxt f e (* level 2*)
  | Optional {txt = str} ->
    if Some str = simple_name then pp f "?%s" str
    else pp f "?%s:%a" str (simple_expr ctxt) e
  | Labelled {txt = lbl} ->
    if Some lbl = simple_name then pp f "~%s" lbl
    else pp f "~%s:%a" lbl (simple_expr ctxt) e

let expression f x = pp f "@[%a@]" (expression reset_ctxt) x

let string_of_expression x =
  ignore (flush_str_formatter ());
  let f = str_formatter in
  expression f x;
  flush_str_formatter ()

let string_of_structure x =
  ignore (flush_str_formatter ());
  let f = str_formatter in
  structure reset_ctxt f x;
  flush_str_formatter ()

let core_type = core_type reset_ctxt
let pattern = pattern reset_ctxt
let signature = signature reset_ctxt
let structure = structure reset_ctxt
