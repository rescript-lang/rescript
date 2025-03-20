module Doc = Res_doc
module CommentTable = Res_comments_table
module Comment = Res_comment
module Token = Res_token
module Parens = Res_parens
module ParsetreeViewer = Res_parsetree_viewer

type callback_style =
  (* regular arrow function, example: `let f = x => x + 1` *)
  | NoCallback
  (* `Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument))` *)
  | FitsOnOneLine
  (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
   *   MyModuleBlah.toList(argument)
   * )
   *)
  | ArgumentsFitOnOneLine

let add_parens doc =
  Doc.group
    (Doc.concat
       [
         Doc.lparen;
         Doc.indent (Doc.concat [Doc.soft_line; doc]);
         Doc.soft_line;
         Doc.rparen;
       ])

let add_braces doc =
  Doc.group
    (Doc.concat
       [
         Doc.lbrace;
         Doc.indent (Doc.concat [Doc.soft_line; doc]);
         Doc.soft_line;
         Doc.rbrace;
       ])

let add_async doc = Doc.concat [Doc.text "async "; doc]

let get_first_leading_comment tbl loc =
  match Hashtbl.find tbl.CommentTable.leading loc with
  | comment :: _ -> Some comment
  | [] -> None
  | exception Not_found -> None

(* Checks if `loc` has a leading line comment, i.e. `// comment above`*)
let has_leading_line_comment tbl loc =
  match get_first_leading_comment tbl loc with
  | Some comment -> Comment.is_single_line_comment comment
  | None -> false

let has_comment_below tbl loc =
  match Hashtbl.find tbl.CommentTable.trailing loc with
  | comment :: _ ->
    let comment_loc = Comment.loc comment in
    comment_loc.Location.loc_start.pos_lnum > loc.Location.loc_end.pos_lnum
  | [] -> false
  | exception Not_found -> false

let has_nested_jsx_or_more_than_one_child expr =
  let rec loop in_recursion expr =
    match expr.Parsetree.pexp_desc with
    | Pexp_construct
        ({txt = Longident.Lident "::"}, Some {pexp_desc = Pexp_tuple [hd; tail]})
      ->
      if in_recursion || ParsetreeViewer.is_jsx_expression hd then true
      else loop true tail
    | _ -> false
  in
  loop false expr

let has_comments_inside tbl loc =
  match Hashtbl.find_opt tbl.CommentTable.inside loc with
  | None -> false
  | _ -> true

let has_trailing_comments tbl loc =
  match Hashtbl.find_opt tbl.CommentTable.trailing loc with
  | None -> false
  | _ -> true

let print_multiline_comment_content txt =
  (* Turns
   *         |* first line
   *  * second line
   *      * third line *|
   * Into
   * |* first line
   *  * second line
   *  * third line *|
   *
   * What makes a comment suitable for this kind of indentation?
   *  ->  multiple lines + every line starts with a star
   *)
  let rec indent_stars lines acc =
    match lines with
    | [] -> Doc.nil
    | [last_line] ->
      let line = String.trim last_line in
      let doc = Doc.text (" " ^ line) in
      let trailing_space = if line = "" then Doc.nil else Doc.space in
      List.rev (trailing_space :: doc :: acc) |> Doc.concat
    | line :: lines ->
      let line = String.trim line in
      if line != "" && String.unsafe_get line 0 == '*' then
        let doc = Doc.text (" " ^ line) in
        indent_stars lines (Doc.hard_line :: doc :: acc)
      else
        let trailing_space =
          let len = String.length txt in
          if len > 0 && String.unsafe_get txt (len - 1) = ' ' then Doc.space
          else Doc.nil
        in
        let content = Comment.trim_spaces txt in
        Doc.concat [Doc.text content; trailing_space]
  in
  let lines = String.split_on_char '\n' txt in
  match lines with
  | [] -> Doc.text "/* */"
  | [line] ->
    Doc.concat
      [Doc.text "/* "; Doc.text (Comment.trim_spaces line); Doc.text " */"]
  | first :: rest ->
    let first_line = Comment.trim_spaces first in
    Doc.concat
      [
        Doc.text "/*";
        (match first_line with
        | "" | "*" -> Doc.nil
        | _ -> Doc.space);
        indent_stars rest [Doc.hard_line; Doc.text first_line];
        Doc.text "*/";
      ]

let print_trailing_comment (prev_loc : Location.t) (node_loc : Location.t)
    comment =
  let single_line = Comment.is_single_line_comment comment in
  let content =
    let txt = Comment.txt comment in
    if single_line then Doc.text ("//" ^ txt)
    else print_multiline_comment_content txt
  in
  let diff =
    let cmt_start = (Comment.loc comment).loc_start in
    cmt_start.pos_lnum - prev_loc.loc_end.pos_lnum
  in
  let is_below =
    (Comment.loc comment).loc_start.pos_lnum > node_loc.loc_end.pos_lnum
  in
  if diff > 0 || is_below then
    Doc.concat
      [
        Doc.break_parent;
        Doc.line_suffix
          (Doc.concat
             [
               Doc.hard_line;
               (if diff > 1 then Doc.hard_line else Doc.nil);
               content;
             ]);
      ]
  else if not single_line then Doc.concat [Doc.space; content]
  else Doc.line_suffix (Doc.concat [Doc.space; content])

let print_leading_comment ?next_comment comment =
  let single_line = Comment.is_single_line_comment comment in
  let content =
    let txt = Comment.txt comment in
    if single_line then Doc.text ("//" ^ txt)
    else print_multiline_comment_content txt
  in
  let separator =
    Doc.concat
      [
        (if single_line then Doc.concat [Doc.hard_line; Doc.break_parent]
         else Doc.nil);
        (match next_comment with
        | Some next ->
          let next_loc = Comment.loc next in
          let curr_loc = Comment.loc comment in
          let diff =
            next_loc.Location.loc_start.pos_lnum
            - curr_loc.Location.loc_end.pos_lnum
          in
          let next_single_line = Comment.is_single_line_comment next in
          if single_line && next_single_line then
            if diff > 1 then Doc.hard_line else Doc.nil
          else if single_line && not next_single_line then
            if diff > 1 then Doc.hard_line else Doc.nil
          else if diff > 1 then Doc.concat [Doc.hard_line; Doc.hard_line]
          else if diff == 1 then Doc.hard_line
          else Doc.space
        | None -> Doc.nil);
      ]
  in
  Doc.concat [content; separator]

(* This function is used for printing comments inside an empty block *)
let print_comments_inside cmt_tbl loc =
  let print_comment comment =
    let single_line = Comment.is_single_line_comment comment in
    let txt = Comment.txt comment in
    if single_line then Doc.text ("//" ^ txt)
    else print_multiline_comment_content txt
  in
  let force_break =
    loc.Location.loc_start.pos_lnum <> loc.Location.loc_end.pos_lnum
  in
  let rec loop acc comments =
    match comments with
    | [] -> Doc.nil
    | [comment] ->
      let cmt_doc = print_comment comment in
      let cmts_doc = Doc.concat (Doc.soft_line :: List.rev (cmt_doc :: acc)) in
      let doc =
        Doc.breakable_group ~force_break
          (Doc.concat
             [Doc.if_breaks (Doc.indent cmts_doc) cmts_doc; Doc.soft_line])
      in
      doc
    | comment :: rest ->
      let cmt_doc = Doc.concat [print_comment comment; Doc.line] in
      loop (cmt_doc :: acc) rest
  in
  match Hashtbl.find cmt_tbl.CommentTable.inside loc with
  | exception Not_found -> Doc.nil
  | comments ->
    Hashtbl.remove cmt_tbl.inside loc;
    loop [] comments

(* This function is used for printing comments inside an empty file *)
let print_comments_inside_file cmt_tbl =
  let rec loop acc comments =
    match comments with
    | [] -> Doc.nil
    | [comment] ->
      let cmt_doc = print_leading_comment comment in
      let doc =
        Doc.group (Doc.concat [Doc.concat (List.rev (cmt_doc :: acc))])
      in
      doc
    | comment :: (next_comment :: _comments as rest) ->
      let cmt_doc = print_leading_comment ~next_comment comment in
      loop (cmt_doc :: acc) rest
  in
  match Hashtbl.find cmt_tbl.CommentTable.inside Location.none with
  | exception Not_found -> Doc.nil
  | comments ->
    Hashtbl.remove cmt_tbl.inside Location.none;
    Doc.group (loop [] comments)

let print_leading_comments node tbl loc =
  let rec loop acc comments =
    match comments with
    | [] -> node
    | [comment] ->
      let cmt_doc = print_leading_comment comment in
      let diff =
        loc.Location.loc_start.pos_lnum
        - (Comment.loc comment).Location.loc_end.pos_lnum
      in
      let separator =
        if Comment.is_single_line_comment comment then
          if diff > 1 then Doc.hard_line else Doc.nil
        else if diff == 0 then Doc.space
        else if diff > 1 then Doc.concat [Doc.hard_line; Doc.hard_line]
        else Doc.hard_line
      in
      let doc =
        Doc.group
          (Doc.concat [Doc.concat (List.rev (cmt_doc :: acc)); separator; node])
      in
      doc
    | comment :: (next_comment :: _comments as rest) ->
      let cmt_doc = print_leading_comment ~next_comment comment in
      loop (cmt_doc :: acc) rest
  in
  match Hashtbl.find tbl loc with
  | exception Not_found -> node
  | comments ->
    (* Remove comments from tbl: Some ast nodes have the same location.
     * We only want to print comments once *)
    Hashtbl.remove tbl loc;
    loop [] comments

let print_trailing_comments node tbl loc =
  let rec loop prev acc comments =
    match comments with
    | [] -> Doc.concat (List.rev acc)
    | comment :: comments ->
      let cmt_doc = print_trailing_comment prev loc comment in
      loop (Comment.loc comment) (cmt_doc :: acc) comments
  in
  match Hashtbl.find tbl loc with
  | exception Not_found -> node
  | [] -> node
  | _first :: _ as comments ->
    (* Remove comments from tbl: Some ast nodes have the same location.
     * We only want to print comments once *)
    Hashtbl.remove tbl loc;
    let cmts_doc = loop loc [] comments in
    Doc.concat [node; cmts_doc]

let print_comments doc (tbl : CommentTable.t) loc =
  let doc_with_leading_comments = print_leading_comments doc tbl.leading loc in
  print_trailing_comments doc_with_leading_comments tbl.trailing loc

let print_list ~get_loc ~nodes ~print ?(force_break = false) t =
  let rec loop (prev_loc : Location.t) acc nodes =
    match nodes with
    | [] -> (prev_loc, Doc.concat (List.rev acc))
    | node :: nodes ->
      let loc = get_loc node in
      let start_pos =
        match get_first_leading_comment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
      in
      let sep =
        if start_pos.pos_lnum - prev_loc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hard_line; Doc.hard_line]
        else Doc.hard_line
      in
      let doc = print_comments (print node t) t loc in
      loop loc (doc :: sep :: acc) nodes
  in
  match nodes with
  | [] -> Doc.nil
  | node :: nodes ->
    let first_loc = get_loc node in
    let doc = print_comments (print node t) t first_loc in
    let last_loc, docs = loop first_loc [doc] nodes in
    let force_break =
      force_break || first_loc.loc_start.pos_lnum != last_loc.loc_end.pos_lnum
    in
    Doc.breakable_group ~force_break docs

let print_listi ~get_loc ~nodes ~print ?(force_break = false) t =
  let rec loop i (prev_loc : Location.t) acc nodes =
    match nodes with
    | [] -> (prev_loc, Doc.concat (List.rev acc))
    | node :: nodes ->
      let loc = get_loc node in
      let start_pos =
        match get_first_leading_comment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
      in
      let sep =
        if start_pos.pos_lnum - prev_loc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hard_line; Doc.hard_line]
        else Doc.line
      in
      let doc = print_comments (print node t i) t loc in
      loop (i + 1) loc (doc :: sep :: acc) nodes
  in
  match nodes with
  | [] -> Doc.nil
  | node :: nodes ->
    let first_loc = get_loc node in
    let doc = print_comments (print node t 0) t first_loc in
    let last_loc, docs = loop 1 first_loc [doc] nodes in
    let force_break =
      force_break || first_loc.loc_start.pos_lnum != last_loc.loc_end.pos_lnum
    in
    Doc.breakable_group ~force_break docs

let rec print_longident_aux accu = function
  | Longident.Lident s -> Doc.text s :: accu
  | Ldot (lid, s) -> print_longident_aux (Doc.text s :: accu) lid
  | Lapply (lid1, lid2) ->
    let d1 = Doc.join ~sep:Doc.dot (print_longident_aux [] lid1) in
    let d2 = Doc.join ~sep:Doc.dot (print_longident_aux [] lid2) in
    Doc.concat [d1; Doc.lparen; d2; Doc.rparen] :: accu

let print_longident = function
  | Longident.Lident txt -> Doc.text txt
  | lid -> Doc.join ~sep:Doc.dot (print_longident_aux [] lid)

type identifier_style = UppercaseExoticIdent | ExoticIdent | NormalIdent

let classify_ident_content ?(allow_uident = false) ?(allow_hyphen = false) txt =
  if Token.is_keyword_txt txt then ExoticIdent
  else
    let len = String.length txt in
    let rec loop i =
      if i == len then NormalIdent
      else if i == 0 then
        match String.unsafe_get txt i with
        | '\\' -> UppercaseExoticIdent
        | 'A' .. 'Z' when allow_uident -> loop (i + 1)
        | 'a' .. 'z' | '_' -> loop (i + 1)
        | _ -> ExoticIdent
      else
        match String.unsafe_get txt i with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' -> loop (i + 1)
        | '-' when allow_hyphen -> loop (i + 1)
        | _ -> ExoticIdent
    in
    loop 0

let print_ident_like ?allow_uident ?allow_hyphen txt =
  match classify_ident_content ?allow_uident ?allow_hyphen txt with
  | ExoticIdent -> Doc.concat [Doc.text "\\\""; Doc.text txt; Doc.text "\""]
  | UppercaseExoticIdent | NormalIdent -> Doc.text txt

let rec unsafe_for_all_range s ~start ~finish p =
  start > finish
  || p (String.unsafe_get s start)
     && unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start p =
  let len = String.length s in
  unsafe_for_all_range s ~start ~finish:(len - 1) p

(* See https://github.com/rescript-lang/rescript-compiler/blob/726cfa534314b586e5b5734471bc2023ad99ebd9/jscomp/ext/ext_string.ml#L510 *)
let is_valid_numeric_polyvar_number (x : string) =
  let len = String.length x in
  len > 0
  &&
  let a = Char.code (String.unsafe_get x 0) in
  a <= 57
  &&
  if len > 1 then
    a > 48
    && for_all_from x 1 (function
         | '0' .. '9' -> true
         | _ -> false)
  else a >= 48

(* Exotic identifiers in poly-vars have a "lighter" syntax: #"ease-in" *)
let print_poly_var_ident txt =
  (* numeric poly-vars don't need quotes: #644 *)
  if is_valid_numeric_polyvar_number txt then Doc.text txt
  else
    match classify_ident_content ~allow_uident:true txt with
    | UppercaseExoticIdent ->
      let len = String.length txt in
      (* UppercaseExoticIdent follows the \"..." format,
         so removing the leading backslash is enough to transform it into polyvar style *)
      Doc.text ((String.sub [@doesNotRaise]) txt 1 (len - 1))
    | ExoticIdent -> Doc.concat [Doc.text "\""; Doc.text txt; Doc.text "\""]
    | NormalIdent -> (
      match txt with
      | "" -> Doc.concat [Doc.text "\""; Doc.text txt; Doc.text "\""]
      | _ -> Doc.text txt)

let polyvar_ident_to_string poly_var_ident =
  Doc.concat [Doc.text "#"; print_poly_var_ident poly_var_ident]
  |> Doc.to_string ~width:80

let find_inline_record_definition inline_record_name
    (inline_record_definitions : Parsetree.type_declaration list option) =
  match inline_record_definitions with
  | None -> None
  | Some inline_record_definitions ->
    inline_record_definitions
    |> List.find_opt (fun (r : Parsetree.type_declaration) ->
           r.ptype_name.txt = inline_record_name)

let print_lident l =
  let flat_lid_opt lid =
    let rec flat accu = function
      | Longident.Lident s -> Some (s :: accu)
      | Ldot (lid, s) -> flat (s :: accu) lid
      | Lapply (_, _) -> None
    in
    flat [] lid
  in
  match l with
  | Longident.Lident txt -> print_ident_like txt
  | Longident.Ldot (path, txt) ->
    let doc =
      match flat_lid_opt path with
      | Some txts ->
        Doc.concat
          [
            Doc.join ~sep:Doc.dot (List.map Doc.text txts);
            Doc.dot;
            print_ident_like txt;
          ]
      | None -> Doc.text "printLident: Longident.Lapply is not supported"
    in
    doc
  | Lapply (_, _) -> Doc.text "printLident: Longident.Lapply is not supported"

let print_longident_location l cmt_tbl =
  let doc = print_longident l.Location.txt in
  print_comments doc cmt_tbl l.loc

(* Module.SubModule.x *)
let print_lident_path path cmt_tbl =
  let doc = print_lident path.Location.txt in
  print_comments doc cmt_tbl path.loc

(* Module.SubModule.x or Module.SubModule.X *)
let print_ident_path path cmt_tbl =
  let doc = print_lident path.Location.txt in
  print_comments doc cmt_tbl path.loc

let print_string_loc sloc cmt_tbl =
  let doc = print_ident_like sloc.Location.txt in
  print_comments doc cmt_tbl sloc.loc

let print_string_contents txt =
  let lines = String.split_on_char '\n' txt in
  Doc.join ~sep:Doc.literal_line (List.map Doc.text lines)

let print_constant ?(template_literal = false) c =
  match c with
  | Parsetree.Pconst_integer (s, suffix) -> (
    match suffix with
    | Some c -> Doc.text (s ^ Char.escaped c)
    | None -> Doc.text s)
  | Pconst_string (txt, None) ->
    Doc.concat [Doc.text "\""; print_string_contents txt; Doc.text "\""]
  | Pconst_string (txt, Some prefix) ->
    if prefix = "INTERNAL_RES_CHAR_CONTENTS" then
      Doc.concat [Doc.text "'"; Doc.text txt; Doc.text "'"]
    else
      let lquote, rquote =
        if template_literal then ("`", "`") else ("\"", "\"")
      in
      Doc.concat
        [
          (if prefix = "js" then Doc.nil else Doc.text prefix);
          Doc.text lquote;
          print_string_contents txt;
          Doc.text rquote;
        ]
  | Pconst_float (s, _) -> Doc.text s
  | Pconst_char c ->
    let str =
      match Char.unsafe_chr c with
      | '\'' -> "\\'"
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | ' ' .. '~' as c ->
        let s = (Bytes.create [@doesNotRaise]) 1 in
        Bytes.unsafe_set s 0 c;
        Bytes.unsafe_to_string s
      | _ -> Res_utf8.encode_code_point c
    in
    Doc.text ("'" ^ str ^ "'")

module State = struct
  let custom_layout_threshold = 2

  type t = {custom_layout: int}

  let init () = {custom_layout = 0}

  let next_custom_layout t = {custom_layout = t.custom_layout + 1}

  let should_break_callback t = t.custom_layout > custom_layout_threshold
end

let rec print_structure ~state (s : Parsetree.structure) t =
  match s with
  | [] -> print_comments_inside_file t
  | structure ->
    print_list
      ~get_loc:(fun s -> s.Parsetree.pstr_loc)
      ~nodes:structure
      ~print:(print_structure_item ~state)
      t

and print_structure_item ~state (si : Parsetree.structure_item) cmt_tbl =
  match si.pstr_desc with
  | Pstr_value (rec_flag, value_bindings) ->
    let rec_flag =
      match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
    in
    print_value_bindings ~state ~rec_flag value_bindings cmt_tbl
  | Pstr_type (Recursive, type_declarations)
    when type_declarations
         |> List.find_opt (fun (td : Parsetree.type_declaration) ->
                Res_parsetree_viewer.has_inline_record_definition_attribute
                  td.ptype_attributes)
         |> Option.is_some ->
    let inline_record_definitions, regular_declarations =
      type_declarations
      |> List.partition (fun (td : Parsetree.type_declaration) ->
             Res_parsetree_viewer.has_inline_record_definition_attribute
               td.ptype_attributes)
    in
    print_type_declarations ~inline_record_definitions ~state
      ~rec_flag:
        (if List.length regular_declarations > 1 then Doc.text "rec "
         else Doc.nil)
      regular_declarations cmt_tbl
  | Pstr_type (rec_flag, type_declarations) ->
    let rec_flag =
      match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
    in
    print_type_declarations ~state ~rec_flag type_declarations cmt_tbl
  | Pstr_primitive value_description ->
    print_value_description ~state value_description cmt_tbl
  | Pstr_eval (expr, attrs) ->
    let expr_doc =
      let doc = print_expression_with_comments ~state expr cmt_tbl in
      match Parens.structure_expr expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc expr braces
      | Nothing -> doc
    in
    Doc.concat [print_attributes ~state attrs cmt_tbl; expr_doc]
  | Pstr_attribute attr ->
    fst (print_attribute ~state ~standalone:true attr cmt_tbl)
  | Pstr_extension (extension, attrs) ->
    Doc.concat
      [
        print_attributes ~state attrs cmt_tbl;
        Doc.concat
          [print_extension ~state ~at_module_lvl:true extension cmt_tbl];
      ]
  | Pstr_include include_declaration ->
    print_include_declaration ~state include_declaration cmt_tbl
  | Pstr_open open_description ->
    print_open_description ~state open_description cmt_tbl
  | Pstr_modtype mod_type_decl ->
    print_module_type_declaration ~state mod_type_decl cmt_tbl
  | Pstr_module module_binding ->
    print_module_binding ~state ~is_rec:false module_binding cmt_tbl 0
  | Pstr_recmodule module_bindings ->
    print_listi
      ~get_loc:(fun mb -> mb.Parsetree.pmb_loc)
      ~nodes:module_bindings
      ~print:(print_module_binding ~state ~is_rec:true)
      cmt_tbl
  | Pstr_exception extension_constructor ->
    print_exception_def ~state extension_constructor cmt_tbl
  | Pstr_typext type_extension ->
    print_type_extension ~state type_extension cmt_tbl

and print_type_extension ~state (te : Parsetree.type_extension) cmt_tbl =
  let prefix = Doc.text "type " in
  let name = print_lident_path te.ptyext_path cmt_tbl in
  let type_params = print_type_params ~state te.ptyext_params cmt_tbl in
  let extension_constructors =
    let ecs = te.ptyext_constructors in
    let force_break =
      match (ecs, List.rev ecs) with
      | first :: _, last :: _ ->
        first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum
        || first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
      | _ -> false
    in
    let private_flag =
      match te.ptyext_private with
      | Asttypes.Private -> Doc.concat [Doc.text "private"; Doc.line]
      | Public -> Doc.nil
    in
    let rows =
      print_listi
        ~get_loc:(fun n -> n.Parsetree.pext_loc)
        ~print:(print_extension_constructor ~state)
        ~nodes:ecs ~force_break cmt_tbl
    in
    Doc.breakable_group ~force_break
      (Doc.indent
         (Doc.concat
            [
              Doc.line;
              private_flag;
              rows;
              (* Doc.join ~sep:Doc.line ( *)
              (* List.mapi printExtensionConstructor ecs *)
              (* ) *)
            ]))
  in
  Doc.group
    (Doc.concat
       [
         print_attributes ~state ~loc:te.ptyext_path.loc te.ptyext_attributes
           cmt_tbl;
         prefix;
         name;
         type_params;
         Doc.text " +=";
         extension_constructors;
       ])

and print_module_binding ~state ~is_rec module_binding cmt_tbl i =
  let prefix =
    if i = 0 then
      Doc.concat
        [Doc.text "module "; (if is_rec then Doc.text "rec " else Doc.nil)]
    else Doc.text "and "
  in
  let mod_expr_doc, mod_constraint_doc =
    match module_binding.pmb_expr with
    | {pmod_desc = Pmod_constraint (mod_expr, mod_type)}
      when not
             (ParsetreeViewer.has_await_attribute
                module_binding.pmb_expr.pmod_attributes) ->
      ( print_mod_expr ~state mod_expr cmt_tbl,
        Doc.concat [Doc.text ": "; print_mod_type ~state mod_type cmt_tbl] )
    | mod_expr -> (print_mod_expr ~state mod_expr cmt_tbl, Doc.nil)
  in
  let mod_expr_doc_parens =
    if Parens.mod_expr_parens module_binding.pmb_expr then
      Doc.concat [Doc.lparen; mod_expr_doc; Doc.rparen]
    else mod_expr_doc
  in
  let mod_name =
    let doc = Doc.text module_binding.pmb_name.Location.txt in
    print_comments doc cmt_tbl module_binding.pmb_name.loc
  in
  let doc =
    Doc.concat
      [
        print_attributes ~state ~loc:module_binding.pmb_name.loc
          module_binding.pmb_attributes cmt_tbl;
        prefix;
        mod_name;
        mod_constraint_doc;
        Doc.text " = ";
        mod_expr_doc_parens;
      ]
  in
  print_comments doc cmt_tbl module_binding.pmb_loc

and print_module_type_declaration ~state
    (mod_type_decl : Parsetree.module_type_declaration) cmt_tbl =
  let mod_name =
    let doc = Doc.text mod_type_decl.pmtd_name.txt in
    print_comments doc cmt_tbl mod_type_decl.pmtd_name.loc
  in
  Doc.concat
    [
      print_attributes ~state mod_type_decl.pmtd_attributes cmt_tbl;
      Doc.text "module type ";
      mod_name;
      (match mod_type_decl.pmtd_type with
      | None -> Doc.nil
      | Some mod_type ->
        Doc.concat [Doc.text " = "; print_mod_type ~state mod_type cmt_tbl]);
    ]

and print_mod_type ~state mod_type cmt_tbl =
  let mod_type_doc =
    match mod_type.pmty_desc with
    | Parsetree.Pmty_ident longident ->
      Doc.concat
        [
          print_attributes ~state ~loc:longident.loc mod_type.pmty_attributes
            cmt_tbl;
          print_longident_location longident cmt_tbl;
        ]
    | Pmty_signature [] ->
      if has_comments_inside cmt_tbl mod_type.pmty_loc then
        let doc = print_comments_inside cmt_tbl mod_type.pmty_loc in
        Doc.concat [Doc.lbrace; doc; Doc.rbrace]
      else
        let should_break =
          mod_type.pmty_loc.loc_start.pos_lnum
          < mod_type.pmty_loc.loc_end.pos_lnum
        in
        Doc.breakable_group ~force_break:should_break
          (Doc.concat [Doc.lbrace; Doc.soft_line; Doc.soft_line; Doc.rbrace])
    | Pmty_signature signature ->
      let signature_doc =
        Doc.breakable_group ~force_break:true
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent
                 (Doc.concat
                    [Doc.line; print_signature ~state signature cmt_tbl]);
               Doc.line;
               Doc.rbrace;
             ])
      in
      Doc.concat
        [
          print_attributes ~state mod_type.pmty_attributes cmt_tbl; signature_doc;
        ]
    | Pmty_functor _ ->
      let parameters, return_type = ParsetreeViewer.functor_type mod_type in
      let parameters_doc =
        match parameters with
        | [] -> Doc.nil
        | [(attrs, {Location.txt = "_"; loc}, Some mod_type)] ->
          let cmt_loc =
            {loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end}
          in
          let attrs = print_attributes ~state attrs cmt_tbl in
          let doc =
            Doc.concat [attrs; print_mod_type ~state mod_type cmt_tbl]
          in
          print_comments doc cmt_tbl cmt_loc
        | params ->
          Doc.group
            (Doc.concat
               [
                 Doc.lparen;
                 Doc.indent
                   (Doc.concat
                      [
                        Doc.soft_line;
                        Doc.join
                          ~sep:(Doc.concat [Doc.comma; Doc.line])
                          (List.map
                             (fun (attrs, lbl, mod_type) ->
                               let cmt_loc =
                                 match mod_type with
                                 | None -> lbl.Asttypes.loc
                                 | Some mod_type ->
                                   {
                                     lbl.Asttypes.loc with
                                     loc_end =
                                       mod_type.Parsetree.pmty_loc.loc_end;
                                   }
                               in
                               let attrs =
                                 print_attributes ~state attrs cmt_tbl
                               in
                               let lbl_doc =
                                 if lbl.Location.txt = "_" || lbl.txt = "*" then
                                   Doc.nil
                                 else
                                   let doc = Doc.text lbl.txt in
                                   print_comments doc cmt_tbl lbl.loc
                               in
                               let doc =
                                 Doc.concat
                                   [
                                     attrs;
                                     lbl_doc;
                                     (match mod_type with
                                     | None -> Doc.nil
                                     | Some mod_type ->
                                       Doc.concat
                                         [
                                           (if lbl.txt = "_" then Doc.nil
                                            else Doc.text ": ");
                                           print_mod_type ~state mod_type
                                             cmt_tbl;
                                         ]);
                                   ]
                               in
                               print_comments doc cmt_tbl cmt_loc)
                             params);
                      ]);
                 Doc.trailing_comma;
                 Doc.soft_line;
                 Doc.rparen;
               ])
      in
      let return_doc =
        let doc = print_mod_type ~state return_type cmt_tbl in
        if Parens.mod_type_functor_return return_type then add_parens doc
        else doc
      in
      Doc.group
        (Doc.concat
           [
             parameters_doc;
             Doc.group (Doc.concat [Doc.text " =>"; Doc.line; return_doc]);
           ])
    | Pmty_typeof mod_expr ->
      Doc.concat
        [Doc.text "module type of "; print_mod_expr ~state mod_expr cmt_tbl]
    | Pmty_extension extension ->
      print_extension ~state ~at_module_lvl:false extension cmt_tbl
    | Pmty_alias longident ->
      Doc.concat
        [Doc.text "module "; print_longident_location longident cmt_tbl]
    | Pmty_with (mod_type, with_constraints) ->
      let operand =
        let doc = print_mod_type ~state mod_type cmt_tbl in
        if Parens.mod_type_with_operand mod_type then add_parens doc else doc
      in
      Doc.group
        (Doc.concat
           [
             operand;
             Doc.indent
               (Doc.concat
                  [
                    Doc.line;
                    print_with_constraints ~state with_constraints cmt_tbl;
                  ]);
           ])
  in
  let attrs_already_printed =
    match mod_type.pmty_desc with
    | Pmty_functor _ | Pmty_signature _ | Pmty_ident _ -> true
    | _ -> false
  in
  let doc =
    Doc.concat
      [
        (if attrs_already_printed then Doc.nil
         else print_attributes ~state mod_type.pmty_attributes cmt_tbl);
        mod_type_doc;
      ]
  in
  print_comments doc cmt_tbl mod_type.pmty_loc

and print_with_constraints ~state with_constraints cmt_tbl =
  let rows =
    List.mapi
      (fun i with_constraint ->
        Doc.group
          (Doc.concat
             [
               (if i == 0 then Doc.text "with " else Doc.text "and ");
               print_with_constraint ~state with_constraint cmt_tbl;
             ]))
      with_constraints
  in
  Doc.join ~sep:Doc.line rows

and print_with_constraint ~state (with_constraint : Parsetree.with_constraint)
    cmt_tbl =
  match with_constraint with
  (* with type X.t = ... *)
  | Pwith_type (longident, type_declaration) ->
    Doc.group
      (print_type_declaration ~state
         ~name:(print_lident_path longident cmt_tbl)
         ~equal_sign:"=" ~rec_flag:Doc.nil 0 type_declaration CommentTable.empty)
  (* with module X.Y = Z *)
  | Pwith_module ({txt = longident1}, {txt = longident2}) ->
    Doc.concat
      [
        Doc.text "module ";
        print_longident longident1;
        Doc.text " =";
        Doc.indent (Doc.concat [Doc.line; print_longident longident2]);
      ]
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Pwith_typesubst (longident, type_declaration) ->
    Doc.group
      (print_type_declaration ~state
         ~name:(print_lident_path longident cmt_tbl)
         ~equal_sign:":=" ~rec_flag:Doc.nil 0 type_declaration
         CommentTable.empty)
  | Pwith_modsubst ({txt = longident1}, {txt = longident2}) ->
    Doc.concat
      [
        Doc.text "module ";
        print_longident longident1;
        Doc.text " :=";
        Doc.indent (Doc.concat [Doc.line; print_longident longident2]);
      ]

and print_signature ~state signature cmt_tbl =
  match signature with
  | [] -> print_comments_inside_file cmt_tbl
  | signature ->
    print_list
      ~get_loc:(fun s -> s.Parsetree.psig_loc)
      ~nodes:signature
      ~print:(print_signature_item ~state)
      cmt_tbl

and print_signature_item ~state (si : Parsetree.signature_item) cmt_tbl =
  match si.psig_desc with
  | Parsetree.Psig_value value_description ->
    print_value_description ~state value_description cmt_tbl
  | Psig_type (rec_flag, type_declarations) ->
    let rec_flag =
      match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
    in
    print_type_declarations ~state ~rec_flag type_declarations cmt_tbl
  | Psig_typext type_extension ->
    print_type_extension ~state type_extension cmt_tbl
  | Psig_exception extension_constructor ->
    print_exception_def ~state extension_constructor cmt_tbl
  | Psig_module module_declaration ->
    print_module_declaration ~state module_declaration cmt_tbl
  | Psig_recmodule module_declarations ->
    print_rec_module_declarations ~state module_declarations cmt_tbl
  | Psig_modtype mod_type_decl ->
    print_module_type_declaration ~state mod_type_decl cmt_tbl
  | Psig_open open_description ->
    print_open_description ~state open_description cmt_tbl
  | Psig_include include_description ->
    print_include_description ~state include_description cmt_tbl
  | Psig_attribute attr ->
    fst (print_attribute ~state ~standalone:true attr cmt_tbl)
  | Psig_extension (extension, attrs) ->
    Doc.concat
      [
        print_attributes ~state attrs cmt_tbl;
        Doc.concat
          [print_extension ~state ~at_module_lvl:true extension cmt_tbl];
      ]

and print_rec_module_declarations ~state module_declarations cmt_tbl =
  print_listi
    ~get_loc:(fun n -> n.Parsetree.pmd_loc)
    ~nodes:module_declarations
    ~print:(print_rec_module_declaration ~state)
    cmt_tbl

and print_rec_module_declaration ~state md cmt_tbl i =
  let body =
    match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; print_longident_location longident cmt_tbl]
    | _ ->
      let needs_parens =
        match md.pmd_type.pmty_desc with
        | Pmty_with _ -> true
        | _ -> false
      in
      let mod_type_doc =
        let doc = print_mod_type ~state md.pmd_type cmt_tbl in
        if needs_parens then add_parens doc else doc
      in
      Doc.concat [Doc.text ": "; mod_type_doc]
  in
  let prefix = if i < 1 then "module rec " else "and " in
  Doc.concat
    [
      print_attributes ~state ~loc:md.pmd_name.loc md.pmd_attributes cmt_tbl;
      Doc.text prefix;
      print_comments (Doc.text md.pmd_name.txt) cmt_tbl md.pmd_name.loc;
      body;
    ]

and print_module_declaration ~state (md : Parsetree.module_declaration) cmt_tbl
    =
  let body =
    match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; print_longident_location longident cmt_tbl]
    | _ -> Doc.concat [Doc.text ": "; print_mod_type ~state md.pmd_type cmt_tbl]
  in
  Doc.concat
    [
      print_attributes ~state ~loc:md.pmd_name.loc md.pmd_attributes cmt_tbl;
      Doc.text "module ";
      print_comments (Doc.text md.pmd_name.txt) cmt_tbl md.pmd_name.loc;
      body;
    ]

and print_open_description ~state
    (open_description : Parsetree.open_description) cmt_tbl =
  Doc.concat
    [
      print_attributes ~state open_description.popen_attributes cmt_tbl;
      Doc.text "open";
      (match open_description.popen_override with
      | Asttypes.Fresh -> Doc.space
      | Asttypes.Override -> Doc.text "! ");
      print_longident_location open_description.popen_lid cmt_tbl;
    ]

and print_include_description ~state
    (include_description : Parsetree.include_description) cmt_tbl =
  Doc.concat
    [
      print_attributes ~state include_description.pincl_attributes cmt_tbl;
      Doc.text "include ";
      print_mod_type ~state include_description.pincl_mod cmt_tbl;
    ]

and print_include_declaration ~state
    (include_declaration : Parsetree.include_declaration) cmt_tbl =
  Doc.concat
    [
      print_attributes ~state include_declaration.pincl_attributes cmt_tbl;
      Doc.text "include ";
      (let include_doc =
         print_mod_expr ~state include_declaration.pincl_mod cmt_tbl
       in
       if Parens.include_mod_expr include_declaration.pincl_mod then
         add_parens include_doc
       else include_doc);
    ]

and print_value_bindings ~state ~rec_flag (vbs : Parsetree.value_binding list)
    cmt_tbl =
  print_listi
    ~get_loc:(fun vb -> vb.Parsetree.pvb_loc)
    ~nodes:vbs
    ~print:(print_value_binding ~state ~rec_flag)
    cmt_tbl

and print_value_description ~state value_description cmt_tbl =
  let is_external =
    match value_description.pval_prim with
    | [] -> false
    | _ -> true
  in
  let attrs =
    print_attributes ~state ~loc:value_description.pval_name.loc
      value_description.pval_attributes cmt_tbl
  in
  let header = if is_external then "external " else "let " in
  Doc.group
    (Doc.concat
       [
         attrs;
         Doc.text header;
         print_comments
           (print_ident_like value_description.pval_name.txt)
           cmt_tbl value_description.pval_name.loc;
         Doc.text ": ";
         print_typ_expr ~state value_description.pval_type cmt_tbl;
         (if is_external then
            Doc.group
              (Doc.concat
                 [
                   Doc.text " =";
                   Doc.indent
                     (Doc.concat
                        [
                          Doc.line;
                          Doc.join ~sep:Doc.line
                            (List.map
                               (fun s ->
                                 Doc.concat
                                   [Doc.text "\""; Doc.text s; Doc.text "\""])
                               value_description.pval_prim);
                        ]);
                 ])
          else Doc.nil);
       ])

and print_type_declarations ?inline_record_definitions ~state ~rec_flag
    type_declarations cmt_tbl =
  print_listi
    ~get_loc:(fun n -> n.Parsetree.ptype_loc)
    ~nodes:type_declarations
    ~print:(print_type_declaration2 ?inline_record_definitions ~state ~rec_flag)
    cmt_tbl

(*
 * type_declaration = {
 *    ptype_name: string loc;
 *    ptype_params: (core_type * variance) list;
 *          (* ('a1,...'an) t; None represents  _*)
 *    ptype_cstrs: (core_type * core_type * Location.t) list;
 *          (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
 *    ptype_kind: type_kind;
 *    ptype_private: private_flag;   (* = private ... *)
 *    ptype_manifest: core_type option;  (* = T *)
 *    ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
 *    ptype_loc: Location.t;
 * }
 *
 *
 *  type t                     (abstract, no manifest)
 *  type t = T0                (abstract, manifest=T0)
 *  type t = C of T | ...      (variant,  no manifest)
 *  type t = T0 = C of T | ... (variant,  manifest=T0)
 *  type t = {l: T; ...}       (record,   no manifest)
 *  type t = T0 = {l : T; ...} (record,   manifest=T0)
 *  type t = ..                (open,     no manifest)
 *
 *
 * and type_kind =
 *  | Ptype_abstract
 *  | Ptype_variant of constructor_declaration list
 *        (* Invariant: non-empty list *)
 *  | Ptype_record of label_declaration list
 *        (* Invariant: non-empty list *)
 *  | Ptype_open
 *)
and print_type_declaration ~state ~name ~equal_sign ~rec_flag i
    (td : Parsetree.type_declaration) cmt_tbl =
  let attrs =
    print_attributes ~state ~loc:td.ptype_loc td.ptype_attributes cmt_tbl
  in
  let prefix =
    if i > 0 then Doc.text "and " else Doc.concat [Doc.text "type "; rec_flag]
  in
  let type_name = name in
  let type_params = print_type_params ~state td.ptype_params cmt_tbl in
  let manifest_and_kind =
    match td.ptype_kind with
    | Ptype_abstract -> (
      match td.ptype_manifest with
      | None -> Doc.nil
      | Some typ ->
        Doc.concat
          [
            Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
            print_private_flag td.ptype_private;
            print_typ_expr ~state typ cmt_tbl;
          ])
    | Ptype_open ->
      Doc.concat
        [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_private_flag td.ptype_private;
          Doc.text "..";
        ]
    | Ptype_record lds ->
      let manifest =
        match td.ptype_manifest with
        | None -> Doc.nil
        | Some typ ->
          Doc.concat
            [
              Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
              print_typ_expr ~state typ cmt_tbl;
            ]
      in
      Doc.concat
        [
          manifest;
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_private_flag td.ptype_private;
          print_record_declaration ~state lds cmt_tbl;
        ]
    | Ptype_variant cds ->
      let manifest =
        match td.ptype_manifest with
        | None -> Doc.nil
        | Some typ ->
          Doc.concat
            [
              Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
              print_typ_expr ~state typ cmt_tbl;
            ]
      in
      Doc.concat
        [
          manifest;
          Doc.concat [Doc.space; Doc.text equal_sign];
          print_constructor_declarations ~state ~private_flag:td.ptype_private
            cds cmt_tbl;
        ]
  in
  let constraints = print_type_definition_constraints ~state td.ptype_cstrs in
  Doc.group
    (Doc.concat
       [attrs; prefix; type_name; type_params; manifest_and_kind; constraints])

and print_type_declaration2 ?inline_record_definitions ~state ~rec_flag
    (td : Parsetree.type_declaration) cmt_tbl i =
  let name =
    let doc = print_ident_like td.Parsetree.ptype_name.txt in
    print_comments doc cmt_tbl td.ptype_name.loc
  in
  let equal_sign = "=" in
  let attrs =
    print_attributes ~state ~loc:td.ptype_loc td.ptype_attributes cmt_tbl
  in
  let prefix =
    if i > 0 then Doc.text "and " else Doc.concat [Doc.text "type "; rec_flag]
  in
  let type_name = name in
  let type_params = print_type_params ~state td.ptype_params cmt_tbl in
  let manifest_and_kind =
    match td.ptype_kind with
    | Ptype_abstract -> (
      match td.ptype_manifest with
      | None -> Doc.nil
      | Some typ ->
        Doc.concat
          [
            Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
            print_private_flag td.ptype_private;
            print_typ_expr ~state typ cmt_tbl;
          ])
    | Ptype_open ->
      Doc.concat
        [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_private_flag td.ptype_private;
          Doc.text "..";
        ]
    | Ptype_record lds ->
      if lds = [] then
        Doc.concat
          [
            Doc.space;
            Doc.text equal_sign;
            Doc.space;
            Doc.lbrace;
            print_comments_inside cmt_tbl td.ptype_loc;
            Doc.rbrace;
          ]
      else
        let manifest =
          match td.ptype_manifest with
          | None -> Doc.nil
          | Some typ ->
            Doc.concat
              [
                Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
                print_typ_expr ~state typ cmt_tbl;
              ]
        in
        Doc.concat
          [
            manifest;
            Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
            print_private_flag td.ptype_private;
            print_record_declaration ?inline_record_definitions ~state lds
              cmt_tbl;
          ]
    | Ptype_variant cds ->
      let manifest =
        match td.ptype_manifest with
        | None -> Doc.nil
        | Some typ ->
          Doc.concat
            [
              Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
              print_typ_expr ~state typ cmt_tbl;
            ]
      in
      Doc.concat
        [
          manifest;
          Doc.concat [Doc.space; Doc.text equal_sign];
          print_constructor_declarations ~state ~private_flag:td.ptype_private
            cds cmt_tbl;
        ]
  in
  let constraints = print_type_definition_constraints ~state td.ptype_cstrs in
  Doc.group
    (Doc.concat
       [attrs; prefix; type_name; type_params; manifest_and_kind; constraints])

and print_type_definition_constraints ~state cstrs =
  match cstrs with
  | [] -> Doc.nil
  | cstrs ->
    Doc.indent
      (Doc.group
         (Doc.concat
            [
              Doc.line;
              Doc.group
                (Doc.join ~sep:Doc.line
                   (List.map (print_type_definition_constraint ~state) cstrs));
            ]))

and print_type_definition_constraint ~state
    ((typ1, typ2, _loc) :
      Parsetree.core_type * Parsetree.core_type * Location.t) =
  Doc.concat
    [
      Doc.text "constraint ";
      print_typ_expr ~state typ1 CommentTable.empty;
      Doc.text " = ";
      print_typ_expr ~state typ2 CommentTable.empty;
    ]

and print_private_flag (flag : Asttypes.private_flag) =
  match flag with
  | Private -> Doc.text "private "
  | Public -> Doc.nil

and print_type_params ~state type_params cmt_tbl =
  match type_params with
  | [] -> Doc.nil
  | type_params ->
    Doc.group
      (Doc.concat
         [
           Doc.less_than;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map
                       (fun type_param ->
                         let doc = print_type_param ~state type_param cmt_tbl in
                         print_comments doc cmt_tbl
                           (fst type_param).Parsetree.ptyp_loc)
                       type_params);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.greater_than;
         ])

and print_type_param ~state (param : Parsetree.core_type * Asttypes.variance)
    cmt_tbl =
  let typ, variance = param in
  let printed_variance =
    match variance with
    | Covariant -> Doc.text "+"
    | Contravariant -> Doc.text "-"
    | Invariant -> Doc.nil
  in
  Doc.concat [printed_variance; print_typ_expr ~state typ cmt_tbl]

and print_record_declaration ?inline_record_definitions ~state
    (lds : Parsetree.label_declaration list) cmt_tbl =
  let force_break =
    match (lds, List.rev lds) with
    | first :: _, last :: _ ->
      first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ -> false
  in
  Doc.breakable_group ~force_break
    (Doc.concat
       [
         Doc.lbrace;
         Doc.indent
           (Doc.concat
              [
                Doc.soft_line;
                Doc.join
                  ~sep:(Doc.concat [Doc.comma; Doc.line])
                  (List.map
                     (fun ld ->
                       let doc =
                         print_label_declaration ?inline_record_definitions
                           ~state ld cmt_tbl
                       in
                       print_comments doc cmt_tbl ld.Parsetree.pld_loc)
                     lds);
              ]);
         Doc.trailing_comma;
         Doc.soft_line;
         Doc.rbrace;
       ])

and print_literal_dict_expr ~state (e : Parsetree.expression) cmt_tbl =
  let force_break =
    e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
  in
  let tuple_to_row (e : Parsetree.expression) =
    match e with
    | {
     pexp_desc =
       Pexp_tuple
         [
           {pexp_desc = Pexp_constant (Pconst_string (name, _)); pexp_loc}; value;
         ];
    } ->
      Some ((Location.mkloc (Longident.Lident name) pexp_loc, value), e)
    | _ -> None
  in
  let rows =
    match e with
    | {pexp_desc = Pexp_array expressions} ->
      List.filter_map tuple_to_row expressions
    | _ -> []
  in
  Doc.breakable_group ~force_break
    (Doc.concat
       [
         Doc.indent
           (Doc.concat
              [
                Doc.soft_line;
                Doc.join
                  ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map
                     (fun ((row, e) :
                            (Longident.t Location.loc * Parsetree.expression)
                            * Parsetree.expression) ->
                       let doc = print_bs_object_row ~state row cmt_tbl in
                       print_comments doc cmt_tbl e.pexp_loc)
                     rows);
              ]);
         Doc.trailing_comma;
         Doc.soft_line;
       ])

and print_constructor_declarations ~state ~private_flag
    (cds : Parsetree.constructor_declaration list) cmt_tbl =
  let force_break =
    match (cds, List.rev cds) with
    | first :: _, last :: _ ->
      first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ -> false
  in
  let private_flag =
    match private_flag with
    | Asttypes.Private -> Doc.concat [Doc.text "private"; Doc.line]
    | Public -> Doc.nil
  in
  let rows =
    print_listi
      ~get_loc:(fun cd -> cd.Parsetree.pcd_loc)
      ~nodes:cds
      ~print:(fun cd cmt_tbl i ->
        let doc = print_constructor_declaration2 ~state i cd cmt_tbl in
        print_comments doc cmt_tbl cd.Parsetree.pcd_loc)
      ~force_break cmt_tbl
  in
  Doc.breakable_group ~force_break
    (Doc.indent (Doc.concat [Doc.line; private_flag; rows]))

and print_constructor_declaration2 ~state i
    (cd : Parsetree.constructor_declaration) cmt_tbl =
  let attrs = print_attributes ~state cd.pcd_attributes cmt_tbl in
  let is_dot_dot_dot = cd.pcd_name.txt = "..." in
  let bar =
    if i > 0 || cd.pcd_attributes <> [] || is_dot_dot_dot then Doc.text "| "
    else Doc.if_breaks (Doc.text "| ") Doc.nil
  in
  let constr_name =
    let doc = Doc.text cd.pcd_name.txt in
    print_comments doc cmt_tbl cd.pcd_name.loc
  in
  let constr_args =
    print_constructor_arguments ~is_dot_dot_dot ~state ~indent:true cd.pcd_args
      cmt_tbl
  in
  let gadt =
    match cd.pcd_res with
    | None -> Doc.nil
    | Some typ ->
      Doc.indent (Doc.concat [Doc.text ": "; print_typ_expr ~state typ cmt_tbl])
  in
  Doc.concat
    [
      bar;
      Doc.group
        (Doc.concat
           [
             attrs;
             (* TODO: fix parsing of attributes, so when can print them above the bar? *)
             constr_name;
             constr_args;
             gadt;
           ]);
    ]

and print_constructor_arguments ?(is_dot_dot_dot = false) ~state ~indent
    (cd_args : Parsetree.constructor_arguments) cmt_tbl =
  match cd_args with
  | Pcstr_tuple [] -> Doc.nil
  | Pcstr_tuple types ->
    let args =
      Doc.concat
        [
          (if is_dot_dot_dot then Doc.nil else Doc.lparen);
          Doc.indent
            (Doc.concat
               [
                 Doc.soft_line;
                 Doc.join
                   ~sep:(Doc.concat [Doc.comma; Doc.line])
                   (List.map
                      (fun typexpr -> print_typ_expr ~state typexpr cmt_tbl)
                      types);
               ]);
          Doc.trailing_comma;
          Doc.soft_line;
          (if is_dot_dot_dot then Doc.nil else Doc.rparen);
        ]
    in
    Doc.group (if indent then Doc.indent args else args)
  | Pcstr_record lds ->
    let args =
      Doc.concat
        [
          Doc.lparen;
          (* manually inline the printRecordDeclaration, gives better layout *)
          Doc.lbrace;
          Doc.indent
            (Doc.concat
               [
                 Doc.soft_line;
                 Doc.join
                   ~sep:(Doc.concat [Doc.comma; Doc.line])
                   (List.map
                      (fun ld ->
                        let doc = print_label_declaration ~state ld cmt_tbl in
                        print_comments doc cmt_tbl ld.Parsetree.pld_loc)
                      lds);
               ]);
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbrace;
          Doc.rparen;
        ]
    in
    if indent then Doc.indent args else args

and print_label_declaration ?inline_record_definitions ~state
    (ld : Parsetree.label_declaration) cmt_tbl =
  let attrs =
    print_attributes ~state ~loc:ld.pld_name.loc ld.pld_attributes cmt_tbl
  in
  let mutable_flag =
    match ld.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
  in
  let name, is_dot =
    let doc, is_dot =
      if ld.pld_name.txt = "..." then (Doc.text ld.pld_name.txt, true)
      else (print_ident_like ld.pld_name.txt, false)
    in
    (print_comments doc cmt_tbl ld.pld_name.loc, is_dot)
  in
  let optional = if ld.pld_optional then Doc.text "?" else Doc.nil in
  Doc.group
    (Doc.concat
       [
         attrs;
         mutable_flag;
         name;
         optional;
         (if is_dot then Doc.nil else Doc.text ": ");
         print_typ_expr ?inline_record_definitions ~state ld.pld_type cmt_tbl;
       ])

and print_typ_expr ?inline_record_definitions ~(state : State.t)
    (typ_expr : Parsetree.core_type) cmt_tbl =
  let print_arrow ~arity typ_expr =
    let max_arity =
      match arity with
      | Some arity -> arity
      | None -> max_int
    in
    let attrs_before, args, return_type =
      ParsetreeViewer.arrow_type ~max_arity typ_expr
    in
    let return_type_needs_parens =
      match return_type.ptyp_desc with
      | Ptyp_alias _ -> true
      | _ -> false
    in
    let return_doc =
      let doc = print_typ_expr ~state return_type cmt_tbl in
      if return_type_needs_parens then Doc.concat [Doc.lparen; doc; Doc.rparen]
      else doc
    in
    match args with
    | [] -> Doc.nil
    | [([], Nolabel, n)] ->
      let has_attrs_before = not (attrs_before = []) in
      let attrs =
        if has_attrs_before then
          print_attributes ~state ~inline:true attrs_before cmt_tbl
        else Doc.nil
      in
      let typ_doc =
        let doc = print_typ_expr ~state n cmt_tbl in
        match n.ptyp_desc with
        | Ptyp_arrow _ | Ptyp_tuple _ | Ptyp_alias _ -> add_parens doc
        | _ -> doc
      in
      Doc.group
        (Doc.concat
           [
             Doc.group attrs;
             Doc.group
               (if has_attrs_before then
                  Doc.concat
                    [
                      Doc.lparen;
                      Doc.indent
                        (Doc.concat
                           [Doc.soft_line; typ_doc; Doc.text " => "; return_doc]);
                      Doc.soft_line;
                      Doc.rparen;
                    ]
                else Doc.concat [typ_doc; Doc.text " => "; return_doc]);
           ])
    | args ->
      let attrs = print_attributes ~state ~inline:true attrs_before cmt_tbl in
      let rendered_args =
        Doc.concat
          [
            attrs;
            Doc.text "(";
            Doc.indent
              (Doc.concat
                 [
                   Doc.soft_line;
                   Doc.join
                     ~sep:(Doc.concat [Doc.comma; Doc.line])
                     (List.map
                        (fun tp -> print_type_parameter ~state tp cmt_tbl)
                        args);
                 ]);
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.text ")";
          ]
      in
      Doc.group (Doc.concat [rendered_args; Doc.text " => "; return_doc])
  in
  let rendered_type =
    match typ_expr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var ->
      Doc.concat [Doc.text "'"; print_ident_like ~allow_uident:true var]
    | Ptyp_extension extension ->
      print_extension ~state ~at_module_lvl:false extension cmt_tbl
    | Ptyp_alias (typ, alias) ->
      let typ =
        (* Technically type t = (string, float) => unit as 'x, doesn't require
         * parens around the arrow expression. This is very confusing though.
         * Is the "as" part of "unit" or "(string, float) => unit". By printing
         * parens we guide the user towards its meaning.*)
        let needs_parens =
          match typ.ptyp_desc with
          | Ptyp_arrow _ -> true
          | _ -> false
        in
        let doc = print_typ_expr ~state typ cmt_tbl in
        if needs_parens then Doc.concat [Doc.lparen; doc; Doc.rparen] else doc
      in
      Doc.concat
        [
          typ; Doc.text " as "; Doc.concat [Doc.text "'"; print_ident_like alias];
        ]
    (* object printings *)
    | Ptyp_object (fields, open_flag) ->
      print_object ~state ~inline:false fields open_flag cmt_tbl
    | Ptyp_arrow {arity} -> print_arrow ~arity typ_expr
    | Ptyp_constr ({txt = Lident inline_record_name}, _)
      when inline_record_definitions
           |> find_inline_record_definition inline_record_name
           |> Option.is_some -> (
      match
        inline_record_definitions
        |> find_inline_record_definition inline_record_name
      with
      | Some {ptype_kind = Ptype_record lds} ->
        print_record_declaration
          ~inline_record_definitions:(inline_record_definitions |> Option.get)
          ~state lds cmt_tbl
      | _ -> assert false)
    | Ptyp_constr
        (longident_loc, [{ptyp_desc = Ptyp_object (fields, open_flag)}]) ->
      (* for foo<{"a": b}>, when the object is long and needs a line break, we
         want the <{ and }> to stay hugged together *)
      let constr_name = print_lident_path longident_loc cmt_tbl in
      Doc.concat
        [
          constr_name;
          Doc.less_than;
          print_object ~state ~inline:true fields open_flag cmt_tbl;
          Doc.greater_than;
        ]
    | Ptyp_constr (longident_loc, [{ptyp_desc = Parsetree.Ptyp_tuple tuple}]) ->
      let constr_name = print_lident_path longident_loc cmt_tbl in
      Doc.group
        (Doc.concat
           [
             constr_name;
             Doc.less_than;
             print_tuple_type ~state ~inline:true tuple cmt_tbl;
             Doc.greater_than;
           ])
    | Ptyp_constr (longident_loc, constr_args) -> (
      let constr_name = print_lident_path longident_loc cmt_tbl in
      match constr_args with
      | [] -> constr_name
      | _args ->
        Doc.group
          (Doc.concat
             [
               constr_name;
               Doc.less_than;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.soft_line;
                      Doc.join
                        ~sep:(Doc.concat [Doc.comma; Doc.line])
                        (List.map
                           (fun typexpr ->
                             print_typ_expr ?inline_record_definitions ~state
                               typexpr cmt_tbl)
                           constr_args);
                    ]);
               Doc.trailing_comma;
               Doc.soft_line;
               Doc.greater_than;
             ]))
    | Ptyp_tuple types -> print_tuple_type ~state ~inline:false types cmt_tbl
    | Ptyp_poly ([], typ) ->
      print_typ_expr ?inline_record_definitions ~state typ cmt_tbl
    | Ptyp_poly (string_locs, typ) ->
      Doc.concat
        [
          Doc.join ~sep:Doc.space
            (List.map
               (fun {Location.txt; loc} ->
                 let doc = Doc.concat [Doc.text "'"; Doc.text txt] in
                 print_comments doc cmt_tbl loc)
               string_locs);
          Doc.dot;
          Doc.space;
          print_typ_expr ~state typ cmt_tbl;
        ]
    | Ptyp_package package_type ->
      print_package_type ~state ~print_module_keyword_and_parens:true
        package_type cmt_tbl
    | Ptyp_variant (row_fields, closed_flag, labels_opt) ->
      let force_break =
        typ_expr.ptyp_loc.Location.loc_start.pos_lnum
        < typ_expr.ptyp_loc.loc_end.pos_lnum
      in
      let print_row_field = function
        | Parsetree.Rtag ({txt; loc}, attrs, true, []) ->
          let doc =
            Doc.group
              (Doc.concat
                 [
                   print_attributes ~state attrs cmt_tbl;
                   Doc.concat [Doc.text "#"; print_poly_var_ident txt];
                 ])
          in
          print_comments doc cmt_tbl loc
        | Rtag ({txt}, attrs, truth, types) ->
          let do_type t =
            match t.Parsetree.ptyp_desc with
            | Ptyp_tuple _ -> print_typ_expr ~state t cmt_tbl
            | _ ->
              Doc.concat
                [Doc.lparen; print_typ_expr ~state t cmt_tbl; Doc.rparen]
          in
          let printed_types = List.map do_type types in
          let cases =
            Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "& "]) printed_types
          in
          let cases =
            if truth then Doc.concat [Doc.line; Doc.text "& "; cases] else cases
          in
          Doc.group
            (Doc.concat
               [
                 print_attributes ~state attrs cmt_tbl;
                 Doc.concat [Doc.text "#"; print_poly_var_ident txt];
                 cases;
               ])
        | Rinherit core_type -> print_typ_expr ~state core_type cmt_tbl
      in
      let docs = List.map print_row_field row_fields in
      let cases = Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "| "]) docs in
      let cases =
        if docs = [] then cases
        else Doc.concat [Doc.if_breaks (Doc.text "| ") Doc.nil; cases]
      in
      let opening_symbol =
        if closed_flag = Open then Doc.concat [Doc.greater_than; Doc.line]
        else if labels_opt = None then Doc.soft_line
        else Doc.concat [Doc.less_than; Doc.line]
      in
      let labels =
        match labels_opt with
        | None | Some [] -> Doc.nil
        | Some labels ->
          Doc.concat
            (List.map
               (fun label ->
                 Doc.concat [Doc.line; Doc.text "#"; print_poly_var_ident label])
               labels)
      in
      let closing_symbol =
        match labels_opt with
        | None | Some [] -> Doc.nil
        | _ -> Doc.text " >"
      in
      Doc.breakable_group ~force_break
        (Doc.concat
           [
             Doc.lbracket;
             Doc.indent
               (Doc.concat [opening_symbol; cases; closing_symbol; labels]);
             Doc.soft_line;
             Doc.rbracket;
           ])
  in
  let should_print_its_own_attributes =
    match typ_expr.ptyp_desc with
    | Ptyp_arrow _ (* es6 arrow types print their own attributes *) -> true
    | _ -> false
  in
  let doc =
    match typ_expr.ptyp_attributes with
    | _ :: _ as attrs when not should_print_its_own_attributes ->
      Doc.group
        (Doc.concat [print_attributes ~state attrs cmt_tbl; rendered_type])
    | _ -> rendered_type
  in
  print_comments doc cmt_tbl typ_expr.ptyp_loc

and print_object ~state ~inline fields open_flag cmt_tbl =
  let doc =
    match fields with
    | [] ->
      Doc.concat
        [
          Doc.lbrace;
          (match open_flag with
          | Asttypes.Closed -> Doc.dot
          | Open -> Doc.dotdot);
          Doc.rbrace;
        ]
    | fields ->
      Doc.concat
        [
          Doc.lbrace;
          (match open_flag with
          | Asttypes.Closed -> Doc.nil
          | Open -> (
            match fields with
            (* handle `type t = {.. ...objType, "x": int}`
             * .. and ... should have a space in between *)
            | Oinherit _ :: _ -> Doc.text ".. "
            | _ -> Doc.dotdot));
          Doc.indent
            (Doc.concat
               [
                 Doc.soft_line;
                 Doc.join
                   ~sep:(Doc.concat [Doc.comma; Doc.line])
                   (List.map
                      (fun field -> print_object_field ~state field cmt_tbl)
                      fields);
               ]);
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbrace;
        ]
  in
  if inline then doc else Doc.group doc

and print_tuple_type ~state ~inline (types : Parsetree.core_type list) cmt_tbl =
  let tuple =
    Doc.concat
      [
        Doc.lparen;
        Doc.indent
          (Doc.concat
             [
               Doc.soft_line;
               Doc.join
                 ~sep:(Doc.concat [Doc.comma; Doc.line])
                 (List.map
                    (fun typexpr -> print_typ_expr ~state typexpr cmt_tbl)
                    types);
             ]);
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rparen;
      ]
  in
  if inline == false then Doc.group tuple else tuple

and print_object_field ~state (field : Parsetree.object_field) cmt_tbl =
  match field with
  | Otag (label_loc, attrs, typ) ->
    let lbl =
      let doc = Doc.text ("\"" ^ label_loc.txt ^ "\"") in
      print_comments doc cmt_tbl label_loc.loc
    in
    let doc =
      Doc.concat
        [
          print_attributes ~state ~loc:label_loc.loc attrs cmt_tbl;
          lbl;
          Doc.text ": ";
          print_typ_expr ~state typ cmt_tbl;
        ]
    in
    let cmt_loc = {label_loc.loc with loc_end = typ.ptyp_loc.loc_end} in
    print_comments doc cmt_tbl cmt_loc
  | Oinherit typexpr ->
    Doc.concat [Doc.dotdotdot; print_typ_expr ~state typexpr cmt_tbl]

(* es6 arrow type arg
 * type t = (~foo: string, ~bar: float=?, unit) => unit
 * i.e. ~foo: string, ~bar: float *)
and print_type_parameter ~state (attrs, lbl, typ) cmt_tbl =
  (* Converting .ml code to .res requires processing uncurried attributes *)
  let attrs = print_attributes ~state attrs cmt_tbl in
  let label =
    match lbl with
    | Asttypes.Nolabel -> Doc.nil
    | Labelled {txt = lbl} ->
      Doc.concat [Doc.text "~"; print_ident_like lbl; Doc.text ": "]
    | Optional {txt = lbl} ->
      Doc.concat [Doc.text "~"; print_ident_like lbl; Doc.text ": "]
  in
  let optional_indicator =
    match lbl with
    | Nolabel | Labelled _ -> Doc.nil
    | Optional _ -> Doc.text "=?"
  in
  let loc = {(Asttypes.get_lbl_loc lbl) with loc_end = typ.ptyp_loc.loc_end} in
  let doc =
    Doc.group
      (Doc.concat
         [attrs; label; print_typ_expr ~state typ cmt_tbl; optional_indicator])
  in
  print_comments doc cmt_tbl loc

and print_value_binding ~state ~rec_flag (vb : Parsetree.value_binding) cmt_tbl
    i =
  let attrs =
    print_attributes ~state ~loc:vb.pvb_pat.ppat_loc vb.pvb_attributes cmt_tbl
  in
  let header =
    if i == 0 then Doc.concat [Doc.text "let "; rec_flag] else Doc.text "and "
  in
  match vb with
  | {
   pvb_pat =
     {
       ppat_desc =
         Ppat_constraint (pattern, ({ptyp_desc = Ptyp_poly _} as pat_typ));
     };
   pvb_expr = {pexp_desc = Pexp_newtype _} as expr;
  } -> (
    let _, parameters, return_expr = ParsetreeViewer.fun_expr expr in
    let abstract_type =
      match parameters with
      | [NewTypes {locs = vars}] ->
        Doc.concat
          [
            Doc.text "type ";
            Doc.join ~sep:Doc.space
              (List.map (fun var -> Doc.text var.Asttypes.txt) vars);
            Doc.dot;
          ]
      | _ -> Doc.nil
    in
    match return_expr.pexp_desc with
    | Pexp_constraint (expr, typ) ->
      Doc.group
        (Doc.concat
           [
             attrs;
             header;
             print_pattern ~state pattern cmt_tbl;
             Doc.text ":";
             Doc.indent
               (Doc.concat
                  [
                    Doc.line;
                    abstract_type;
                    Doc.space;
                    print_typ_expr ~state typ cmt_tbl;
                    Doc.text " =";
                    Doc.concat
                      [
                        Doc.line;
                        print_expression_with_comments ~state expr cmt_tbl;
                      ];
                  ]);
           ])
    | _ ->
      (* Example:
       * let cancel_and_collect_callbacks:
       *   'a 'u 'c. (list<packed_callbacks>, promise<'a, 'u, 'c>) => list<packed_callbacks> =         *  (type x, callbacks_accumulator, p: promise<_, _, c>)
       *)
      Doc.group
        (Doc.concat
           [
             attrs;
             header;
             print_pattern ~state pattern cmt_tbl;
             Doc.text ":";
             Doc.indent
               (Doc.concat
                  [
                    Doc.line;
                    abstract_type;
                    Doc.space;
                    print_typ_expr ~state pat_typ cmt_tbl;
                    Doc.text " =";
                    Doc.concat
                      [
                        Doc.line;
                        print_expression_with_comments ~state expr cmt_tbl;
                      ];
                  ]);
           ]))
  | _ ->
    let opt_braces, expr = ParsetreeViewer.process_braces_attr vb.pvb_expr in
    let printed_expr =
      let doc = print_expression_with_comments ~state vb.pvb_expr cmt_tbl in
      match Parens.expr vb.pvb_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc expr braces
      | Nothing -> doc
    in
    let pattern_doc = print_pattern ~state vb.pvb_pat cmt_tbl in
    (*
     * we want to optimize the layout of one pipe:
     *   let tbl = data->Js.Array2.reduce((map, curr) => {
     *     ...
     *   })
     * important is that we don't do this for multiple pipes:
     *   let decoratorTags =
     *     items
     *     ->Js.Array2.filter(items => {items.category === Decorators})
     *     ->Belt.Array.map(...)
     * Multiple pipes chained together lend themselves more towards the last layout.
     *)
    if ParsetreeViewer.is_single_pipe_expr vb.pvb_expr then
      Doc.custom_layout
        [
          Doc.group
            (Doc.concat
               [
                 attrs;
                 header;
                 pattern_doc;
                 Doc.text " =";
                 Doc.space;
                 printed_expr;
               ]);
          Doc.group
            (Doc.concat
               [
                 attrs;
                 header;
                 pattern_doc;
                 Doc.text " =";
                 Doc.indent (Doc.concat [Doc.line; printed_expr]);
               ]);
        ]
    else
      let should_indent =
        match opt_braces with
        | Some _ -> false
        | _ -> (
          ParsetreeViewer.is_binary_expression expr
          ||
          match vb.pvb_expr with
          | {
           pexp_attributes = [({Location.txt = "res.ternary"}, _)];
           pexp_desc = Pexp_ifthenelse (if_expr, _, _);
          } ->
            ParsetreeViewer.is_binary_expression if_expr
            || ParsetreeViewer.has_attributes if_expr.pexp_attributes
          | {pexp_desc = Pexp_newtype _} -> false
          | {pexp_attributes = [({Location.txt = "res.taggedTemplate"}, _)]} ->
            false
          | e ->
            ParsetreeViewer.has_attributes e.pexp_attributes
            || ParsetreeViewer.is_array_access e)
      in
      Doc.group
        (Doc.concat
           [
             attrs;
             header;
             pattern_doc;
             Doc.text " =";
             (if should_indent then
                Doc.indent (Doc.concat [Doc.line; printed_expr])
              else Doc.concat [Doc.space; printed_expr]);
           ])

and print_package_type ~state ~print_module_keyword_and_parens
    (package_type : Parsetree.package_type) cmt_tbl =
  let doc =
    match package_type with
    | longident_loc, [] ->
      Doc.group (Doc.concat [print_longident_location longident_loc cmt_tbl])
    | longident_loc, package_constraints ->
      Doc.group
        (Doc.concat
           [
             print_longident_location longident_loc cmt_tbl;
             print_package_constraints ~state package_constraints cmt_tbl;
             Doc.soft_line;
           ])
  in
  if print_module_keyword_and_parens then
    Doc.concat [Doc.text "module("; doc; Doc.rparen]
  else doc

and print_package_constraints ~state package_constraints cmt_tbl =
  Doc.concat
    [
      Doc.text " with";
      Doc.indent
        (Doc.concat
           [
             Doc.line;
             Doc.join ~sep:Doc.line
               (List.mapi
                  (fun i pc ->
                    let longident, typexpr = pc in
                    let cmt_loc =
                      {
                        longident.Asttypes.loc with
                        loc_end = typexpr.Parsetree.ptyp_loc.loc_end;
                      }
                    in
                    let doc = print_package_constraint ~state i cmt_tbl pc in
                    print_comments doc cmt_tbl cmt_loc)
                  package_constraints);
           ]);
    ]

and print_package_constraint ~state i cmt_tbl (longident_loc, typ) =
  let prefix = if i == 0 then Doc.text "type " else Doc.text "and type " in
  Doc.concat
    [
      prefix;
      print_longident_location longident_loc cmt_tbl;
      Doc.text " = ";
      print_typ_expr ~state typ cmt_tbl;
    ]

and print_extension ~state ~at_module_lvl (string_loc, payload) cmt_tbl =
  let txt = string_loc.Location.txt in
  let ext_name =
    let doc =
      Doc.concat
        [
          Doc.text "%";
          (if at_module_lvl then Doc.text "%" else Doc.nil);
          Doc.text txt;
        ]
    in
    print_comments doc cmt_tbl string_loc.Location.loc
  in
  Doc.group (Doc.concat [ext_name; print_payload ~state payload cmt_tbl])

and print_pattern ~state (p : Parsetree.pattern) cmt_tbl =
  let pattern_without_attributes =
    match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var var -> print_ident_like var.txt
    | Ppat_constant c ->
      let template_literal =
        ParsetreeViewer.has_template_literal_attr p.ppat_attributes
      in
      print_constant ~template_literal c
    | Ppat_tuple patterns ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun pat -> print_pattern ~state pat cmt_tbl)
                         patterns);
                  ]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.rparen;
           ])
    | Ppat_array [] ->
      Doc.concat
        [Doc.lbracket; print_comments_inside cmt_tbl p.ppat_loc; Doc.rbracket]
    | Ppat_array patterns ->
      Doc.group
        (Doc.concat
           [
             Doc.text "[";
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun pat -> print_pattern ~state pat cmt_tbl)
                         patterns);
                  ]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.text "]";
           ])
    | Ppat_construct ({txt = Longident.Lident "()"}, _) ->
      Doc.concat
        [Doc.lparen; print_comments_inside cmt_tbl p.ppat_loc; Doc.rparen]
    | Ppat_construct ({txt = Longident.Lident "[]"}, _) ->
      Doc.concat
        [Doc.text "list{"; print_comments_inside cmt_tbl p.ppat_loc; Doc.rbrace]
    | Ppat_construct ({txt = Longident.Lident "::"}, _) ->
      let patterns, tail =
        ParsetreeViewer.collect_patterns_from_list_construct [] p
      in
      let should_hug =
        match (patterns, tail) with
        | [pat], {ppat_desc = Ppat_construct ({txt = Longident.Lident "[]"}, _)}
          when ParsetreeViewer.is_huggable_pattern pat ->
          true
        | _ -> false
      in
      let children =
        Doc.concat
          [
            (if should_hug then Doc.nil else Doc.soft_line);
            Doc.join
              ~sep:(Doc.concat [Doc.text ","; Doc.line])
              (List.map (fun pat -> print_pattern ~state pat cmt_tbl) patterns);
            (match tail.Parsetree.ppat_desc with
            | Ppat_construct ({txt = Longident.Lident "[]"}, _) -> Doc.nil
            | _ ->
              let doc =
                Doc.concat [Doc.text "..."; print_pattern ~state tail cmt_tbl]
              in
              let tail = print_comments doc cmt_tbl tail.ppat_loc in
              Doc.concat [Doc.text ","; Doc.line; tail]);
          ]
      in
      Doc.group
        (Doc.concat
           [
             Doc.text "list{";
             (if should_hug then children
              else
                Doc.concat
                  [
                    Doc.indent children;
                    Doc.if_breaks (Doc.text ",") Doc.nil;
                    Doc.soft_line;
                  ]);
             Doc.rbrace;
           ])
    | Ppat_construct (constr_name, constructor_args) ->
      let constr_name = print_longident_location constr_name cmt_tbl in
      let args_doc =
        match constructor_args with
        | None -> Doc.nil
        | Some
            {
              ppat_loc;
              ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _);
            } ->
          Doc.concat
            [Doc.lparen; print_comments_inside cmt_tbl ppat_loc; Doc.rparen]
        | Some {ppat_desc = Ppat_tuple []; ppat_loc = loc} ->
          Doc.concat [Doc.lparen; print_comments_inside cmt_tbl loc; Doc.rparen]
        (* Some((1, 2) *)
        | Some {ppat_desc = Ppat_tuple [({ppat_desc = Ppat_tuple _} as arg)]} ->
          Doc.concat [Doc.lparen; print_pattern ~state arg cmt_tbl; Doc.rparen]
        | Some {ppat_desc = Ppat_tuple patterns} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.soft_line;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun pat -> print_pattern ~state pat cmt_tbl)
                          patterns);
                   ]);
              Doc.trailing_comma;
              Doc.soft_line;
              Doc.rparen;
            ]
        | Some arg ->
          let arg_doc = print_pattern ~state arg cmt_tbl in
          let should_hug = ParsetreeViewer.is_huggable_pattern arg in
          Doc.concat
            [
              Doc.lparen;
              (if should_hug then arg_doc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.soft_line; arg_doc]);
                     Doc.trailing_comma;
                     Doc.soft_line;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [constr_name; args_doc])
    | Ppat_variant (label, None) ->
      Doc.concat [Doc.text "#"; print_poly_var_ident label]
    | Ppat_variant (label, variant_args) ->
      let variant_name =
        Doc.concat [Doc.text "#"; print_poly_var_ident label]
      in
      let args_doc =
        match variant_args with
        | None -> Doc.nil
        | Some {ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _)}
          ->
          Doc.text "()"
        | Some {ppat_desc = Ppat_tuple []; ppat_loc = loc} ->
          Doc.concat [Doc.lparen; print_comments_inside cmt_tbl loc; Doc.rparen]
        (* Some((1, 2) *)
        | Some {ppat_desc = Ppat_tuple [({ppat_desc = Ppat_tuple _} as arg)]} ->
          Doc.concat [Doc.lparen; print_pattern ~state arg cmt_tbl; Doc.rparen]
        | Some {ppat_desc = Ppat_tuple patterns} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.soft_line;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun pat -> print_pattern ~state pat cmt_tbl)
                          patterns);
                   ]);
              Doc.trailing_comma;
              Doc.soft_line;
              Doc.rparen;
            ]
        | Some arg ->
          let arg_doc = print_pattern ~state arg cmt_tbl in
          let should_hug = ParsetreeViewer.is_huggable_pattern arg in
          Doc.concat
            [
              Doc.lparen;
              (if should_hug then arg_doc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.soft_line; arg_doc]);
                     Doc.trailing_comma;
                     Doc.soft_line;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [variant_name; args_doc])
    | Ppat_type ident
      when ParsetreeViewer.has_res_pat_variant_spread_attribute
             p.ppat_attributes ->
      Doc.concat [Doc.text "..."; print_ident_path ident cmt_tbl]
    | Ppat_type ident ->
      Doc.concat [Doc.text "#..."; print_ident_path ident cmt_tbl]
    | Ppat_record (rows, _)
      when ParsetreeViewer.has_dict_pattern_attribute p.ppat_attributes ->
      Doc.concat
        [
          Doc.text "dict{";
          Doc.indent
            (Doc.concat
               [
                 Doc.soft_line;
                 Doc.join
                   ~sep:(Doc.concat [Doc.text ","; Doc.line])
                   (List.map
                      (fun row -> print_pattern_dict_row ~state row cmt_tbl)
                      rows);
               ]);
          Doc.if_breaks (Doc.text ",") Doc.nil;
          Doc.soft_line;
          Doc.rbrace;
        ]
    | Ppat_record ([], Open) ->
      Doc.concat [Doc.lbrace; Doc.text "_"; Doc.rbrace]
    | Ppat_record (rows, open_flag) ->
      Doc.group
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun row ->
                           print_pattern_record_row ~state row cmt_tbl)
                         rows);
                    (match open_flag with
                    | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                    | Closed -> Doc.nil);
                  ]);
             Doc.if_breaks (Doc.text ",") Doc.nil;
             Doc.soft_line;
             Doc.rbrace;
           ])
    | Ppat_exception p ->
      let needs_parens =
        match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
      in
      let pat =
        let p = print_pattern ~state p cmt_tbl in
        if needs_parens then Doc.concat [Doc.text "("; p; Doc.text ")"] else p
      in
      Doc.group (Doc.concat [Doc.text "exception"; Doc.line; pat])
    | Ppat_or _ ->
      (* Blue | Red | Green -> [Blue; Red; Green] *)
      let or_chain = ParsetreeViewer.collect_or_pattern_chain p in
      let docs =
        List.mapi
          (fun i pat ->
            let pattern_doc = print_pattern ~state pat cmt_tbl in
            Doc.concat
              [
                (if i == 0 then Doc.nil else Doc.concat [Doc.line; Doc.text "| "]);
                (match pat.ppat_desc with
                (* (Blue | Red) | (Green | Black) | White *)
                | Ppat_or _ -> add_parens pattern_doc
                | _ -> pattern_doc);
              ])
          or_chain
      in
      let is_spread_over_multiple_lines =
        match (or_chain, List.rev or_chain) with
        | first :: _, last :: _ ->
          first.ppat_loc.loc_start.pos_lnum < last.ppat_loc.loc_end.pos_lnum
        | _ -> false
      in
      Doc.breakable_group ~force_break:is_spread_over_multiple_lines
        (Doc.concat docs)
    | Ppat_extension ext ->
      print_extension ~state ~at_module_lvl:false ext cmt_tbl
    | Ppat_lazy p ->
      let needs_parens =
        match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
      in
      let pat =
        let p = print_pattern ~state p cmt_tbl in
        if needs_parens then Doc.concat [Doc.text "("; p; Doc.text ")"] else p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, alias_loc) ->
      let needs_parens =
        match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
      in
      let rendered_pattern =
        let p = print_pattern ~state p cmt_tbl in
        if needs_parens then Doc.concat [Doc.text "("; p; Doc.text ")"] else p
      in
      Doc.concat
        [rendered_pattern; Doc.text " as "; print_string_loc alias_loc cmt_tbl]
    (* Note: module(P : S) is represented as *)
    (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_constraint
        ( {ppat_desc = Ppat_unpack string_loc},
          {ptyp_desc = Ptyp_package package_type; ptyp_loc} ) ->
      Doc.concat
        [
          Doc.text "module(";
          print_comments (Doc.text string_loc.txt) cmt_tbl string_loc.loc;
          Doc.text ": ";
          print_comments
            (print_package_type ~state ~print_module_keyword_and_parens:false
               package_type cmt_tbl)
            cmt_tbl ptyp_loc;
          Doc.rparen;
        ]
    | Ppat_constraint (pattern, typ) ->
      Doc.concat
        [
          print_pattern ~state pattern cmt_tbl;
          Doc.text ": ";
          print_typ_expr ~state typ cmt_tbl;
        ]
    (* Note: module(P : S) is represented as *)
    (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_unpack string_loc ->
      Doc.concat
        [
          Doc.text "module(";
          print_comments (Doc.text string_loc.txt) cmt_tbl string_loc.loc;
          Doc.rparen;
        ]
    | Ppat_interval (a, b) ->
      Doc.concat [print_constant a; Doc.text " .. "; print_constant b]
    | Ppat_open _ -> Doc.nil
  in
  let doc =
    match p.ppat_attributes with
    | [] -> pattern_without_attributes
    | attrs ->
      Doc.group
        (Doc.concat
           [print_attributes ~state attrs cmt_tbl; pattern_without_attributes])
  in
  print_comments doc cmt_tbl p.ppat_loc

and print_pattern_record_row ~state row cmt_tbl =
  match row with
  (* punned {x}*)
  | ( ({Location.txt = Longident.Lident ident} as longident),
      {Parsetree.ppat_desc = Ppat_var {txt; _}; ppat_attributes},
      opt )
    when ident = txt ->
    Doc.concat
      [
        (if opt then Doc.text "?" else Doc.nil);
        print_attributes ~state ppat_attributes cmt_tbl;
        print_lident_path longident cmt_tbl;
      ]
  | longident, pattern, opt ->
    let loc_for_comments =
      {longident.loc with loc_end = pattern.Parsetree.ppat_loc.loc_end}
    in
    let rhs_doc =
      let doc = print_pattern ~state pattern cmt_tbl in
      let doc =
        if Parens.pattern_record_row_rhs pattern then add_parens doc else doc
      in
      if opt then Doc.concat [Doc.text "?"; doc] else doc
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             print_lident_path longident cmt_tbl;
             Doc.text ":";
             (if ParsetreeViewer.is_huggable_pattern pattern then
                Doc.concat [Doc.space; rhs_doc]
              else Doc.indent (Doc.concat [Doc.line; rhs_doc]));
           ])
    in
    print_comments doc cmt_tbl loc_for_comments

and print_pattern_dict_row ~state
    ((longident, pattern, opt) :
      Longident.t Location.loc * Parsetree.pattern * bool) cmt_tbl =
  let loc_for_comments =
    {longident.loc with loc_end = pattern.ppat_loc.loc_end}
  in
  let rhs_doc =
    let doc = print_pattern ~state pattern cmt_tbl in
    let doc =
      if Parens.pattern_record_row_rhs pattern then add_parens doc else doc
    in
    if opt then Doc.concat [Doc.text "?"; doc] else doc
  in
  let lbl_doc =
    Doc.concat [Doc.text "\""; print_longident longident.txt; Doc.text "\""]
  in
  let doc =
    Doc.group
      (Doc.concat
         [
           lbl_doc;
           Doc.text ":";
           (if ParsetreeViewer.is_huggable_pattern pattern then
              Doc.concat [Doc.space; rhs_doc]
            else Doc.indent (Doc.concat [Doc.line; rhs_doc]));
         ])
  in
  print_comments doc cmt_tbl loc_for_comments

and print_expression_with_comments ~state expr cmt_tbl : Doc.t =
  let doc = print_expression ~state expr cmt_tbl in
  print_comments doc cmt_tbl expr.Parsetree.pexp_loc

and print_if_chain ~state pexp_attributes ifs else_expr cmt_tbl =
  let if_docs =
    Doc.join ~sep:Doc.space
      (List.mapi
         (fun i (outer_loc, if_expr, then_expr) ->
           let if_txt = if i > 0 then Doc.text "else if " else Doc.text "if " in
           let doc =
             match if_expr with
             | ParsetreeViewer.If if_expr ->
               let condition =
                 if ParsetreeViewer.is_block_expr if_expr then
                   print_expression_block ~state ~braces:true if_expr cmt_tbl
                 else
                   let doc =
                     print_expression_with_comments ~state if_expr cmt_tbl
                   in
                   match Parens.expr if_expr with
                   | Parens.Parenthesized -> add_parens doc
                   | Braced braces -> print_braces doc if_expr braces
                   | Nothing -> Doc.if_breaks (add_parens doc) doc
               in
               Doc.concat
                 [
                   if_txt;
                   Doc.group condition;
                   Doc.space;
                   (let then_expr =
                      match ParsetreeViewer.process_braces_attr then_expr with
                      (* This case only happens when coming from Reason, we strip braces *)
                      | Some _, expr -> expr
                      | _ -> then_expr
                    in
                    print_expression_block ~state ~braces:true then_expr cmt_tbl);
                 ]
             | IfLet (pattern, condition_expr) ->
               let condition_doc =
                 let doc =
                   print_expression_with_comments ~state condition_expr cmt_tbl
                 in
                 match Parens.expr condition_expr with
                 | Parens.Parenthesized -> add_parens doc
                 | Braced braces -> print_braces doc condition_expr braces
                 | Nothing -> doc
               in
               Doc.concat
                 [
                   if_txt;
                   Doc.text "let ";
                   print_pattern ~state pattern cmt_tbl;
                   Doc.text " = ";
                   condition_doc;
                   Doc.space;
                   print_expression_block ~state ~braces:true then_expr cmt_tbl;
                 ]
           in
           print_leading_comments doc cmt_tbl.leading outer_loc)
         ifs)
  in
  let else_doc =
    match else_expr with
    | None -> Doc.nil
    | Some expr ->
      Doc.concat
        [
          Doc.text " else ";
          print_expression_block ~state ~braces:true expr cmt_tbl;
        ]
  in
  let attrs = ParsetreeViewer.filter_fragile_match_attributes pexp_attributes in
  Doc.concat [print_attributes ~state attrs cmt_tbl; if_docs; else_doc]

and print_expression ~state (e : Parsetree.expression) cmt_tbl =
  let print_arrow e =
    let async, parameters, return_expr = ParsetreeViewer.fun_expr e in
    let attrs_on_arrow = e.pexp_attributes in
    let return_expr, typ_constraint =
      match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ) ->
        ( {
            expr with
            pexp_attributes =
              List.concat [expr.pexp_attributes; return_expr.pexp_attributes];
          },
          Some typ )
      | _ -> (return_expr, None)
    in
    let has_constraint =
      match typ_constraint with
      | Some _ -> true
      | None -> false
    in
    let parameters_doc =
      print_expr_fun_parameters ~state ~in_callback:NoCallback ~async
        ~has_constraint parameters cmt_tbl
    in
    let return_expr_doc =
      let opt_braces, _ = ParsetreeViewer.process_braces_attr return_expr in
      let should_inline =
        match (return_expr.pexp_desc, opt_braces) with
        | _, Some _ -> true
        | ( ( Pexp_array _ | Pexp_tuple _
            | Pexp_construct (_, Some _)
            | Pexp_record _ ),
            _ ) ->
          true
        | _ -> false
      in
      let should_indent =
        match return_expr.pexp_desc with
        | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _
        | Pexp_open _ ->
          false
        | _ -> true
      in
      let return_doc =
        let doc = print_expression_with_comments ~state return_expr cmt_tbl in
        match Parens.expr return_expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc return_expr braces
        | Nothing -> doc
      in
      if should_inline then Doc.concat [Doc.space; return_doc]
      else
        Doc.group
          (if should_indent then Doc.indent (Doc.concat [Doc.line; return_doc])
           else Doc.concat [Doc.space; return_doc])
    in
    let typ_constraint_doc =
      match typ_constraint with
      | Some typ ->
        let typ_doc =
          let doc = print_typ_expr ~state typ cmt_tbl in
          if Parens.arrow_return_typ_expr typ then add_parens doc else doc
        in
        Doc.concat [Doc.text ": "; typ_doc]
      | _ -> Doc.nil
    in
    let attrs = print_attributes ~state attrs_on_arrow cmt_tbl in
    Doc.group
      (Doc.concat
         [
           attrs;
           parameters_doc;
           typ_constraint_doc;
           Doc.text " =>";
           return_expr_doc;
         ])
  in
  let uncurried = Ast_uncurried.expr_is_uncurried_fun e in
  let e_fun =
    if uncurried then Ast_uncurried.expr_extract_uncurried_fun e else e
  in
  let printed_expression =
    match e_fun.pexp_desc with
    | Pexp_fun
        {
          arg_label = Nolabel;
          default = None;
          lhs = {ppat_desc = Ppat_var {txt = "__x"}};
          rhs = {pexp_desc = Pexp_apply _};
        } ->
      (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
      print_expression_with_comments ~state
        (ParsetreeViewer.rewrite_underscore_apply e_fun)
        cmt_tbl
    | Pexp_fun _ | Pexp_newtype _ -> print_arrow e
    | Parsetree.Pexp_constant c ->
      print_constant ~template_literal:(ParsetreeViewer.is_template_literal e) c
    | Pexp_construct _ when ParsetreeViewer.has_jsx_attribute e.pexp_attributes
      ->
      print_jsx_fragment ~state e cmt_tbl
    | Pexp_construct ({txt = Longident.Lident "()"}, _) -> Doc.text "()"
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      Doc.concat
        [Doc.text "list{"; print_comments_inside cmt_tbl e.pexp_loc; Doc.rbrace]
    | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
      let expressions, spread = ParsetreeViewer.collect_list_expressions e in
      let spread_doc =
        match spread with
        | Some expr ->
          Doc.concat
            [
              Doc.text ",";
              Doc.line;
              Doc.dotdotdot;
              (let doc = print_expression_with_comments ~state expr cmt_tbl in
               match Parens.expr expr with
               | Parens.Parenthesized -> add_parens doc
               | Braced braces -> print_braces doc expr braces
               | Nothing -> doc);
            ]
        | None -> Doc.nil
      in
      Doc.group
        (Doc.concat
           [
             Doc.text "list{";
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun expr ->
                           let doc =
                             print_expression_with_comments ~state expr cmt_tbl
                           in
                           match Parens.expr expr with
                           | Parens.Parenthesized -> add_parens doc
                           | Braced braces -> print_braces doc expr braces
                           | Nothing -> doc)
                         expressions);
                    spread_doc;
                  ]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.rbrace;
           ])
    | Pexp_construct (longident_loc, args) ->
      let constr = print_longident_location longident_loc cmt_tbl in
      let args =
        match args with
        | None -> Doc.nil
        | Some {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}
          ->
          Doc.text "()"
        (* Some((1, 2)) *)
        | Some {pexp_desc = Pexp_tuple [({pexp_desc = Pexp_tuple _} as arg)]} ->
          Doc.concat
            [
              Doc.lparen;
              (let doc = print_expression_with_comments ~state arg cmt_tbl in
               match Parens.expr arg with
               | Parens.Parenthesized -> add_parens doc
               | Braced braces -> print_braces doc arg braces
               | Nothing -> doc);
              Doc.rparen;
            ]
        | Some {pexp_desc = Pexp_tuple args} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.soft_line;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun expr ->
                            let doc =
                              print_expression_with_comments ~state expr cmt_tbl
                            in
                            match Parens.expr expr with
                            | Parens.Parenthesized -> add_parens doc
                            | Braced braces -> print_braces doc expr braces
                            | Nothing -> doc)
                          args);
                   ]);
              Doc.trailing_comma;
              Doc.soft_line;
              Doc.rparen;
            ]
        | Some arg ->
          let arg_doc =
            let doc = print_expression_with_comments ~state arg cmt_tbl in
            match Parens.expr arg with
            | Parens.Parenthesized -> add_parens doc
            | Braced braces -> print_braces doc arg braces
            | Nothing -> doc
          in
          let should_hug = ParsetreeViewer.is_huggable_expression arg in
          Doc.concat
            [
              Doc.lparen;
              (if should_hug then arg_doc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.soft_line; arg_doc]);
                     Doc.trailing_comma;
                     Doc.soft_line;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [constr; args])
    | Pexp_ident path -> print_lident_path path cmt_tbl
    | Pexp_tuple exprs ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun expr ->
                           let doc =
                             print_expression_with_comments ~state expr cmt_tbl
                           in
                           match Parens.expr expr with
                           | Parens.Parenthesized -> add_parens doc
                           | Braced braces -> print_braces doc expr braces
                           | Nothing -> doc)
                         exprs);
                  ]);
             Doc.if_breaks (Doc.text ",") Doc.nil;
             Doc.soft_line;
             Doc.rparen;
           ])
    | Pexp_array [] ->
      Doc.concat
        [Doc.lbracket; print_comments_inside cmt_tbl e.pexp_loc; Doc.rbracket]
    | Pexp_array exprs ->
      Doc.group
        (Doc.concat
           [
             Doc.lbracket;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun expr ->
                           let doc =
                             print_expression_with_comments ~state expr cmt_tbl
                           in
                           match Parens.expr expr with
                           | Parens.Parenthesized -> add_parens doc
                           | Braced braces -> print_braces doc expr braces
                           | Nothing -> doc)
                         exprs);
                  ]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.rbracket;
           ])
    | Pexp_variant (label, args) ->
      let variant_name =
        Doc.concat [Doc.text "#"; print_poly_var_ident label]
      in
      let args =
        match args with
        | None -> Doc.nil
        | Some {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}
          ->
          Doc.text "()"
        (* #poly((1, 2) *)
        | Some {pexp_desc = Pexp_tuple [({pexp_desc = Pexp_tuple _} as arg)]} ->
          Doc.concat
            [
              Doc.lparen;
              (let doc = print_expression_with_comments ~state arg cmt_tbl in
               match Parens.expr arg with
               | Parens.Parenthesized -> add_parens doc
               | Braced braces -> print_braces doc arg braces
               | Nothing -> doc);
              Doc.rparen;
            ]
        | Some {pexp_desc = Pexp_tuple args} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.soft_line;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun expr ->
                            let doc =
                              print_expression_with_comments ~state expr cmt_tbl
                            in
                            match Parens.expr expr with
                            | Parens.Parenthesized -> add_parens doc
                            | Braced braces -> print_braces doc expr braces
                            | Nothing -> doc)
                          args);
                   ]);
              Doc.trailing_comma;
              Doc.soft_line;
              Doc.rparen;
            ]
        | Some arg ->
          let arg_doc =
            let doc = print_expression_with_comments ~state arg cmt_tbl in
            match Parens.expr arg with
            | Parens.Parenthesized -> add_parens doc
            | Braced braces -> print_braces doc arg braces
            | Nothing -> doc
          in
          let should_hug = ParsetreeViewer.is_huggable_expression arg in
          Doc.concat
            [
              Doc.lparen;
              (if should_hug then arg_doc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.soft_line; arg_doc]);
                     Doc.trailing_comma;
                     Doc.soft_line;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [variant_name; args])
    | Pexp_record (rows, spread_expr) ->
      if rows = [] then
        Doc.concat
          [Doc.lbrace; print_comments_inside cmt_tbl e.pexp_loc; Doc.rbrace]
      else
        let spread =
          match spread_expr with
          | None -> Doc.nil
          | Some ({pexp_desc} as expr) ->
            let doc =
              match pexp_desc with
              | Pexp_ident {txt = expr} -> print_lident expr
              | _ -> print_expression ~state expr cmt_tbl
            in
            let doc_with_spread =
              Doc.concat
                [
                  Doc.dotdotdot;
                  (match Parens.expr expr with
                  | Parens.Parenthesized -> add_parens doc
                  | Braced braces -> print_braces doc expr braces
                  | Nothing -> doc);
                ]
            in
            Doc.concat
              [
                print_comments doc_with_spread cmt_tbl expr.Parsetree.pexp_loc;
                Doc.comma;
                Doc.line;
              ]
        in
        (* If the record is written over multiple lines, break automatically
         * `let x = {a: 1, b: 3}` -> same line, break when line-width exceeded
         * `let x = {
         *   a: 1,
         *   b: 2,
         *  }` -> record is written on multiple lines, break the group *)
        let force_break =
          e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
        in
        let punning_allowed =
          match (spread_expr, rows) with
          | None, [_] -> false (* disallow punning for single-element records *)
          | _ -> true
        in
        Doc.breakable_group ~force_break
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.soft_line;
                      spread;
                      Doc.join
                        ~sep:(Doc.concat [Doc.text ","; Doc.line])
                        (List.map
                           (fun row ->
                             print_expression_record_row ~state row cmt_tbl
                               punning_allowed)
                           rows);
                    ]);
               Doc.trailing_comma;
               Doc.soft_line;
               Doc.rbrace;
             ])
    | Pexp_extension extension -> (
      match extension with
      | ( {txt = "obj"},
          PStr
            [
              {
                pstr_loc = loc;
                pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (rows, _)}, []);
              };
            ] ) ->
        (* If the object is written over multiple lines, break automatically
         * `let x = {"a": 1, "b": 3}` -> same line, break when line-width exceeded
         * `let x = {
         *   "a": 1,
         *   "b": 2,
         *  }` -> object is written on multiple lines, break the group *)
        let force_break = loc.loc_start.pos_lnum < loc.loc_end.pos_lnum in
        Doc.breakable_group ~force_break
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.soft_line;
                      Doc.join
                        ~sep:(Doc.concat [Doc.text ","; Doc.line])
                        (List.map
                           (fun (loc, e, _opt) ->
                             print_bs_object_row ~state (loc, e) cmt_tbl)
                           rows);
                    ]);
               Doc.trailing_comma;
               Doc.soft_line;
               Doc.rbrace;
             ])
      | ( {txt = "re"},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_constant (Pconst_string (expr, _))}, []);
              };
            ] ) ->
        Doc.text expr
      | extension ->
        print_extension ~state ~at_module_lvl:false extension cmt_tbl)
    | Pexp_apply
        {funct = e; args = [(Nolabel, {pexp_desc = Pexp_array sub_lists})]}
      when ParsetreeViewer.is_spread_belt_array_concat e ->
      print_belt_array_concat_apply ~state sub_lists cmt_tbl
    | Pexp_apply
        {funct = e; args = [(Nolabel, {pexp_desc = Pexp_array sub_lists})]}
      when ParsetreeViewer.is_spread_belt_list_concat e ->
      print_belt_list_concat_apply ~state sub_lists cmt_tbl
    | Pexp_apply {funct = call_expr; args} ->
      if ParsetreeViewer.is_unary_expression e then
        print_unary_expression ~state e cmt_tbl
      else if ParsetreeViewer.is_template_literal e then
        print_template_literal ~state e cmt_tbl
      else if ParsetreeViewer.is_tagged_template_literal e then
        print_tagged_template_literal ~state call_expr args cmt_tbl
      else if ParsetreeViewer.is_binary_expression e then
        print_binary_expression ~state e cmt_tbl
      else print_pexp_apply ~state e cmt_tbl
    | Pexp_field (expr, longident_loc) ->
      let lhs =
        let doc = print_expression_with_comments ~state expr cmt_tbl in
        match Parens.field_expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [lhs; Doc.dot; print_lident_path longident_loc cmt_tbl]
    | Pexp_setfield (expr1, longident_loc, expr2) ->
      print_set_field_expr ~state e.pexp_attributes expr1 longident_loc expr2
        e.pexp_loc cmt_tbl
    | Pexp_ifthenelse (_ifExpr, _thenExpr, _elseExpr)
      when ParsetreeViewer.is_ternary_expr e ->
      let parts, alternate = ParsetreeViewer.collect_ternary_parts e in
      let ternary_doc =
        match parts with
        | (condition1, consequent1) :: rest ->
          Doc.group
            (Doc.concat
               [
                 print_ternary_operand ~state condition1 cmt_tbl;
                 Doc.indent
                   (Doc.concat
                      [
                        Doc.line;
                        Doc.indent
                          (Doc.concat
                             [
                               Doc.text "? ";
                               print_ternary_operand ~state consequent1 cmt_tbl;
                             ]);
                        Doc.concat
                          (List.map
                             (fun (condition, consequent) ->
                               Doc.concat
                                 [
                                   Doc.line;
                                   Doc.text ": ";
                                   print_ternary_operand ~state condition
                                     cmt_tbl;
                                   Doc.line;
                                   Doc.text "? ";
                                   print_ternary_operand ~state consequent
                                     cmt_tbl;
                                 ])
                             rest);
                        Doc.line;
                        Doc.text ": ";
                        Doc.indent
                          (print_ternary_operand ~state alternate cmt_tbl);
                      ]);
               ])
        | _ -> Doc.nil
      in
      let attrs = ParsetreeViewer.filter_ternary_attributes e.pexp_attributes in
      let needs_parens =
        match ParsetreeViewer.filter_parsing_attrs attrs with
        | [] -> false
        | _ -> true
      in
      Doc.concat
        [
          print_attributes ~state attrs cmt_tbl;
          (if needs_parens then add_parens ternary_doc else ternary_doc);
        ]
    | Pexp_ifthenelse (_ifExpr, _thenExpr, _elseExpr) ->
      let ifs, else_expr = ParsetreeViewer.collect_if_expressions e in
      print_if_chain ~state e.pexp_attributes ifs else_expr cmt_tbl
    | Pexp_while (expr1, expr2) ->
      let condition =
        let doc = print_expression_with_comments ~state expr1 cmt_tbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr1 braces
        | Nothing -> doc
      in
      Doc.breakable_group ~force_break:true
        (Doc.concat
           [
             Doc.text "while ";
             (if ParsetreeViewer.is_block_expr expr1 then condition
              else Doc.group (Doc.if_breaks (add_parens condition) condition));
             Doc.space;
             print_expression_block ~state ~braces:true expr2 cmt_tbl;
           ])
    | Pexp_for (pattern, from_expr, to_expr, direction_flag, body) ->
      Doc.breakable_group ~force_break:true
        (Doc.concat
           [
             Doc.text "for ";
             print_pattern ~state pattern cmt_tbl;
             Doc.text " in ";
             (let doc =
                print_expression_with_comments ~state from_expr cmt_tbl
              in
              match Parens.expr from_expr with
              | Parens.Parenthesized -> add_parens doc
              | Braced braces -> print_braces doc from_expr braces
              | Nothing -> doc);
             print_direction_flag direction_flag;
             (let doc = print_expression_with_comments ~state to_expr cmt_tbl in
              match Parens.expr to_expr with
              | Parens.Parenthesized -> add_parens doc
              | Braced braces -> print_braces doc to_expr braces
              | Nothing -> doc);
             Doc.space;
             print_expression_block ~state ~braces:true body cmt_tbl;
           ])
    | Pexp_constraint
        ( {pexp_desc = Pexp_pack mod_expr},
          {ptyp_desc = Ptyp_package package_type; ptyp_loc} ) ->
      Doc.group
        (Doc.concat
           [
             Doc.text "module(";
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    print_mod_expr ~state mod_expr cmt_tbl;
                    Doc.text ": ";
                    print_comments
                      (print_package_type ~state
                         ~print_module_keyword_and_parens:false package_type
                         cmt_tbl)
                      cmt_tbl ptyp_loc;
                  ]);
             Doc.soft_line;
             Doc.rparen;
           ])
    | Pexp_constraint (expr, typ) ->
      let expr_doc =
        let doc = print_expression_with_comments ~state expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [expr_doc; Doc.text ": "; print_typ_expr ~state typ cmt_tbl]
    | Pexp_letmodule ({txt = _modName}, _modExpr, _expr) ->
      print_expression_block ~state ~braces:true e cmt_tbl
    | Pexp_letexception (_extensionConstructor, _expr) ->
      print_expression_block ~state ~braces:true e cmt_tbl
    | Pexp_assert expr ->
      let expr = print_expression_with_comments ~state expr cmt_tbl in
      Doc.concat [Doc.text "assert("; expr; Doc.text ")"]
    | Pexp_lazy expr ->
      let rhs =
        let doc = print_expression_with_comments ~state expr cmt_tbl in
        match Parens.lazy_or_assert_or_await_expr_rhs expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.group (Doc.concat [Doc.text "lazy "; rhs])
    | Pexp_open (_overrideFlag, _longidentLoc, _expr) ->
      print_expression_block ~state ~braces:true e cmt_tbl
    | Pexp_pack mod_expr ->
      Doc.group
        (Doc.concat
           [
             Doc.text "module(";
             Doc.indent
               (Doc.concat
                  [Doc.soft_line; print_mod_expr ~state mod_expr cmt_tbl]);
             Doc.soft_line;
             Doc.rparen;
           ])
    | Pexp_sequence _ -> print_expression_block ~state ~braces:true e cmt_tbl
    | Pexp_let _ -> print_expression_block ~state ~braces:true e cmt_tbl
    | Pexp_try (expr, cases) ->
      let expr_doc =
        let doc = print_expression_with_comments ~state expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat
        [
          Doc.text "try ";
          expr_doc;
          Doc.text " catch ";
          print_cases ~state cases cmt_tbl;
        ]
    | Pexp_match (_, [_; _]) when ParsetreeViewer.is_if_let_expr e ->
      let ifs, else_expr = ParsetreeViewer.collect_if_expressions e in
      print_if_chain ~state e.pexp_attributes ifs else_expr cmt_tbl
    | Pexp_match (expr, cases) ->
      let expr_doc =
        let doc = print_expression_with_comments ~state expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.concat
        [
          Doc.text "switch ";
          expr_doc;
          Doc.space;
          print_cases ~state cases cmt_tbl;
        ]
    | Pexp_coerce (expr, (), typ) ->
      let doc_expr = print_expression_with_comments ~state expr cmt_tbl in
      let doc_typ = print_typ_expr ~state typ cmt_tbl in
      Doc.concat [Doc.lparen; doc_expr; Doc.text " :> "; doc_typ; Doc.rparen]
    | Pexp_send (parent_expr, label) ->
      let parent_doc =
        let doc = print_expression_with_comments ~state parent_expr cmt_tbl in
        match Parens.unary_expr_operand parent_expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc parent_expr braces
        | Nothing -> doc
      in
      let member =
        let member_doc =
          print_comments (Doc.text label.txt) cmt_tbl label.loc
        in
        Doc.concat [Doc.text "\""; member_doc; Doc.text "\""]
      in
      Doc.group (Doc.concat [parent_doc; Doc.lbracket; member; Doc.rbracket])
  in
  let expr_with_await =
    if ParsetreeViewer.has_await_attribute e.pexp_attributes then
      let rhs =
        match
          Parens.lazy_or_assert_or_await_expr_rhs ~in_await:true
            {
              e with
              pexp_attributes =
                List.filter
                  (function
                    | {Location.txt = "res.braces" | "ns.braces"}, _ -> false
                    | _ -> true)
                  e.pexp_attributes;
            }
        with
        | Parens.Parenthesized -> add_parens printed_expression
        | Braced braces -> print_braces printed_expression e braces
        | Nothing -> printed_expression
      in
      Doc.concat [Doc.text "await "; rhs]
    else printed_expression
  in
  let should_print_its_own_attributes =
    match e.pexp_desc with
    | Pexp_apply _ | Pexp_fun _ | Pexp_newtype _ | Pexp_setfield _
    | Pexp_ifthenelse _ ->
      true
    | Pexp_match _ when ParsetreeViewer.is_if_let_expr e -> true
    | Pexp_construct _ when ParsetreeViewer.has_jsx_attribute e.pexp_attributes
      ->
      true
    | _ -> false
  in
  match e.pexp_attributes with
  | [] -> expr_with_await
  | attrs when not should_print_its_own_attributes ->
    Doc.group
      (Doc.concat [print_attributes ~state attrs cmt_tbl; expr_with_await])
  | _ -> expr_with_await

and print_pexp_fun ~state ~in_callback e cmt_tbl =
  let async, parameters, return_expr = ParsetreeViewer.fun_expr e in
  let attrs_on_arrow = e.pexp_attributes in
  let return_expr, typ_constraint =
    match return_expr.pexp_desc with
    | Pexp_constraint (expr, typ) ->
      ( {
          expr with
          pexp_attributes =
            List.concat [expr.pexp_attributes; return_expr.pexp_attributes];
        },
        Some typ )
    | _ -> (return_expr, None)
  in
  let parameters_doc =
    print_expr_fun_parameters ~state ~in_callback ~async
      ~has_constraint:
        (match typ_constraint with
        | Some _ -> true
        | None -> false)
      parameters cmt_tbl
  in
  let return_should_indent =
    match return_expr.pexp_desc with
    | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _
    | Pexp_open _ ->
      false
    | _ -> true
  in
  let return_expr_doc =
    let opt_braces, _ = ParsetreeViewer.process_braces_attr return_expr in
    let should_inline =
      match (return_expr.pexp_desc, opt_braces) with
      | _, Some _ -> true
      | ( ( Pexp_array _ | Pexp_tuple _
          | Pexp_construct (_, Some _)
          | Pexp_record _ ),
          _ ) ->
        true
      | _ -> false
    in
    let return_doc =
      let doc = print_expression_with_comments ~state return_expr cmt_tbl in
      match Parens.expr return_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc return_expr braces
      | Nothing -> doc
    in
    if should_inline then Doc.concat [Doc.space; return_doc]
    else
      Doc.group
        (if return_should_indent then
           Doc.concat
             [
               Doc.indent (Doc.concat [Doc.line; return_doc]);
               (match in_callback with
               | FitsOnOneLine | ArgumentsFitOnOneLine -> Doc.soft_line
               | _ -> Doc.nil);
             ]
         else Doc.concat [Doc.space; return_doc])
  in
  let typ_constraint_doc =
    match typ_constraint with
    | Some typ -> Doc.concat [Doc.text ": "; print_typ_expr ~state typ cmt_tbl]
    | _ -> Doc.nil
  in
  Doc.concat
    [
      print_attributes ~state attrs_on_arrow cmt_tbl;
      parameters_doc;
      typ_constraint_doc;
      Doc.text " =>";
      return_expr_doc;
    ]

and print_ternary_operand ~state expr cmt_tbl =
  let doc = print_expression_with_comments ~state expr cmt_tbl in
  match Parens.ternary_operand expr with
  | Parens.Parenthesized -> add_parens doc
  | Braced braces -> print_braces doc expr braces
  | Nothing -> doc

and print_set_field_expr ~state attrs lhs longident_loc rhs loc cmt_tbl =
  let rhs_doc =
    let doc = print_expression_with_comments ~state rhs cmt_tbl in
    match Parens.set_field_expr_rhs rhs with
    | Parens.Parenthesized -> add_parens doc
    | Braced braces -> print_braces doc rhs braces
    | Nothing -> doc
  in
  let lhs_doc =
    let doc = print_expression_with_comments ~state lhs cmt_tbl in
    match Parens.field_expr lhs with
    | Parens.Parenthesized -> add_parens doc
    | Braced braces -> print_braces doc lhs braces
    | Nothing -> doc
  in
  let should_indent = ParsetreeViewer.is_binary_expression rhs in
  let doc =
    Doc.group
      (Doc.concat
         [
           lhs_doc;
           Doc.dot;
           print_lident_path longident_loc cmt_tbl;
           Doc.text " =";
           (if should_indent then
              Doc.group (Doc.indent (Doc.concat [Doc.line; rhs_doc]))
            else Doc.concat [Doc.space; rhs_doc]);
         ])
  in
  let doc =
    match attrs with
    | [] -> doc
    | attrs ->
      Doc.group (Doc.concat [print_attributes ~state attrs cmt_tbl; doc])
  in
  print_comments doc cmt_tbl loc

and print_template_literal ~state expr cmt_tbl =
  let tag = ref "js" in
  let rec walk_expr expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "++"}};
          args = [(Nolabel, arg1); (Nolabel, arg2)];
        } ->
      let lhs = walk_expr arg1 in
      let rhs = walk_expr arg2 in
      Doc.concat [lhs; rhs]
    | Pexp_constant (Pconst_string (txt, Some prefix)) ->
      tag := prefix;
      print_string_contents txt
    | _ ->
      let doc = print_expression_with_comments ~state expr cmt_tbl in
      let doc =
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      Doc.group (Doc.concat [Doc.text "${"; Doc.indent doc; Doc.rbrace])
  in
  let content = walk_expr expr in
  Doc.concat
    [
      (if !tag = "js" then Doc.nil else Doc.text !tag);
      Doc.text "`";
      content;
      Doc.text "`";
    ]

and print_tagged_template_literal ~state call_expr args cmt_tbl =
  let strings_list, values_list =
    match args with
    | [
     (_, {Parsetree.pexp_desc = Pexp_array strings});
     (_, {Parsetree.pexp_desc = Pexp_array values});
    ] ->
      (strings, values)
    | _ -> assert false
  in

  let strings =
    List.map
      (fun x ->
        match x with
        | {Parsetree.pexp_desc = Pexp_constant (Pconst_string (txt, _))} ->
          print_string_contents txt
        | _ -> assert false)
      strings_list
  in

  let values =
    List.map
      (fun x ->
        Doc.concat
          [
            Doc.text "${";
            print_expression_with_comments ~state x cmt_tbl;
            Doc.text "}";
          ])
      values_list
  in

  let process strings values =
    let rec aux acc = function
      | [], [] -> acc
      | a_head :: a_rest, b -> aux (Doc.concat [acc; a_head]) (b, a_rest)
      | _ -> assert false
    in
    aux Doc.nil (strings, values)
  in

  let content : Doc.t = process strings values in

  let tag = print_expression_with_comments ~state call_expr cmt_tbl in
  Doc.concat [tag; Doc.text "`"; content; Doc.text "`"]

and print_unary_expression ~state expr cmt_tbl =
  let print_unary_operator op =
    Doc.text
      (match op with
      | "~+" -> "+"
      | "~+." -> "+."
      | "~-" -> "-"
      | "~-." -> "-."
      | "not" -> "!"
      | _ -> assert false)
  in
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident operator}};
        args = [(Nolabel, operand)];
      } ->
    let printed_operand =
      let doc = print_expression_with_comments ~state operand cmt_tbl in
      match Parens.unary_expr_operand operand with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc operand braces
      | Nothing -> doc
    in
    let doc = Doc.concat [print_unary_operator operator; printed_operand] in
    print_comments doc cmt_tbl expr.pexp_loc
  | _ -> assert false

and print_binary_expression ~state (expr : Parsetree.expression) cmt_tbl =
  let print_binary_operator ~inline_rhs operator =
    let spacing_before_operator =
      if operator = "->" then Doc.soft_line
      else if operator = "|>" then Doc.line
      else Doc.space
    in
    let spacing_after_operator =
      if operator = "->" then Doc.nil
      else if operator = "|>" then Doc.space
      else if inline_rhs then Doc.space
      else Doc.line
    in
    Doc.concat
      [spacing_before_operator; Doc.text operator; spacing_after_operator]
  in
  let print_operand ~is_lhs ~is_multiline expr parent_operator =
    let rec flatten ~is_lhs ~is_multiline expr parent_operator =
      if ParsetreeViewer.is_binary_expression expr then
        match expr with
        | {
         pexp_desc =
           Pexp_apply
             {
               funct = {pexp_desc = Pexp_ident {txt = Longident.Lident operator}};
               args = [(_, left); (_, right)];
             };
        } ->
          if
            ParsetreeViewer.flattenable_operators parent_operator operator
            && not (ParsetreeViewer.has_attributes expr.pexp_attributes)
          then
            let left_printed =
              flatten ~is_lhs:true ~is_multiline left operator
            in
            let right_printed =
              let right_printeable_attrs, right_internal_attrs =
                ParsetreeViewer.partition_printable_attributes
                  right.pexp_attributes
              in
              let doc =
                print_expression_with_comments ~state
                  {right with pexp_attributes = right_internal_attrs}
                  cmt_tbl
              in
              let doc =
                if Parens.flatten_operand_rhs parent_operator right then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else doc
              in
              let doc =
                Doc.concat
                  [print_attributes ~state right_printeable_attrs cmt_tbl; doc]
              in
              match right_printeable_attrs with
              | [] -> doc
              | _ -> add_parens doc
            in
            let is_await =
              ParsetreeViewer.has_await_attribute expr.pexp_attributes
            in
            let doc =
              if is_await then
                let parens =
                  Res_parens.binary_operator_inside_await_needs_parens operator
                in
                Doc.concat
                  [
                    Doc.lparen;
                    Doc.text "await ";
                    (if parens then Doc.lparen else Doc.nil);
                    left_printed;
                    print_binary_operator ~inline_rhs:false operator;
                    right_printed;
                    (if parens then Doc.rparen else Doc.nil);
                    Doc.rparen;
                  ]
              else
                match operator with
                | "->" when is_multiline ->
                  (* If the pipe-chain is written over multiple lines, break automatically
                   * `let x = a->b->c -> same line, break when line-width exceeded
                   * `let x = a->
                   *   b->c` -> pipe-chain is written on multiple lines, break the group *)
                  Doc.breakable_group ~force_break:true
                    (Doc.concat
                       [
                         left_printed;
                         print_binary_operator ~inline_rhs:false operator;
                         right_printed;
                       ])
                | _ ->
                  Doc.concat
                    [
                      left_printed;
                      print_binary_operator ~inline_rhs:false operator;
                      right_printed;
                    ]
            in

            let doc =
              if (not is_lhs) && Parens.rhs_binary_expr_operand operator expr
              then Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
            in
            print_comments doc cmt_tbl expr.pexp_loc
          else
            let printeable_attrs, internal_attrs =
              ParsetreeViewer.partition_printable_attributes
                expr.pexp_attributes
            in
            let doc =
              print_expression_with_comments ~state
                {expr with pexp_attributes = internal_attrs}
                cmt_tbl
            in
            let doc =
              if
                Parens.sub_binary_expr_operand parent_operator operator
                || printeable_attrs <> []
                   && (ParsetreeViewer.is_binary_expression expr
                      || ParsetreeViewer.is_ternary_expr expr)
              then Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
            in
            Doc.concat [print_attributes ~state printeable_attrs cmt_tbl; doc]
        | _ -> assert false
      else
        match expr.pexp_desc with
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "++"; loc}};
              args = [(Nolabel, _); (Nolabel, _)];
            }
          when loc.loc_ghost ->
          let doc = print_template_literal ~state expr cmt_tbl in
          print_comments doc cmt_tbl expr.Parsetree.pexp_loc
        | Pexp_setfield (lhs, field, rhs) ->
          let doc =
            print_set_field_expr ~state expr.pexp_attributes lhs field rhs
              expr.pexp_loc cmt_tbl
          in
          if is_lhs then add_parens doc else doc
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}};
              args = [(Nolabel, lhs); (Nolabel, rhs)];
            } ->
          let rhs_doc = print_expression_with_comments ~state rhs cmt_tbl in
          let lhs_doc = print_expression_with_comments ~state lhs cmt_tbl in
          (* TODO: unify indentation of "=" *)
          let should_indent = ParsetreeViewer.is_binary_expression rhs in
          let doc =
            Doc.group
              (Doc.concat
                 [
                   lhs_doc;
                   Doc.text " =";
                   (if should_indent then
                      Doc.group (Doc.indent (Doc.concat [Doc.line; rhs_doc]))
                    else Doc.concat [Doc.space; rhs_doc]);
                 ])
          in
          let doc =
            match expr.pexp_attributes with
            | [] -> doc
            | attrs ->
              Doc.group
                (Doc.concat [print_attributes ~state attrs cmt_tbl; doc])
          in
          if is_lhs then add_parens doc else doc
        | _ -> (
          let doc = print_expression_with_comments ~state expr cmt_tbl in
          match Parens.binary_expr_operand ~is_lhs expr with
          | Parens.Parenthesized -> add_parens doc
          | Braced braces -> print_braces doc expr braces
          | Nothing -> doc)
    in
    flatten ~is_lhs ~is_multiline expr parent_operator
  in
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct =
          {
            pexp_desc = Pexp_ident {txt = Longident.Lident (("->" | "|>") as op)};
          };
        args = [(Nolabel, lhs); (Nolabel, rhs)];
      }
    when not
           (ParsetreeViewer.is_binary_expression lhs
           || ParsetreeViewer.is_binary_expression rhs
           || print_attributes ~state expr.pexp_attributes cmt_tbl <> Doc.nil)
    ->
    let lhs_has_comment_below = has_comment_below cmt_tbl lhs.pexp_loc in
    let lhs_doc = print_operand ~is_lhs:true ~is_multiline:false lhs op in
    let rhs_doc = print_operand ~is_lhs:false ~is_multiline:false rhs op in
    Doc.group
      (Doc.concat
         [
           print_attributes ~state expr.pexp_attributes cmt_tbl;
           lhs_doc;
           (match (lhs_has_comment_below, op) with
           | true, "->" -> Doc.concat [Doc.soft_line; Doc.text "->"]
           | false, "->" -> Doc.text "->"
           | true, "|>" -> Doc.concat [Doc.line; Doc.text "|> "]
           | false, "|>" -> Doc.text " |> "
           | _ -> Doc.nil);
           rhs_doc;
         ])
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident operator}};
        args = [(Nolabel, lhs); (Nolabel, rhs)];
      } ->
    let is_multiline =
      lhs.pexp_loc.loc_start.pos_lnum < rhs.pexp_loc.loc_start.pos_lnum
    in

    let right =
      let operator_with_rhs =
        let rhs_doc =
          print_operand
            ~is_lhs:(ParsetreeViewer.is_rhs_binary_operator operator)
            ~is_multiline rhs operator
        in
        Doc.concat
          [
            print_binary_operator
              ~inline_rhs:(ParsetreeViewer.should_inline_rhs_binary_expr rhs)
              operator;
            rhs_doc;
          ]
      in
      if ParsetreeViewer.should_indent_binary_expr expr then
        Doc.group (Doc.indent operator_with_rhs)
      else operator_with_rhs
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             print_operand
               ~is_lhs:(not @@ ParsetreeViewer.is_rhs_binary_operator operator)
               ~is_multiline lhs operator;
             right;
           ])
    in
    Doc.group
      (Doc.concat
         [
           print_attributes ~state expr.pexp_attributes cmt_tbl;
           (match
              Parens.binary_expr
                {
                  expr with
                  pexp_attributes =
                    ParsetreeViewer.filter_printable_attributes
                      expr.pexp_attributes;
                }
            with
           | Braced braces_loc -> print_braces doc expr braces_loc
           | Parenthesized -> add_parens doc
           | Nothing -> doc);
         ])
  | _ -> Doc.nil

and print_belt_array_concat_apply ~state sub_lists cmt_tbl =
  let make_spread_doc comma_before_spread = function
    | Some expr ->
      Doc.concat
        [
          comma_before_spread;
          Doc.dotdotdot;
          (let doc = print_expression_with_comments ~state expr cmt_tbl in
           match Parens.expr expr with
           | Parens.Parenthesized -> add_parens doc
           | Braced braces -> print_braces doc expr braces
           | Nothing -> doc);
        ]
    | None -> Doc.nil
  in
  let make_sub_list_doc (expressions, spread) =
    let comma_before_spread =
      match expressions with
      | [] -> Doc.nil
      | _ -> Doc.concat [Doc.text ","; Doc.line]
    in
    let spread_doc = make_spread_doc comma_before_spread spread in
    Doc.concat
      [
        Doc.join
          ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map
             (fun expr ->
               let doc = print_expression_with_comments ~state expr cmt_tbl in
               match Parens.expr expr with
               | Parens.Parenthesized -> add_parens doc
               | Braced braces -> print_braces doc expr braces
               | Nothing -> doc)
             expressions);
        spread_doc;
      ]
  in
  Doc.group
    (Doc.concat
       [
         Doc.lbracket;
         Doc.indent
           (Doc.concat
              [
                Doc.soft_line;
                Doc.join
                  ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map make_sub_list_doc
                     (List.map ParsetreeViewer.collect_array_expressions
                        sub_lists));
              ]);
         Doc.trailing_comma;
         Doc.soft_line;
         Doc.rbracket;
       ])

and print_belt_list_concat_apply ~state sub_lists cmt_tbl =
  let make_spread_doc comma_before_spread = function
    | Some expr ->
      Doc.concat
        [
          comma_before_spread;
          Doc.dotdotdot;
          (let doc = print_expression_with_comments ~state expr cmt_tbl in
           match Parens.expr expr with
           | Parens.Parenthesized -> add_parens doc
           | Braced braces -> print_braces doc expr braces
           | Nothing -> doc);
        ]
    | None -> Doc.nil
  in
  let make_sub_list_doc (expressions, spread) =
    let comma_before_spread =
      match expressions with
      | [] -> Doc.nil
      | _ -> Doc.concat [Doc.text ","; Doc.line]
    in
    let spread_doc = make_spread_doc comma_before_spread spread in
    Doc.concat
      [
        Doc.join
          ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map
             (fun expr ->
               let doc = print_expression_with_comments ~state expr cmt_tbl in
               match Parens.expr expr with
               | Parens.Parenthesized -> add_parens doc
               | Braced braces -> print_braces doc expr braces
               | Nothing -> doc)
             expressions);
        spread_doc;
      ]
  in
  Doc.group
    (Doc.concat
       [
         Doc.text "list{";
         Doc.indent
           (Doc.concat
              [
                Doc.soft_line;
                Doc.join
                  ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map make_sub_list_doc
                     (List.map ParsetreeViewer.collect_list_expressions
                        sub_lists));
              ]);
         Doc.trailing_comma;
         Doc.soft_line;
         Doc.rbrace;
       ])

(* callExpr(arg1, arg2) *)
and print_pexp_apply ~state expr cmt_tbl =
  match expr.pexp_desc with
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}};
        args = [(Nolabel, parent_expr); (Nolabel, member_expr)];
      } ->
    let parent_doc =
      let doc = print_expression_with_comments ~state parent_expr cmt_tbl in
      match Parens.unary_expr_operand parent_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc parent_expr braces
      | Nothing -> doc
    in
    let member =
      let member_doc =
        match member_expr.pexp_desc with
        | Pexp_ident lident ->
          print_comments
            (print_longident lident.txt)
            cmt_tbl member_expr.pexp_loc
        | _ -> print_expression_with_comments ~state member_expr cmt_tbl
      in
      Doc.concat [Doc.text "\""; member_doc; Doc.text "\""]
    in
    Doc.group
      (Doc.concat
         [
           print_attributes ~state expr.pexp_attributes cmt_tbl;
           parent_doc;
           Doc.lbracket;
           member;
           Doc.rbracket;
         ])
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}};
        args = [(Nolabel, lhs); (Nolabel, rhs)];
      } -> (
    let rhs_doc =
      let doc = print_expression_with_comments ~state rhs cmt_tbl in
      match Parens.expr rhs with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc rhs braces
      | Nothing -> doc
    in
    (* TODO: unify indentation of "=" *)
    let should_indent =
      (not (ParsetreeViewer.is_braced_expr rhs))
      && ParsetreeViewer.is_binary_expression rhs
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             print_expression_with_comments ~state lhs cmt_tbl;
             Doc.text " =";
             (if should_indent then
                Doc.group (Doc.indent (Doc.concat [Doc.line; rhs_doc]))
              else Doc.concat [Doc.space; rhs_doc]);
           ])
    in
    match expr.pexp_attributes with
    | [] -> doc
    | attrs ->
      Doc.group (Doc.concat [print_attributes ~state attrs cmt_tbl; doc]))
  | Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident
                {txt = Longident.Ldot (Lident "Primitive_dict", "make")};
          };
        args = [(Nolabel, key_values)];
      }
    when Res_parsetree_viewer.is_tuple_array key_values ->
    Doc.concat
      [
        Doc.text "dict{";
        print_literal_dict_expr ~state key_values cmt_tbl;
        Doc.rbrace;
      ]
  | Pexp_apply
      {
        funct =
          {
            pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")};
          };
        args = [(Nolabel, parent_expr); (Nolabel, member_expr)];
      }
    when not (ParsetreeViewer.is_rewritten_underscore_apply_sugar parent_expr)
    ->
    (* Don't print the Array.get(_, 0) sugar a.k.a. (__x) => Array.get(__x, 0) as _[0] *)
    let member =
      let member_doc =
        let doc = print_expression_with_comments ~state member_expr cmt_tbl in
        match Parens.expr member_expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc member_expr braces
        | Nothing -> doc
      in
      let should_inline =
        match member_expr.pexp_desc with
        | Pexp_constant _ | Pexp_ident _ -> true
        | _ -> false
      in
      if should_inline then member_doc
      else
        Doc.concat
          [Doc.indent (Doc.concat [Doc.soft_line; member_doc]); Doc.soft_line]
    in
    let parent_doc =
      let doc = print_expression_with_comments ~state parent_expr cmt_tbl in
      match Parens.unary_expr_operand parent_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc parent_expr braces
      | Nothing -> doc
    in
    Doc.group
      (Doc.concat
         [
           print_attributes ~state expr.pexp_attributes cmt_tbl;
           parent_doc;
           Doc.lbracket;
           member;
           Doc.rbracket;
         ])
  | Pexp_apply
      {
        funct =
          {
            pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "set")};
          };
        args =
          [
            (Nolabel, parent_expr);
            (Nolabel, member_expr);
            (Nolabel, target_expr);
          ];
      } ->
    let member =
      let member_doc =
        let doc = print_expression_with_comments ~state member_expr cmt_tbl in
        match Parens.expr member_expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc member_expr braces
        | Nothing -> doc
      in
      let should_inline =
        match member_expr.pexp_desc with
        | Pexp_constant _ | Pexp_ident _ -> true
        | _ -> false
      in
      if should_inline then member_doc
      else
        Doc.concat
          [Doc.indent (Doc.concat [Doc.soft_line; member_doc]); Doc.soft_line]
    in
    let should_indent_target_expr =
      if ParsetreeViewer.is_braced_expr target_expr then false
      else
        ParsetreeViewer.is_binary_expression target_expr
        ||
        match target_expr with
        | {
         pexp_attributes = [({Location.txt = "res.ternary"}, _)];
         pexp_desc = Pexp_ifthenelse (if_expr, _, _);
        } ->
          ParsetreeViewer.is_binary_expression if_expr
          || ParsetreeViewer.has_attributes if_expr.pexp_attributes
        | {pexp_desc = Pexp_newtype _} -> false
        | e ->
          ParsetreeViewer.has_attributes e.pexp_attributes
          || ParsetreeViewer.is_array_access e
    in
    let target_expr =
      let doc = print_expression_with_comments ~state target_expr cmt_tbl in
      match Parens.expr target_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc target_expr braces
      | Nothing -> doc
    in
    let parent_doc =
      let doc = print_expression_with_comments ~state parent_expr cmt_tbl in
      match Parens.unary_expr_operand parent_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc parent_expr braces
      | Nothing -> doc
    in
    Doc.group
      (Doc.concat
         [
           print_attributes ~state expr.pexp_attributes cmt_tbl;
           parent_doc;
           Doc.lbracket;
           member;
           Doc.rbracket;
           Doc.text " =";
           (if should_indent_target_expr then
              Doc.indent (Doc.concat [Doc.line; target_expr])
            else Doc.concat [Doc.space; target_expr]);
         ])
  (* TODO: cleanup, are those branches even remotely performant? *)
  | Pexp_apply {funct = {pexp_desc = Pexp_ident lident}; args}
    when ParsetreeViewer.is_jsx_expression expr ->
    print_jsx_expression ~state lident args cmt_tbl
  | Pexp_apply {funct = call_expr; args; partial} ->
    let args =
      List.map
        (fun (lbl, arg) -> (lbl, ParsetreeViewer.rewrite_underscore_apply arg))
        args
    in
    let attrs = expr.pexp_attributes in
    let args =
      if partial then
        let dummy = Ast_helper.Exp.constant ~attrs (Ast_helper.Const.int 0) in
        args @ [(Asttypes.Labelled {txt = "..."; loc = Location.none}, dummy)]
      else args
    in
    let call_expr_doc =
      let doc = print_expression_with_comments ~state call_expr cmt_tbl in
      match Parens.call_expr call_expr with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc call_expr braces
      | Nothing -> doc
    in
    if ParsetreeViewer.requires_special_callback_printing_first_arg args then
      let args_doc =
        print_arguments_with_callback_in_first_position ~state ~partial args
          cmt_tbl
      in
      Doc.concat
        [print_attributes ~state attrs cmt_tbl; call_expr_doc; args_doc]
    else if ParsetreeViewer.requires_special_callback_printing_last_arg args
    then
      let args_doc =
        print_arguments_with_callback_in_last_position ~state ~partial args
          cmt_tbl
      in
      (*
       * Fixes the following layout (the `[` and `]` should break):
       *   [fn(x => {
       *     let _ = x
       *   }), fn(y => {
       *     let _ = y
       *   }), fn(z => {
       *     let _ = z
       *   })]
       * See `Doc.willBreak documentation in interface file for more context.
       * Context:
       *  https://github.com/rescript-lang/syntax/issues/111
       *  https://github.com/rescript-lang/syntax/issues/166
       *)
      let maybe_break_parent =
        if Doc.will_break args_doc then Doc.break_parent else Doc.nil
      in
      Doc.concat
        [
          maybe_break_parent;
          print_attributes ~state attrs cmt_tbl;
          call_expr_doc;
          args_doc;
        ]
    else
      let args_doc = print_arguments ~state ~partial args cmt_tbl in
      Doc.concat
        [print_attributes ~state attrs cmt_tbl; call_expr_doc; args_doc]
  | _ -> assert false

and print_jsx_expression ~state lident args cmt_tbl =
  let name = print_jsx_name lident in
  let formatted_props, children = print_jsx_props ~state args cmt_tbl in
  (* <div className="test" /> *)
  let has_children =
    match children with
    | Some
        {
          Parsetree.pexp_desc =
            Pexp_construct ({txt = Longident.Lident "[]"}, None);
        } ->
      false
    | None -> false
    | _ -> true
  in
  let is_self_closing =
    match children with
    | Some
        {
          Parsetree.pexp_desc =
            Pexp_construct ({txt = Longident.Lident "[]"}, None);
          pexp_loc = loc;
        } ->
      not (has_comments_inside cmt_tbl loc)
    | _ -> false
  in
  let print_children children =
    let line_sep =
      match children with
      | Some expr ->
        if has_nested_jsx_or_more_than_one_child expr then Doc.hard_line
        else Doc.line
      | None -> Doc.line
    in
    Doc.concat
      [
        Doc.indent
          (Doc.concat
             [
               Doc.line;
               (match children with
               | Some children_expression ->
                 print_jsx_children ~state children_expression ~sep:line_sep
                   cmt_tbl
               | None -> Doc.nil);
             ]);
        line_sep;
      ]
  in
  Doc.group
    (Doc.concat
       [
         Doc.group
           (Doc.concat
              [
                print_comments
                  (Doc.concat [Doc.less_than; name])
                  cmt_tbl lident.Asttypes.loc;
                formatted_props;
                (match children with
                | Some
                    {
                      Parsetree.pexp_desc =
                        Pexp_construct ({txt = Longident.Lident "[]"}, None);
                    }
                  when is_self_closing ->
                  Doc.text "/>"
                | _ ->
                  (* if tag A has trailing comments then put > on the next line
                     <A
                     // comments
                     >
                     </A>
                  *)
                  if has_trailing_comments cmt_tbl lident.Asttypes.loc then
                    Doc.concat [Doc.soft_line; Doc.greater_than]
                  else Doc.greater_than);
              ]);
         (if is_self_closing then Doc.nil
          else
            Doc.concat
              [
                (if has_children then print_children children
                 else
                   match children with
                   | Some
                       {
                         Parsetree.pexp_desc =
                           Pexp_construct ({txt = Longident.Lident "[]"}, None);
                         pexp_loc = loc;
                       } ->
                     print_comments_inside cmt_tbl loc
                   | _ -> Doc.nil);
                Doc.text "</";
                name;
                Doc.greater_than;
              ]);
       ])

and print_jsx_fragment ~state expr cmt_tbl =
  let opening = Doc.text "<>" in
  let closing = Doc.text "</>" in
  let line_sep =
    if has_nested_jsx_or_more_than_one_child expr then Doc.hard_line
    else Doc.line
  in
  Doc.group
    (Doc.concat
       [
         opening;
         (match expr.pexp_desc with
         | Pexp_construct ({txt = Longident.Lident "[]"}, None) -> Doc.nil
         | _ ->
           Doc.indent
             (Doc.concat
                [Doc.line; print_jsx_children ~state expr ~sep:line_sep cmt_tbl]));
         line_sep;
         closing;
       ])

and print_jsx_children ~state (children_expr : Parsetree.expression) ~sep
    cmt_tbl =
  match children_expr.pexp_desc with
  | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
    let children, _ = ParsetreeViewer.collect_list_expressions children_expr in
    let print_expr (expr : Parsetree.expression) =
      let leading_line_comment_present =
        has_leading_line_comment cmt_tbl expr.pexp_loc
      in
      let expr_doc = print_expression_with_comments ~state expr cmt_tbl in
      let add_parens_or_braces expr_doc =
        (* {(20: int)} make sure that we also protect the expression inside *)
        let inner_doc =
          if Parens.braced_expr expr then add_parens expr_doc else expr_doc
        in
        if leading_line_comment_present then add_braces inner_doc
        else Doc.concat [Doc.lbrace; inner_doc; Doc.rbrace]
      in
      match Parens.jsx_child_expr expr with
      | Nothing -> expr_doc
      | Parenthesized -> add_parens_or_braces expr_doc
      | Braced braces_loc ->
        print_comments (add_parens_or_braces expr_doc) cmt_tbl braces_loc
    in
    let get_first_leading_comment loc =
      match get_first_leading_comment cmt_tbl loc with
      | None -> loc
      | Some comment -> Comment.loc comment
    in
    let get_loc expr =
      match ParsetreeViewer.process_braces_attr expr with
      | None, _ -> get_first_leading_comment expr.pexp_loc
      | Some ({loc}, _), _ -> get_first_leading_comment loc
    in
    let rec loop prev acc exprs =
      match exprs with
      | [] -> List.rev acc
      | expr :: tails ->
        let start_loc = (get_loc expr).loc_start.pos_lnum in
        let end_loc = (get_loc prev).loc_end.pos_lnum in
        let expr_doc = print_expr expr in
        let docs =
          if start_loc - end_loc > 1 then
            Doc.concat [Doc.hard_line; expr_doc] :: acc
          else expr_doc :: acc
        in
        loop expr docs tails
    in
    let docs = loop children_expr [] children in
    Doc.group (Doc.join ~sep docs)
  | _ ->
    let leading_line_comment_present =
      has_leading_line_comment cmt_tbl children_expr.pexp_loc
    in
    let expr_doc =
      print_expression_with_comments ~state children_expr cmt_tbl
    in
    Doc.concat
      [
        Doc.dotdotdot;
        (match Parens.jsx_child_expr children_expr with
        | Parenthesized | Braced _ ->
          let inner_doc =
            if Parens.braced_expr children_expr then add_parens expr_doc
            else expr_doc
          in
          if leading_line_comment_present then add_braces inner_doc
          else Doc.concat [Doc.lbrace; inner_doc; Doc.rbrace]
        | Nothing -> expr_doc);
      ]

and print_jsx_props ~state args cmt_tbl : Doc.t * Parsetree.expression option =
  (* This function was introduced because we have different formatting behavior for self-closing tags and other tags
     we always put /> on a new line for self-closing tag when it breaks
     <A
      a=""
     />

     <A
     a="">
      <B />
     </A>
     we should remove this function once the format is unified
  *)
  let is_self_closing children =
    match children with
    | {
     Parsetree.pexp_desc = Pexp_construct ({txt = Longident.Lident "[]"}, None);
     pexp_loc = loc;
    } ->
      not (has_comments_inside cmt_tbl loc)
    | _ -> false
  in
  let rec loop props args =
    match args with
    | [] -> (Doc.nil, None)
    | [
     (Asttypes.Labelled {txt = "children"}, children);
     ( Asttypes.Nolabel,
       {
         Parsetree.pexp_desc =
           Pexp_construct ({txt = Longident.Lident "()"}, None);
       } );
    ] ->
      let doc = if is_self_closing children then Doc.line else Doc.nil in
      (doc, Some children)
    | ((e_lbl, expr) as last_prop)
      :: [
           (Asttypes.Labelled {txt = "children"}, children);
           ( Asttypes.Nolabel,
             {
               Parsetree.pexp_desc =
                 Pexp_construct ({txt = Longident.Lident "()"}, None);
             } );
         ] ->
      let loc =
        match e_lbl with
        | Asttypes.Labelled {loc} | Asttypes.Optional {loc} ->
          {loc with loc_end = expr.pexp_loc.loc_end}
        | Nolabel -> expr.pexp_loc
      in
      let trailing_comments_present = has_trailing_comments cmt_tbl loc in
      let prop_doc = print_jsx_prop ~state last_prop cmt_tbl in
      let formatted_props =
        Doc.concat
          [
            Doc.indent
              (Doc.concat
                 [
                   Doc.line;
                   Doc.group
                     (Doc.join ~sep:Doc.line (prop_doc :: props |> List.rev));
                 ]);
            (* print > on new line if the last prop has trailing comments *)
            (match (is_self_closing children, trailing_comments_present) with
            (* we always put /> on a new line when a self-closing tag breaks *)
            | true, _ -> Doc.line
            | false, true -> Doc.soft_line
            | false, false -> Doc.nil);
          ]
      in
      (formatted_props, Some children)
    | arg :: args ->
      let prop_doc = print_jsx_prop ~state arg cmt_tbl in
      loop (prop_doc :: props) args
  in
  loop [] args

and print_jsx_prop ~state arg cmt_tbl =
  match arg with
  | ( ((Asttypes.Labelled {txt = lbl_txt} | Optional {txt = lbl_txt}) as lbl),
      {
        pexp_attributes = [];
        pexp_desc = Pexp_ident {txt = Longident.Lident ident};
      } )
    when lbl_txt = ident (* jsx punning *) -> (
    match lbl with
    | Nolabel -> Doc.nil
    | Labelled {loc} -> print_comments (print_ident_like ident) cmt_tbl loc
    | Optional {loc} ->
      let doc = Doc.concat [Doc.question; print_ident_like ident] in
      print_comments doc cmt_tbl loc)
  | ( ((Asttypes.Labelled {txt = lbl_txt} | Optional {txt = lbl_txt}) as lbl),
      {
        Parsetree.pexp_attributes = [];
        pexp_desc = Pexp_ident {txt = Longident.Lident ident};
      } )
    when lbl_txt = ident (* jsx punning when printing from Reason *) -> (
    match lbl with
    | Nolabel -> Doc.nil
    | Labelled _lbl -> print_ident_like ident
    | Optional _lbl -> Doc.concat [Doc.question; print_ident_like ident])
  | Asttypes.Labelled {txt = "_spreadProps"}, expr ->
    let doc = print_expression_with_comments ~state expr cmt_tbl in
    Doc.concat [Doc.lbrace; Doc.dotdotdot; doc; Doc.rbrace]
  | lbl, expr ->
    let arg_loc, lbl_doc =
      match lbl with
      | Asttypes.Labelled {txt = lbl; loc} ->
        let lbl = print_comments (print_ident_like lbl) cmt_tbl loc in
        (loc, Doc.concat [lbl; Doc.equal])
      | Asttypes.Optional {txt = lbl; loc} ->
        let lbl = print_comments (print_ident_like lbl) cmt_tbl loc in
        (loc, Doc.concat [lbl; Doc.equal; Doc.question])
      | Nolabel -> (Location.none, Doc.nil)
    in
    let expr_doc =
      let leading_line_comment_present =
        has_leading_line_comment cmt_tbl expr.pexp_loc
      in
      let doc = print_expression_with_comments ~state expr cmt_tbl in
      match Parens.jsx_prop_expr expr with
      | Parenthesized | Braced _ ->
        (* {(20: int)} make sure that we also protect the expression inside *)
        let inner_doc =
          if Parens.braced_expr expr then add_parens doc else doc
        in
        if leading_line_comment_present then add_braces inner_doc
        else Doc.concat [Doc.lbrace; inner_doc; Doc.rbrace]
      | _ -> doc
    in
    let full_loc = {arg_loc with loc_end = expr.pexp_loc.loc_end} in
    print_comments (Doc.concat [lbl_doc; expr_doc]) cmt_tbl full_loc

(* div -> div.
 * Navabar.createElement -> Navbar
 * Staff.Users.createElement -> Staff.Users *)
and print_jsx_name {txt = lident} =
  let print_ident = print_ident_like ~allow_uident:true ~allow_hyphen:true in
  let rec flatten acc lident =
    match lident with
    | Longident.Lident txt -> print_ident txt :: acc
    | Ldot (lident, "createElement") -> flatten acc lident
    | Ldot (lident, txt) -> flatten (print_ident txt :: acc) lident
    | _ -> acc
  in
  match lident with
  | Longident.Lident txt -> print_ident txt
  | _ as lident ->
    let segments = flatten [] lident in
    Doc.join ~sep:Doc.dot segments

and print_arguments_with_callback_in_first_position ~state ~partial args cmt_tbl
    =
  (* Because the same subtree gets printed twice, we need to copy the cmt_tbl.
   * consumed comments need to be marked not-consumed and reprinted…
   * Cheng's different comment algorithm will solve this. *)
  let state = State.next_custom_layout state in
  let cmt_tbl_copy = CommentTable.copy cmt_tbl in
  let callback, printed_args =
    match args with
    | (lbl, expr) :: args ->
      let lbl_doc =
        match lbl with
        | Asttypes.Nolabel -> Doc.nil
        | Asttypes.Labelled {txt} ->
          Doc.concat [Doc.tilde; print_ident_like txt; Doc.equal]
        | Asttypes.Optional {txt} ->
          Doc.concat [Doc.tilde; print_ident_like txt; Doc.equal; Doc.question]
      in
      let callback =
        Doc.concat
          [
            lbl_doc;
            print_pexp_fun ~state ~in_callback:FitsOnOneLine expr cmt_tbl;
          ]
      in
      let callback = lazy (print_comments callback cmt_tbl expr.pexp_loc) in
      let printed_args =
        lazy
          (Doc.join
             ~sep:(Doc.concat [Doc.comma; Doc.line])
             (List.map (fun arg -> print_argument ~state arg cmt_tbl) args))
      in
      (callback, printed_args)
    | _ -> assert false
  in

  (* Thing.map((arg1, arg2) => MyModuleBlah.toList(argument), foo) *)
  (* Thing.map((arg1, arg2) => {
   *   MyModuleBlah.toList(argument)
   * }, longArgumet, veryLooooongArgument)
   *)
  let fits_on_one_line =
    lazy
      (Doc.concat
         [
           Doc.lparen;
           Lazy.force callback;
           Doc.comma;
           Doc.line;
           Lazy.force printed_args;
           Doc.rparen;
         ])
  in

  (* Thing.map(
   *   (param1, parm2) => doStuff(param1, parm2),
   *   arg1,
   *   arg2,
   *   arg3,
   * )
   *)
  let break_all_args =
    lazy (print_arguments ~state ~partial args cmt_tbl_copy)
  in

  (* Sometimes one of the non-callback arguments will break.
   * There might be a single line comment in there, or a multiline string etc.
   * showDialog(
   *   ~onConfirm={() => ()},
   *   `
   *   Do you really want to leave this workspace?
   *   Some more text with detailed explanations...
   *   `,
   *   ~danger=true,
   *   // comment   --> here a single line comment
   *   ~confirmText="Yes, I am sure!",
   *  )
   * In this case, we always want the arguments broken over multiple lines,
   * like a normal function call.
   *)
  if state |> State.should_break_callback then Lazy.force break_all_args
  else if Doc.will_break (Lazy.force printed_args) then
    Lazy.force break_all_args
  else
    Doc.custom_layout [Lazy.force fits_on_one_line; Lazy.force break_all_args]

and print_arguments_with_callback_in_last_position ~state ~partial args cmt_tbl
    =
  (* Because the same subtree gets printed twice, we need to copy the cmt_tbl.
   * consumed comments need to be marked not-consumed and reprinted…
   * Cheng's different comment algorithm will solve this. *)
  let state = state |> State.next_custom_layout in
  let cmt_tbl_copy = CommentTable.copy cmt_tbl in
  let cmt_tbl_copy2 = CommentTable.copy cmt_tbl in
  let rec loop acc args =
    match args with
    | [] -> (lazy Doc.nil, lazy Doc.nil, lazy Doc.nil)
    | [(lbl, expr)] ->
      let lbl_doc =
        match lbl with
        | Asttypes.Nolabel -> Doc.nil
        | Asttypes.Labelled {txt} ->
          Doc.concat [Doc.tilde; print_ident_like txt; Doc.equal]
        | Asttypes.Optional {txt} ->
          Doc.concat [Doc.tilde; print_ident_like txt; Doc.equal; Doc.question]
      in
      let callback_fits_on_one_line =
        lazy
          (let pexp_fun_doc =
             print_pexp_fun ~state ~in_callback:FitsOnOneLine expr cmt_tbl
           in
           let doc = Doc.concat [lbl_doc; pexp_fun_doc] in
           print_comments doc cmt_tbl expr.pexp_loc)
      in
      let callback_arguments_fits_on_one_line =
        lazy
          (let pexp_fun_doc =
             print_pexp_fun ~state ~in_callback:ArgumentsFitOnOneLine expr
               cmt_tbl_copy
           in
           let doc = Doc.concat [lbl_doc; pexp_fun_doc] in
           print_comments doc cmt_tbl_copy expr.pexp_loc)
      in
      ( lazy (Doc.concat (List.rev acc)),
        callback_fits_on_one_line,
        callback_arguments_fits_on_one_line )
    | arg :: args ->
      let arg_doc = print_argument ~state arg cmt_tbl in
      loop (Doc.line :: Doc.comma :: arg_doc :: acc) args
  in
  let printed_args, callback, callback2 = loop [] args in

  (* Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument)) *)
  let fits_on_one_line =
    lazy
      (Doc.concat
         [Doc.lparen; Lazy.force printed_args; Lazy.force callback; Doc.rparen])
  in

  (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
   *   MyModuleBlah.toList(argument)
   * )
   *)
  let arugments_fit_on_one_line =
    lazy
      (Doc.concat
         [
           Doc.lparen;
           Lazy.force printed_args;
           Doc.breakable_group ~force_break:true (Lazy.force callback2);
           Doc.rparen;
         ])
  in

  (* Thing.map(
   *   arg1,
   *   arg2,
   *   arg3,
   *   (param1, parm2) => doStuff(param1, parm2)
   * )
   *)
  let break_all_args =
    lazy (print_arguments ~state ~partial args cmt_tbl_copy2)
  in

  (* Sometimes one of the non-callback arguments will break.
   * There might be a single line comment in there, or a multiline string etc.
   * showDialog(
   *   `
   *   Do you really want to leave this workspace?
   *   Some more text with detailed explanations...
   *   `,
   *   ~danger=true,
   *   // comment   --> here a single line comment
   *   ~confirmText="Yes, I am sure!",
   *   ~onConfirm={() => ()},
   *  )
   * In this case, we always want the arguments broken over multiple lines,
   * like a normal function call.
   *)
  if state |> State.should_break_callback then Lazy.force break_all_args
  else if Doc.will_break (Lazy.force printed_args) then
    Lazy.force break_all_args
  else
    Doc.custom_layout
      [
        Lazy.force fits_on_one_line;
        Lazy.force arugments_fit_on_one_line;
        Lazy.force break_all_args;
      ]

and print_arguments ~state ~partial
    (args : (Asttypes.arg_label * Parsetree.expression) list) cmt_tbl =
  match args with
  | [
   ( Nolabel,
     {
       pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _);
       pexp_loc = loc;
     } );
  ] ->
    if has_leading_line_comment cmt_tbl loc then
      let cmt = print_comments Doc.nil cmt_tbl loc in
      Doc.concat
        [
          Doc.lparen;
          Doc.indent (Doc.group (Doc.concat [Doc.soft_line; cmt]));
          Doc.rparen;
        ]
    else Doc.text "()"
  | [(Nolabel, arg)] when ParsetreeViewer.is_huggable_expression arg ->
    let arg_doc =
      let doc = print_expression_with_comments ~state arg cmt_tbl in
      match Parens.expr arg with
      | Parens.Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc arg braces
      | Nothing -> doc
    in
    Doc.concat [Doc.lparen; arg_doc; Doc.rparen]
  | args ->
    (* Avoid printing trailing comma when there is ... in function application *)
    let printed_args =
      List.map (fun arg -> print_argument ~state arg cmt_tbl) args
    in
    Doc.group
      (Doc.concat
         [
           Doc.lparen;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) printed_args;
                ]);
           (if partial then Doc.nil else Doc.trailing_comma);
           Doc.soft_line;
           Doc.rparen;
         ])

(*
 * argument ::=
 *   | _                            (* syntax sugar *)
 *   | expr
 *   | expr : type
 *   | ~ label-name
 *   | ~ label-name
 *   | ~ label-name ?
 *   | ~ label-name =   expr
 *   | ~ label-name =   _           (* syntax sugar *)
 *   | ~ label-name =   expr : type
 *   | ~ label-name = ? expr
 *   | ~ label-name = ? _           (* syntax sugar *)
 *   | ~ label-name = ? expr : type *)
and print_argument ~state (arg_lbl, arg) cmt_tbl =
  match (arg_lbl, arg) with
  (* ~a (punned)*)
  | ( Labelled {txt = lbl; loc = l0},
      {
        pexp_attributes = [];
        pexp_desc = Pexp_ident {txt = Longident.Lident name};
      } )
    when lbl = name && not (ParsetreeViewer.is_braced_expr arg) ->
    let loc = {l0 with loc_end = arg.pexp_loc.loc_end} in
    let doc = Doc.concat [Doc.tilde; print_ident_like lbl] in
    print_comments doc cmt_tbl loc
  (* ~a: int (punned)*)
  | ( Labelled {txt = lbl; loc = l0},
      {
        pexp_desc =
          Pexp_constraint
            ( ({pexp_desc = Pexp_ident {txt = Longident.Lident name}} as arg_expr),
              typ );
        pexp_attributes = [];
      } )
    when lbl = name && not (ParsetreeViewer.is_braced_expr arg_expr) ->
    let loc = {l0 with loc_end = arg.pexp_loc.loc_end} in
    let doc =
      Doc.concat
        [
          Doc.tilde;
          print_ident_like lbl;
          Doc.text ": ";
          print_typ_expr ~state typ cmt_tbl;
        ]
    in
    print_comments doc cmt_tbl loc
  (* ~a? (optional lbl punned)*)
  | ( Optional {txt = lbl; loc},
      {
        pexp_desc = Pexp_ident {txt = Longident.Lident name};
        pexp_attributes = [];
      } )
    when lbl = name ->
    let doc = Doc.concat [Doc.tilde; print_ident_like lbl; Doc.question] in
    print_comments doc cmt_tbl loc
  | _lbl, expr ->
    let arg_loc, printed_lbl, dotdotdot =
      match arg_lbl with
      | Nolabel -> (expr.pexp_loc, Doc.nil, false)
      | Labelled {txt = "..."; loc} ->
        let arg_loc = loc in
        let doc = Doc.text "..." in
        (loc, print_comments doc cmt_tbl arg_loc, true)
      | Labelled {txt = lbl; loc} ->
        let arg_loc = loc in
        let doc = Doc.concat [Doc.tilde; print_ident_like lbl; Doc.equal] in
        (loc, print_comments doc cmt_tbl arg_loc, false)
      | Optional {txt = lbl; loc} ->
        let arg_loc = loc in
        let doc =
          Doc.concat [Doc.tilde; print_ident_like lbl; Doc.equal; Doc.question]
        in
        (loc, print_comments doc cmt_tbl arg_loc, false)
    in
    let printed_expr =
      let doc = print_expression_with_comments ~state expr cmt_tbl in
      match Parens.expr expr with
      | Parenthesized -> add_parens doc
      | Braced braces -> print_braces doc expr braces
      | Nothing -> doc
    in
    let loc = {arg_loc with loc_end = expr.pexp_loc.loc_end} in
    let doc =
      if dotdotdot then printed_lbl else Doc.concat [printed_lbl; printed_expr]
    in
    print_comments doc cmt_tbl loc

and print_cases ~state (cases : Parsetree.case list) cmt_tbl =
  Doc.breakable_group ~force_break:true
    (Doc.concat
       [
         Doc.lbrace;
         Doc.concat
           [
             Doc.line;
             print_list
               ~get_loc:(fun n ->
                 {
                   n.Parsetree.pc_lhs.ppat_loc with
                   loc_end =
                     (match ParsetreeViewer.process_braces_attr n.pc_rhs with
                     | None, _ -> n.pc_rhs.pexp_loc.loc_end
                     | Some ({loc}, _), _ -> loc.Location.loc_end);
                 })
               ~print:(print_case ~state) ~nodes:cases cmt_tbl;
           ];
         Doc.line;
         Doc.rbrace;
       ])

and print_case ~state (case : Parsetree.case) cmt_tbl =
  let rhs =
    match case.pc_rhs.pexp_desc with
    | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ | Pexp_open _
    | Pexp_sequence _ ->
      print_expression_block ~state
        ~braces:(ParsetreeViewer.is_braced_expr case.pc_rhs)
        case.pc_rhs cmt_tbl
    | _ -> (
      let doc = print_expression_with_comments ~state case.pc_rhs cmt_tbl in
      match Parens.expr case.pc_rhs with
      | Parenthesized -> add_parens doc
      | _ -> doc)
  in

  let guard =
    match case.pc_guard with
    | None -> Doc.nil
    | Some expr ->
      Doc.group
        (Doc.concat
           [
             Doc.line;
             Doc.text "if ";
             print_expression_with_comments ~state expr cmt_tbl;
           ])
  in
  let should_inline_rhs =
    match case.pc_rhs.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident ("()" | "true" | "false")}, _)
    | Pexp_constant _ | Pexp_ident _ ->
      true
    | _ when ParsetreeViewer.is_huggable_rhs case.pc_rhs -> true
    | _ -> false
  in
  let should_indent_pattern =
    match case.pc_lhs.ppat_desc with
    | Ppat_or _ -> false
    | _ -> true
  in
  let pattern_doc =
    let doc = print_pattern ~state case.pc_lhs cmt_tbl in
    match case.pc_lhs.ppat_desc with
    | Ppat_constraint _ -> add_parens doc
    | _ -> doc
  in
  let content =
    Doc.concat
      [
        (if should_indent_pattern then Doc.indent pattern_doc else pattern_doc);
        Doc.indent guard;
        Doc.text " =>";
        Doc.indent
          (Doc.concat
             [(if should_inline_rhs then Doc.space else Doc.line); rhs]);
      ]
  in
  Doc.group (Doc.concat [Doc.text "| "; content])

and print_expr_fun_parameters ~state ~in_callback ~async ~has_constraint
    parameters cmt_tbl =
  match parameters with
  (* let f = _ => () *)
  | [
   ParsetreeViewer.Parameter
     {
       attrs = [];
       lbl = Nolabel;
       default_expr = None;
       pat = {Parsetree.ppat_desc = Ppat_any; ppat_loc};
     };
  ] ->
    let any =
      let doc = if has_constraint then Doc.text "(_)" else Doc.text "_" in
      print_comments doc cmt_tbl ppat_loc
    in
    if async then add_async any else any
  (* let f = a => () *)
  | [
   ParsetreeViewer.Parameter
     {
       attrs = [];
       lbl = Nolabel;
       default_expr = None;
       pat =
         {
           Parsetree.ppat_desc = Ppat_var string_loc;
           Parsetree.ppat_attributes = attrs;
         };
     };
  ] ->
    let txt_doc =
      let var = print_ident_like string_loc.txt in
      let var =
        match attrs with
        | [] -> if has_constraint then add_parens var else var
        | attrs ->
          let attrs = print_attributes ~state attrs cmt_tbl in
          add_parens (Doc.concat [attrs; var])
      in
      if async then add_async var else var
    in
    print_comments txt_doc cmt_tbl string_loc.loc
  (* let f = () => () *)
  | [
   ParsetreeViewer.Parameter
     {
       attrs = [];
       lbl = Nolabel;
       default_expr = None;
       pat =
         {ppat_desc = Ppat_construct ({txt = Longident.Lident "()"; loc}, None)};
     };
  ] ->
    let doc =
      let lparen_rparen = Doc.text "()" in
      if async then add_async lparen_rparen else lparen_rparen
    in
    print_comments doc cmt_tbl loc
  (* let f = (~greeting, ~from as hometown, ~x=?) => () *)
  | parameters ->
    let in_callback =
      match in_callback with
      | FitsOnOneLine -> true
      | _ -> false
    in
    let maybe_async_lparen =
      let lparen = Doc.lparen in
      if async then add_async lparen else lparen
    in
    let should_hug = ParsetreeViewer.parameters_should_hug parameters in
    let printed_paramaters =
      Doc.concat
        [
          (if should_hug || in_callback then Doc.nil else Doc.soft_line);
          Doc.join
            ~sep:(Doc.concat [Doc.comma; Doc.line])
            (List.map
               (fun p -> print_exp_fun_parameter ~state p cmt_tbl)
               parameters);
        ]
    in
    Doc.group
      (Doc.concat
         [
           maybe_async_lparen;
           (if should_hug || in_callback then printed_paramaters
            else
              Doc.concat
                [
                  Doc.indent printed_paramaters;
                  Doc.trailing_comma;
                  Doc.soft_line;
                ]);
           Doc.rparen;
         ])

and print_exp_fun_parameter ~state parameter cmt_tbl =
  match parameter with
  | ParsetreeViewer.NewTypes {attrs; locs = lbls} ->
    Doc.group
      (Doc.concat
         [
           print_attributes ~state attrs cmt_tbl;
           Doc.text "type ";
           (* XX *)
           Doc.join ~sep:Doc.space
             (List.map
                (fun lbl ->
                  print_comments
                    (print_ident_like lbl.Asttypes.txt)
                    cmt_tbl lbl.Asttypes.loc)
                lbls);
         ])
  | Parameter {attrs; lbl; default_expr; pat = pattern} ->
    let attrs = print_attributes ~state attrs cmt_tbl in
    (* =defaultValue *)
    let default_expr_doc =
      match default_expr with
      | Some expr ->
        Doc.concat
          [Doc.text "="; print_expression_with_comments ~state expr cmt_tbl]
      | None -> Doc.nil
    in
    (* ~from as hometown
     * ~from                   ->  punning *)
    let label_with_pattern =
      match (lbl, pattern) with
      | Nolabel, pattern -> print_pattern ~state pattern cmt_tbl
      | ( (Labelled {txt = lbl} | Optional {txt = lbl}),
          {ppat_desc = Ppat_var string_loc; ppat_attributes} )
        when lbl = string_loc.txt ->
        (* ~d *)
        Doc.concat
          [
            print_attributes ~state ppat_attributes cmt_tbl;
            Doc.text "~";
            print_ident_like lbl;
          ]
      | ( (Labelled {txt = lbl} | Optional {txt = lbl}),
          {
            ppat_desc = Ppat_constraint ({ppat_desc = Ppat_var {txt}}, typ);
            ppat_attributes;
          } )
        when lbl = txt ->
        (* ~d: e *)
        Doc.concat
          [
            print_attributes ~state ppat_attributes cmt_tbl;
            Doc.text "~";
            print_ident_like lbl;
            Doc.text ": ";
            print_typ_expr ~state typ cmt_tbl;
          ]
      | (Labelled {txt = lbl} | Optional {txt = lbl}), pattern ->
        (* ~b as c *)
        Doc.concat
          [
            Doc.text "~";
            print_ident_like lbl;
            Doc.text " as ";
            print_pattern ~state pattern cmt_tbl;
          ]
    in
    let optional_label_suffix =
      match (lbl, default_expr) with
      | Optional _, None -> Doc.text "=?"
      | _ -> Doc.nil
    in
    let doc =
      Doc.group
        (Doc.concat
           [attrs; label_with_pattern; default_expr_doc; optional_label_suffix])
    in
    let lbl_loc = Asttypes.get_lbl_loc lbl in
    let cmt_loc =
      match default_expr with
      | None -> {lbl_loc with loc_end = pattern.ppat_loc.loc_end}
      | Some expr -> {lbl_loc with loc_end = expr.pexp_loc.loc_end}
    in
    print_comments doc cmt_tbl cmt_loc

and print_expression_block ~state ~braces expr cmt_tbl =
  let rec collect_rows acc expr =
    match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_letmodule (mod_name, mod_expr, expr2) ->
      let name =
        let doc = Doc.text mod_name.txt in
        print_comments doc cmt_tbl mod_name.loc
      in
      let name, mod_expr =
        match mod_expr.pmod_desc with
        | Pmod_constraint (mod_expr2, mod_type)
          when not
                 (ParsetreeViewer.has_await_attribute mod_expr.pmod_attributes)
          ->
          let name =
            Doc.concat
              [name; Doc.text ": "; print_mod_type ~state mod_type cmt_tbl]
          in
          (name, mod_expr2)
        | _ -> (name, mod_expr)
      in
      let let_module_doc =
        Doc.concat
          [
            Doc.text "module ";
            name;
            Doc.text " = ";
            print_mod_expr ~state mod_expr cmt_tbl;
          ]
      in
      let loc = {expr.pexp_loc with loc_end = mod_expr.pmod_loc.loc_end} in
      collect_rows ((loc, let_module_doc) :: acc) expr2
    | Pexp_letexception (extension_constructor, expr2) ->
      let loc =
        let loc =
          {expr.pexp_loc with loc_end = extension_constructor.pext_loc.loc_end}
        in
        match get_first_leading_comment cmt_tbl loc with
        | None -> loc
        | Some comment ->
          let cmt_loc = Comment.loc comment in
          {cmt_loc with loc_end = loc.loc_end}
      in
      let let_exception_doc =
        print_exception_def ~state extension_constructor cmt_tbl
      in
      collect_rows ((loc, let_exception_doc) :: acc) expr2
    | Pexp_open (override_flag, longident_loc, expr2) ->
      let open_doc =
        Doc.concat
          [
            Doc.text "open";
            print_override_flag override_flag;
            Doc.space;
            print_longident_location longident_loc cmt_tbl;
          ]
      in
      let loc = {expr.pexp_loc with loc_end = longident_loc.loc.loc_end} in
      collect_rows ((loc, open_doc) :: acc) expr2
    | Pexp_sequence (expr1, expr2) ->
      let expr_doc =
        let doc = print_expression ~state expr1 cmt_tbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr1 braces
        | Nothing -> doc
      in
      let loc = expr1.pexp_loc in
      collect_rows ((loc, expr_doc) :: acc) expr2
    | Pexp_let (rec_flag, value_bindings, expr2) -> (
      let loc =
        let loc =
          match (value_bindings, List.rev value_bindings) with
          | vb :: _, last_vb :: _ ->
            {vb.pvb_loc with loc_end = last_vb.pvb_loc.loc_end}
          | _ -> Location.none
        in
        match get_first_leading_comment cmt_tbl loc with
        | None -> loc
        | Some comment ->
          let cmt_loc = Comment.loc comment in
          {cmt_loc with loc_end = loc.loc_end}
      in
      let rec_flag =
        match rec_flag with
        | Asttypes.Nonrecursive -> Doc.nil
        | Asttypes.Recursive -> Doc.text "rec "
      in
      let let_doc =
        print_value_bindings ~state ~rec_flag value_bindings cmt_tbl
      in
      (* let () = {
       *   let () = foo()
       *   ()
       * }
       * We don't need to print the () on the last line of the block
       *)
      match expr2.pexp_desc with
      | Pexp_construct ({txt = Longident.Lident "()"}, _) ->
        List.rev ((loc, let_doc) :: acc)
      | _ -> collect_rows ((loc, let_doc) :: acc) expr2)
    | _ ->
      let expr_doc =
        let doc = print_expression ~state expr cmt_tbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> add_parens doc
        | Braced braces -> print_braces doc expr braces
        | Nothing -> doc
      in
      List.rev ((expr.pexp_loc, expr_doc) :: acc)
  in
  let rows = collect_rows [] expr in
  let block =
    print_list ~get_loc:fst ~nodes:rows
      ~print:(fun (_, doc) _ -> doc)
      ~force_break:true cmt_tbl
  in
  Doc.breakable_group ~force_break:true
    (if braces then
       Doc.concat
         [
           Doc.lbrace;
           Doc.indent (Doc.concat [Doc.line; block]);
           Doc.line;
           Doc.rbrace;
         ]
     else block)

(*
 * // user types:
 * let f = (a, b) => { a + b }
 *
 * // printer: everything is on one line
 * let f = (a, b) => { a + b }
 *
 * // user types: over multiple lines
 * let f = (a, b) => {
 *   a + b
 * }
 *
 * // printer: over multiple lines
 * let f = (a, b) => {
 *   a + b
 * }
 *)
and print_braces doc expr braces_loc =
  let over_multiple_lines =
    let open Location in
    braces_loc.loc_end.pos_lnum > braces_loc.loc_start.pos_lnum
  in
  match expr.Parsetree.pexp_desc with
  | Pexp_letmodule _ | Pexp_letexception _ | Pexp_let _ | Pexp_open _
  | Pexp_sequence _ ->
    (* already has braces *)
    doc
  | _ ->
    Doc.breakable_group ~force_break:over_multiple_lines
      (Doc.concat
         [
           Doc.lbrace;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  (if Parens.braced_expr expr then add_parens doc else doc);
                ]);
           Doc.soft_line;
           Doc.rbrace;
         ])

and print_override_flag override_flag =
  match override_flag with
  | Asttypes.Override -> Doc.text "!"
  | Fresh -> Doc.nil

and print_direction_flag flag =
  match flag with
  | Asttypes.Downto -> Doc.text " downto "
  | Asttypes.Upto -> Doc.text " to "

and print_expression_record_row ~state (lbl, expr, optional) cmt_tbl
    punning_allowed =
  let cmt_loc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
  let doc =
    Doc.group
      (match expr.pexp_desc with
      | Pexp_ident {txt = Lident key; loc = _keyLoc}
        when punning_allowed && Longident.last lbl.txt = key ->
        (* print punned field *)
        Doc.concat
          [
            print_attributes ~state expr.pexp_attributes cmt_tbl;
            (if optional then Doc.text "?" else Doc.nil);
            print_lident_path lbl cmt_tbl;
          ]
      | _ ->
        Doc.concat
          [
            print_lident_path lbl cmt_tbl;
            Doc.text ": ";
            (if optional then Doc.text "?" else Doc.nil);
            (let doc = print_expression_with_comments ~state expr cmt_tbl in
             match Parens.expr_record_row_rhs ~optional expr with
             | Parens.Parenthesized -> add_parens doc
             | Braced braces -> print_braces doc expr braces
             | Nothing -> doc);
          ])
  in
  print_comments doc cmt_tbl cmt_loc

and print_bs_object_row ~state (lbl, expr) cmt_tbl =
  let cmt_loc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
  let lbl_doc =
    let doc =
      Doc.concat [Doc.text "\""; print_longident lbl.txt; Doc.text "\""]
    in
    print_comments doc cmt_tbl lbl.loc
  in
  let doc =
    Doc.concat
      [
        lbl_doc;
        Doc.text ": ";
        (let doc = print_expression_with_comments ~state expr cmt_tbl in
         match Parens.expr expr with
         | Parens.Parenthesized -> add_parens doc
         | Braced braces -> print_braces doc expr braces
         | Nothing -> doc);
      ]
  in
  print_comments doc cmt_tbl cmt_loc

(* The optional loc indicates whether we need to print the attributes in
 * relation to some location. In practise this means the following:
 *  `@attr type t = string` -> on the same line, print on the same line
 *  `@attr
 *   type t = string` -> attr is on prev line, print the attributes
 *   with a line break between, we respect the users' original layout *)
and print_attributes ?loc ?(inline = false) ~state
    (attrs : Parsetree.attributes) cmt_tbl =
  match ParsetreeViewer.filter_parsing_attrs attrs with
  | [] -> Doc.nil
  | attrs ->
    let line_break =
      match loc with
      | None -> Doc.line
      | Some loc -> (
        match List.rev attrs with
        | ({loc = first_loc}, _) :: _
          when loc.loc_start.pos_lnum > first_loc.loc_end.pos_lnum ->
          Doc.hard_line
        | _ -> Doc.line)
    in
    Doc.concat
      [
        Doc.group
          (Doc.join_with_sep
             (List.map (fun attr -> print_attribute ~state attr cmt_tbl) attrs));
        (if inline then Doc.space else line_break);
      ]

and print_payload ~state (payload : Parsetree.payload) cmt_tbl =
  match payload with
  | PStr [] -> Doc.nil
  | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
    let expr_doc = print_expression_with_comments ~state expr cmt_tbl in
    let needs_parens =
      match attrs with
      | [] -> false
      | _ -> true
    in
    let should_hug = ParsetreeViewer.is_huggable_expression expr in
    if should_hug then
      Doc.concat
        [
          Doc.lparen;
          print_attributes ~state attrs cmt_tbl;
          (if needs_parens then add_parens expr_doc else expr_doc);
          Doc.rparen;
        ]
    else
      Doc.concat
        [
          Doc.lparen;
          Doc.indent
            (Doc.concat
               [
                 Doc.soft_line;
                 print_attributes ~state attrs cmt_tbl;
                 (if needs_parens then add_parens expr_doc else expr_doc);
               ]);
          Doc.soft_line;
          Doc.rparen;
        ]
  | PStr [({pstr_desc = Pstr_value (_recFlag, _bindings)} as si)] ->
    add_parens (print_structure_item ~state si cmt_tbl)
  | PStr structure -> add_parens (print_structure ~state structure cmt_tbl)
  | PTyp typ ->
    Doc.concat
      [
        Doc.lparen;
        Doc.text ":";
        Doc.indent (Doc.concat [Doc.line; print_typ_expr ~state typ cmt_tbl]);
        Doc.soft_line;
        Doc.rparen;
      ]
  | PPat (pat, opt_expr) ->
    let when_doc =
      match opt_expr with
      | Some expr ->
        Doc.concat
          [
            Doc.line;
            Doc.text "if ";
            print_expression_with_comments ~state expr cmt_tbl;
          ]
      | None -> Doc.nil
    in
    Doc.concat
      [
        Doc.lparen;
        Doc.indent
          (Doc.concat
             [
               Doc.soft_line;
               Doc.text "? ";
               print_pattern ~state pat cmt_tbl;
               when_doc;
             ]);
        Doc.soft_line;
        Doc.rparen;
      ]
  | PSig signature ->
    Doc.concat
      [
        Doc.lparen;
        Doc.text ":";
        Doc.indent
          (Doc.concat [Doc.line; print_signature ~state signature cmt_tbl]);
        Doc.soft_line;
        Doc.rparen;
      ]

and print_attribute ?(standalone = false) ~state
    ((id, payload) : Parsetree.attribute) cmt_tbl =
  match (id, payload) with
  | ( {txt = "res.doc"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (txt, _))}, _);
          };
        ] ) ->
    ( Doc.concat
        [
          Doc.text (if standalone then "/***" else "/**");
          Doc.text txt;
          Doc.text "*/";
        ],
      Doc.hard_line )
  | _ ->
    ( Doc.group
        (Doc.concat
           [
             Doc.text (if standalone then "@@" else "@");
             Doc.text id.txt;
             print_payload ~state payload cmt_tbl;
           ]),
      Doc.line )

and print_mod_expr ~state mod_expr cmt_tbl =
  let doc =
    match mod_expr.pmod_desc with
    | Pmod_ident longident_loc -> print_longident_location longident_loc cmt_tbl
    | Pmod_structure [] ->
      let should_break =
        mod_expr.pmod_loc.loc_start.pos_lnum
        < mod_expr.pmod_loc.loc_end.pos_lnum
      in
      Doc.breakable_group ~force_break:should_break
        (Doc.concat
           [
             Doc.lbrace;
             print_comments_inside cmt_tbl mod_expr.pmod_loc;
             Doc.rbrace;
           ])
    | Pmod_structure structure ->
      Doc.breakable_group ~force_break:true
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent
               (Doc.concat
                  [Doc.soft_line; print_structure ~state structure cmt_tbl]);
             Doc.soft_line;
             Doc.rbrace;
           ])
    | Pmod_unpack expr ->
      let should_hug =
        match expr.pexp_desc with
        | Pexp_let _ -> true
        | Pexp_constraint
            ({pexp_desc = Pexp_let _}, {ptyp_desc = Ptyp_package _packageType})
          ->
          true
        | _ -> false
      in
      let expr, module_constraint =
        match expr.pexp_desc with
        | Pexp_constraint
            (expr, {ptyp_desc = Ptyp_package package_type; ptyp_loc}) ->
          let package_doc =
            let doc =
              print_package_type ~state ~print_module_keyword_and_parens:false
                package_type cmt_tbl
            in
            print_comments doc cmt_tbl ptyp_loc
          in
          let type_doc =
            Doc.group
              (Doc.concat
                 [Doc.text ":"; Doc.indent (Doc.concat [Doc.line; package_doc])])
          in
          (expr, type_doc)
        | _ -> (expr, Doc.nil)
      in
      let unpack_doc =
        Doc.group
          (Doc.concat
             [
               print_expression_with_comments ~state expr cmt_tbl;
               module_constraint;
             ])
      in
      Doc.group
        (Doc.concat
           [
             Doc.text "unpack(";
             (if should_hug then unpack_doc
              else
                Doc.concat
                  [
                    Doc.indent (Doc.concat [Doc.soft_line; unpack_doc]);
                    Doc.soft_line;
                  ]);
             Doc.rparen;
           ])
    | Pmod_extension extension ->
      print_extension ~state ~at_module_lvl:false extension cmt_tbl
    | Pmod_apply _ ->
      let args, call_expr = ParsetreeViewer.mod_expr_apply mod_expr in
      let is_unit_sugar =
        match args with
        | [{pmod_desc = Pmod_structure []}] -> true
        | _ -> false
      in
      let should_hug =
        match args with
        | [{pmod_desc = Pmod_structure _}] -> true
        | _ -> false
      in
      Doc.group
        (Doc.concat
           [
             print_mod_expr ~state call_expr cmt_tbl;
             (if is_unit_sugar then
                print_mod_apply_arg ~state
                  (List.hd args [@doesNotRaise])
                  cmt_tbl
              else
                Doc.concat
                  [
                    Doc.lparen;
                    (if should_hug then
                       print_mod_apply_arg ~state
                         (List.hd args [@doesNotRaise])
                         cmt_tbl
                     else
                       Doc.indent
                         (Doc.concat
                            [
                              Doc.soft_line;
                              Doc.join
                                ~sep:(Doc.concat [Doc.comma; Doc.line])
                                (List.map
                                   (fun mod_arg ->
                                     print_mod_apply_arg ~state mod_arg cmt_tbl)
                                   args);
                            ]));
                    (if not should_hug then
                       Doc.concat [Doc.trailing_comma; Doc.soft_line]
                     else Doc.nil);
                    Doc.rparen;
                  ]);
           ])
    | Pmod_constraint (mod_expr, mod_type) ->
      Doc.concat
        [
          print_mod_expr ~state mod_expr cmt_tbl;
          Doc.text ": ";
          print_mod_type ~state mod_type cmt_tbl;
        ]
    | Pmod_functor _ -> print_mod_functor ~state mod_expr cmt_tbl
  in
  let doc =
    if ParsetreeViewer.has_await_attribute mod_expr.pmod_attributes then
      match mod_expr.pmod_desc with
      | Pmod_constraint _ ->
        Doc.concat [Doc.text "await "; Doc.lparen; doc; Doc.rparen]
      | _ -> Doc.concat [Doc.text "await "; doc]
    else doc
  in
  print_comments doc cmt_tbl mod_expr.pmod_loc

and print_mod_functor ~state mod_expr cmt_tbl =
  let parameters, return_mod_expr = ParsetreeViewer.mod_expr_functor mod_expr in
  (* let shouldInline = match returnModExpr.pmod_desc with *)
  (* | Pmod_structure _ | Pmod_ident _ -> true *)
  (* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true *)
  (* | _ -> false *)
  (* in *)
  let return_constraint, return_mod_expr =
    match return_mod_expr.pmod_desc with
    | Pmod_constraint (mod_expr, mod_type) ->
      let constraint_doc =
        let doc = print_mod_type ~state mod_type cmt_tbl in
        if Parens.mod_expr_functor_constraint mod_type then add_parens doc
        else doc
      in
      let mod_constraint = Doc.concat [Doc.text ": "; constraint_doc] in
      (mod_constraint, print_mod_expr ~state mod_expr cmt_tbl)
    | _ -> (Doc.nil, print_mod_expr ~state return_mod_expr cmt_tbl)
  in
  let parameters_doc =
    match parameters with
    | [(attrs, {txt = "*"}, None)] ->
      Doc.group
        (Doc.concat [print_attributes ~state attrs cmt_tbl; Doc.text "()"])
    | [([], {txt = lbl}, None)] -> Doc.text lbl
    | parameters ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.comma; Doc.line])
                      (List.map
                         (fun param ->
                           print_mod_functor_param ~state param cmt_tbl)
                         parameters);
                  ]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.rparen;
           ])
  in
  Doc.group
    (Doc.concat
       [parameters_doc; return_constraint; Doc.text " => "; return_mod_expr])

and print_mod_functor_param ~state (attrs, lbl, opt_mod_type) cmt_tbl =
  let cmt_loc =
    match opt_mod_type with
    | None -> lbl.Asttypes.loc
    | Some mod_type ->
      {lbl.loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end}
  in
  let attrs = print_attributes ~state attrs cmt_tbl in
  let lbl_doc =
    let doc = if lbl.txt = "*" then Doc.text "()" else Doc.text lbl.txt in
    print_comments doc cmt_tbl lbl.loc
  in
  let doc =
    Doc.group
      (Doc.concat
         [
           attrs;
           lbl_doc;
           (match opt_mod_type with
           | None -> Doc.nil
           | Some mod_type ->
             Doc.concat [Doc.text ": "; print_mod_type ~state mod_type cmt_tbl]);
         ])
  in
  print_comments doc cmt_tbl cmt_loc

and print_mod_apply_arg ~state mod_expr cmt_tbl =
  match mod_expr.pmod_desc with
  | Pmod_structure [] -> Doc.text "()"
  | _ -> print_mod_expr ~state mod_expr cmt_tbl

and print_exception_def ~state (constr : Parsetree.extension_constructor)
    cmt_tbl =
  let kind =
    match constr.pext_kind with
    | Pext_rebind longident ->
      Doc.indent
        (Doc.concat
           [Doc.text " ="; Doc.line; print_longident_location longident cmt_tbl])
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadt_doc =
        match gadt with
        | Some typ ->
          Doc.concat [Doc.text ": "; print_typ_expr ~state typ cmt_tbl]
        | None -> Doc.nil
      in
      Doc.concat
        [
          print_constructor_arguments ~state ~indent:false args cmt_tbl; gadt_doc;
        ]
  in
  let name =
    print_comments (Doc.text constr.pext_name.txt) cmt_tbl constr.pext_name.loc
  in
  let doc =
    Doc.group
      (Doc.concat
         [
           print_attributes ~state constr.pext_attributes cmt_tbl;
           Doc.text "exception ";
           name;
           kind;
         ])
  in
  print_comments doc cmt_tbl constr.pext_loc

and print_extension_constructor ~state
    (constr : Parsetree.extension_constructor) cmt_tbl i =
  let attrs = print_attributes ~state constr.pext_attributes cmt_tbl in
  let bar =
    if i > 0 then Doc.text "| " else Doc.if_breaks (Doc.text "| ") Doc.nil
  in
  let kind =
    match constr.pext_kind with
    | Pext_rebind longident ->
      Doc.indent
        (Doc.concat
           [Doc.text " ="; Doc.line; print_longident_location longident cmt_tbl])
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadt_doc =
        match gadt with
        | Some typ ->
          Doc.concat [Doc.text ": "; print_typ_expr ~state typ cmt_tbl]
        | None -> Doc.nil
      in
      Doc.concat
        [
          print_constructor_arguments ~state ~indent:false args cmt_tbl; gadt_doc;
        ]
  in
  let name =
    print_comments (Doc.text constr.pext_name.txt) cmt_tbl constr.pext_name.loc
  in
  Doc.concat [bar; Doc.group (Doc.concat [attrs; name; kind])]

let print_type_params params = print_type_params ~state:(State.init ()) params
let print_typ_expr t = print_typ_expr ~state:(State.init ()) t
let print_expression e = print_expression ~state:(State.init ()) e
let print_pattern p = print_pattern ~state:(State.init ()) p

let print_implementation ~width (s : Parsetree.structure) ~comments =
  let cmt_tbl = CommentTable.make () in
  CommentTable.walk_structure s cmt_tbl comments;
  (* CommentTable.log cmt_tbl; *)
  let doc = print_structure ~state:(State.init ()) s cmt_tbl in
  (* Doc.debug doc; *)
  Doc.to_string ~width doc ^ "\n"

let print_interface ~width (s : Parsetree.signature) ~comments =
  let cmt_tbl = CommentTable.make () in
  CommentTable.walk_signature s cmt_tbl comments;
  Doc.to_string ~width (print_signature ~state:(State.init ()) s cmt_tbl) ^ "\n"

let print_structure = print_structure ~state:(State.init ())
