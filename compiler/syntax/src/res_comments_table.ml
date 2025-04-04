module Comment = Res_comment
module Doc = Res_doc
module ParsetreeViewer = Res_parsetree_viewer

type t = {
  leading: (Location.t, Comment.t list) Hashtbl.t;
  inside: (Location.t, Comment.t list) Hashtbl.t;
  trailing: (Location.t, Comment.t list) Hashtbl.t;
}

let make () =
  {
    leading = Hashtbl.create 100;
    inside = Hashtbl.create 100;
    trailing = Hashtbl.create 100;
  }

let copy tbl =
  {
    leading = Hashtbl.copy tbl.leading;
    inside = Hashtbl.copy tbl.inside;
    trailing = Hashtbl.copy tbl.trailing;
  }

let empty = make ()

let rec list_last = function
  | [] -> failwith "list_last: empty list"
  | [x] -> x
  | _ :: rest -> list_last rest

let print_location (k : Warnings.loc) =
  Doc.concat
    [
      Doc.lbracket;
      Doc.text (string_of_int k.loc_start.pos_lnum);
      Doc.text ":";
      Doc.text (string_of_int (k.loc_start.pos_cnum - k.loc_start.pos_bol));
      Doc.text "-";
      Doc.text (string_of_int k.loc_end.pos_lnum);
      Doc.text ":";
      Doc.text (string_of_int (k.loc_end.pos_cnum - k.loc_end.pos_bol));
      Doc.rbracket;
    ]

let print_entries tbl =
  Hashtbl.fold
    (fun (k : Location.t) (v : Comment.t list) acc ->
      let loc = print_location k in
      let doc =
        Doc.breakable_group ~force_break:true
          (Doc.concat
             [
               loc;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.line;
                      Doc.join
                        ~sep:(Doc.concat [Doc.comma; Doc.line])
                        (List.map (fun c -> Doc.text (Comment.txt c)) v);
                    ]);
               Doc.line;
             ])
      in
      doc :: acc)
    tbl []

let log t =
  let leading_stuff = print_entries t.leading in
  let trailing_stuff = print_entries t.trailing in
  let stuff_inside = print_entries t.inside in
  Doc.breakable_group ~force_break:true
    (Doc.concat
       [
         Doc.text "leading comments:";
         Doc.indent (Doc.concat [Doc.line; Doc.concat leading_stuff]);
         Doc.line;
         Doc.text "comments inside:";
         Doc.indent (Doc.concat [Doc.line; Doc.concat stuff_inside]);
         Doc.line;
         Doc.text "trailing comments:";
         Doc.indent (Doc.concat [Doc.line; Doc.concat trailing_stuff]);
         Doc.line;
       ])
  |> Doc.to_string ~width:80 |> print_endline

let attach tbl loc comments =
  match comments with
  | [] -> ()
  | comments -> Hashtbl.replace tbl loc comments

(* Partitions a list of comments into three groups based on their position relative to a location:
 * - leading: comments that end before the location's start position
 * - inside: comments that overlap with the location
 * - trailing: comments that start after the location's end position
 *
 * For example, given code:
 *   /* comment1 */ let x = /* comment2 */ 5 /* comment3 */
 *
 * When splitting around the location of `x = 5`:
 * - leading: [comment1]
 * - inside: [comment2]  
 * - trailing: [comment3]
 * 
 * This is the primary comment partitioning function used for associating comments
 * with AST nodes during the tree traversal.
 *
 * Parameters:
 * - comments: list of comments to partition
 * - loc: location to split around
 *
 * Returns: (leading_comments, inside_comments, trailing_comments)
 *)
let partition_by_loc comments loc =
  let rec loop (leading, inside, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmt_loc = Comment.loc comment in
      if cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
        loop (comment :: leading, inside, trailing) rest
      else if cmt_loc.loc_start.pos_cnum >= loc.loc_end.pos_cnum then
        loop (leading, inside, comment :: trailing) rest
      else loop (leading, comment :: inside, trailing) rest
    | [] -> (List.rev leading, List.rev inside, List.rev trailing)
  in
  loop ([], [], []) comments

(* Splits a list of comments into two groups based on their position relative to a location:
 * - leading: comments that end before the location's start
 * - trailing: comments that start at or after the location's start
 *
 * For example, given the code:
 *   /* comment1 */ let /* comment2 */ x = 1 /* comment3 */
 *
 * When splitting around `x`'s location:
 * - leading: [comment1, comment2]
 * - trailing: [comment3]
 *
 * Parameters:
 * - comments: list of comments to partition
 * - loc: location to split around
 *
 * Returns: (leading_comments, trailing_comments)
 *)
let partition_leading_trailing comments loc =
  let rec loop (leading, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmt_loc = Comment.loc comment in
      if cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
        loop (comment :: leading, trailing) rest
      else loop (leading, comment :: trailing) rest
    | [] -> (List.rev leading, List.rev trailing)
  in
  loop ([], []) comments

(* Splits comments into two groups based on whether they start on the same line as a location's end position.
 *
 * This is particularly useful for handling comments that appear on the same line as a syntax element
 * versus comments that appear on subsequent lines.
 *
 * For example, given code:
 *   let x = 1 /* same line comment */
 *   /* different line comment */
 *
 * When splitting around `x = 1`'s location:
 * - on_same_line: [/* same line comment */]
 * - on_other_line: [/* different line comment */]
 *
 * This function is often used for formatting decisions where comments on the same line
 * should be treated differently than comments on different lines (like in JSX elements).
 *
 * Parameters:
 * - loc: location to compare line numbers against
 * - comments: list of comments to partition
 *
 * Returns: (on_same_line_comments, on_other_line_comments)
 *)
let partition_by_on_same_line loc comments =
  let rec loop (on_same_line, on_other_line) comments =
    let open Location in
    match comments with
    | [] -> (List.rev on_same_line, List.rev on_other_line)
    | comment :: rest ->
      let cmt_loc = Comment.loc comment in
      if cmt_loc.loc_start.pos_lnum == loc.loc_end.pos_lnum then
        loop (comment :: on_same_line, on_other_line) rest
      else loop (on_same_line, comment :: on_other_line) rest
  in
  loop ([], []) comments

let partition_adjacent_trailing loc1 comments =
  let open Location in
  let open Lexing in
  let rec loop ~prev_end_pos after_loc1 comments =
    match comments with
    | [] -> (List.rev after_loc1, [])
    | comment :: rest as comments ->
      let cmt_prev_end_pos = Comment.prev_tok_end_pos comment in
      if prev_end_pos.Lexing.pos_cnum == cmt_prev_end_pos.pos_cnum then
        let comment_end = (Comment.loc comment).loc_end in
        loop ~prev_end_pos:comment_end (comment :: after_loc1) rest
      else (List.rev after_loc1, comments)
  in
  loop ~prev_end_pos:loc1.loc_end [] comments

(* Splits comments that follow a location but come before another token.
 * This is particularly useful for handling comments between two tokens
 * where traditional leading/trailing partitioning isn't precise enough.
 *
 * For example, given code:
 *   let x = 1 /* comment */ + 2
 *
 * When splitting comments between `1` and `+`:
 * - first_part: [/* comment */]  (comments on same line as loc and before next_token)
 * - rest: []                     (remaining comments)
 *
 * Parameters:
 * - loc: location of the reference token
 * - next_token: location of the next token
 * - comments: list of comments to partition
 *
 * Returns: (first_part, rest) where first_part contains comments on the same line as loc
 * that appear entirely before next_token, and rest contains all other comments.
 *
 * This function is useful for precisely attaching comments between specific tokens
 * in constructs like JSX props, function arguments, and other multi-token expressions.
 *)
let partition_adjacent_trailing_before_next_token_on_same_line
    (loc : Warnings.loc) (next_token : Warnings.loc) (comments : Comment.t list)
    : Comment.t list * Comment.t list =
  let open Location in
  let open Lexing in
  let rec loop after_loc comments =
    match comments with
    | [] -> (List.rev after_loc, [])
    | comment :: rest ->
      (* Check if the comment is on the same line as the loc, and is entirely before the next_token *)
      let cmt_loc = Comment.loc comment in
      if
        (* Same line *)
        cmt_loc.loc_start.pos_lnum == loc.loc_end.pos_lnum
        (* comment after loc *)
        && cmt_loc.loc_start.pos_cnum > loc.loc_end.pos_cnum
        (* comment before next_token *)
        && cmt_loc.loc_end.pos_cnum <= next_token.loc_start.pos_cnum
      then loop (comment :: after_loc) rest
      else (List.rev after_loc, comments)
  in
  loop [] comments

(* Extracts comments that appear between two specified line numbers in a source file.
 *
 * This function is particularly useful for handling comments that should be preserved
 * between two syntax elements that appear on different lines, such as comments between
 * opening and closing tags in JSX elements.
 *
 * For example, given code:
 *   <div>
 *     // comment 1
 *     // comment 2
 *   </div>
 *
 * When calling partition_between_lines with the line numbers of the opening and closing tags:
 * - between_comments: [comment 1, comment 2]
 * - rest: any comments that appear before or after the specified lines
 *
 * Parameters:
 * - start_line: the line number after which to start collecting comments
 * - end_line: the line number before which to stop collecting comments
 * - comments: list of comments to partition
 *
 * Returns: (between_comments, rest) where between_comments contains all comments
 * entirely between the start_line and end_line, and rest contains all other comments.
 *)
let partition_between_lines start_line end_line comments =
  let open Location in
  let open Lexing in
  let rec loop between_comments comments =
    match comments with
    | [] -> (List.rev between_comments, [])
    | comment :: rest ->
      (* Check if the comment is between the start_line and end_line *)
      let cmt_loc = Comment.loc comment in
      if
        cmt_loc.loc_start.pos_lnum > start_line
        && cmt_loc.loc_end.pos_lnum < end_line
      then loop (comment :: between_comments) rest
      else (List.rev between_comments, comments)
  in
  loop [] comments

let rec collect_list_patterns acc pattern =
  let open Parsetree in
  match pattern.ppat_desc with
  | Ppat_construct
      ({txt = Longident.Lident "::"}, Some {ppat_desc = Ppat_tuple [pat; rest]})
    ->
    collect_list_patterns (pat :: acc) rest
  | Ppat_construct ({txt = Longident.Lident "[]"}, None) -> List.rev acc
  | _ -> List.rev (pattern :: acc)

let rec collect_list_exprs acc expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct
      ({txt = Longident.Lident "::"}, Some {pexp_desc = Pexp_tuple [expr; rest]})
    ->
    collect_list_exprs (expr :: acc) rest
  | Pexp_construct ({txt = Longident.Lident "[]"}, _) -> List.rev acc
  | _ -> List.rev (expr :: acc)

(* TODO: use ParsetreeViewer *)
let arrow_type ct =
  let open Parsetree in
  let rec process attrs_before acc typ =
    match typ with
    | {
     ptyp_desc = Ptyp_arrow {lbl = Nolabel as lbl; arg; ret};
     ptyp_attributes = [];
    } ->
      let arg = ([], lbl, arg) in
      process attrs_before (arg :: acc) ret
    | {
     ptyp_desc = Ptyp_arrow {lbl = Nolabel as lbl; arg; ret};
     ptyp_attributes = [({txt = "bs"}, _)] as attrs;
    } ->
      let arg = (attrs, lbl, arg) in
      process attrs_before (arg :: acc) ret
    | {ptyp_desc = Ptyp_arrow {lbl = Nolabel}} as return_type ->
      let args = List.rev acc in
      (attrs_before, args, return_type)
    | {ptyp_desc = Ptyp_arrow {lbl; arg; ret}; ptyp_attributes = attrs} ->
      let arg = (attrs, lbl, arg) in
      process attrs_before (arg :: acc) ret
    | typ -> (attrs_before, List.rev acc, typ)
  in
  match ct with
  | {ptyp_desc = Ptyp_arrow {lbl = Nolabel}; ptyp_attributes = attrs} as typ ->
    process attrs [] {typ with ptyp_attributes = []}
  | typ -> process [] [] typ

(* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
let mod_expr_apply mod_expr =
  let rec loop acc mod_expr =
    match mod_expr with
    | {Parsetree.pmod_desc = Pmod_apply (next, arg)} -> loop (arg :: acc) next
    | _ -> mod_expr :: acc
  in
  loop [] mod_expr

(* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
let mod_expr_functor mod_expr =
  let rec loop acc mod_expr =
    match mod_expr with
    | {
     Parsetree.pmod_desc = Pmod_functor (lbl, mod_type, return_mod_expr);
     pmod_attributes = attrs;
    } ->
      let param = (attrs, lbl, mod_type) in
      loop (param :: acc) return_mod_expr
    | return_mod_expr -> (List.rev acc, return_mod_expr)
  in
  loop [] mod_expr

let functor_type modtype =
  let rec process acc modtype =
    match modtype with
    | {
     Parsetree.pmty_desc = Pmty_functor (lbl, arg_type, return_type);
     pmty_attributes = attrs;
    } ->
      let arg = (attrs, lbl, arg_type) in
      process (arg :: acc) return_type
    | mod_type -> (List.rev acc, mod_type)
  in
  process [] modtype

let fun_expr expr =
  let open Parsetree in
  (* Turns (type t, type u, type z) into "type t u z" *)
  let rec collect_new_types acc return_expr =
    match return_expr with
    | {pexp_desc = Pexp_newtype (string_loc, return_expr); pexp_attributes = []}
      ->
      collect_new_types (string_loc :: acc) return_expr
    | return_expr ->
      let loc =
        match (acc, List.rev acc) with
        | _startLoc :: _, end_loc :: _ ->
          {end_loc.loc with loc_end = end_loc.loc.loc_end}
        | _ -> Location.none
      in
      let txt =
        List.fold_right
          (fun curr acc -> acc ^ " " ^ curr.Location.txt)
          acc "type"
      in
      (Location.mkloc txt loc, return_expr)
  in
  (* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
   * otherwise this function would need to return a variant:
   * | NormalParamater(...)
   * | NewType(...)
   * This complicates printing with an extra variant/boxing/allocation for a code-path
   * that is not often used. Lets just keep it simple for now *)
  let rec collect attrs_before acc expr =
    match expr with
    | {
     pexp_desc =
       Pexp_fun
         {
           arg_label = lbl;
           default = default_expr;
           lhs = pattern;
           rhs = return_expr;
         };
     pexp_attributes = [];
    } ->
      let parameter = ([], lbl, default_expr, pattern) in
      collect attrs_before (parameter :: acc) return_expr
    | {pexp_desc = Pexp_newtype (string_loc, rest); pexp_attributes = attrs} ->
      let var, return_expr = collect_new_types [string_loc] rest in
      let parameter =
        ( attrs,
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.var ~loc:string_loc.loc var )
      in
      collect attrs_before (parameter :: acc) return_expr
    | {
     pexp_desc =
       Pexp_fun
         {
           arg_label = lbl;
           default = default_expr;
           lhs = pattern;
           rhs = return_expr;
         };
     pexp_attributes = [({txt = "bs"}, _)] as attrs;
    } ->
      let parameter = (attrs, lbl, default_expr, pattern) in
      collect attrs_before (parameter :: acc) return_expr
    | {
     pexp_desc =
       Pexp_fun
         {
           arg_label = (Labelled _ | Optional _) as lbl;
           default = default_expr;
           lhs = pattern;
           rhs = return_expr;
         };
     pexp_attributes = attrs;
    } ->
      let parameter = (attrs, lbl, default_expr, pattern) in
      collect attrs_before (parameter :: acc) return_expr
    | expr -> (attrs_before, List.rev acc, expr)
  in
  match expr with
  | {pexp_desc = Pexp_fun {arg_label = Nolabel}; pexp_attributes = attrs} as
    expr ->
    collect attrs [] {expr with pexp_attributes = []}
  | expr -> collect [] [] expr

let rec is_block_expr expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_letmodule _ | Pexp_letexception _ | Pexp_let _ | Pexp_open _
  | Pexp_sequence _ ->
    true
  | Pexp_apply {funct = call_expr} when is_block_expr call_expr -> true
  | Pexp_constraint (expr, _) when is_block_expr expr -> true
  | Pexp_field (expr, _) when is_block_expr expr -> true
  | Pexp_setfield (expr, _, _) when is_block_expr expr -> true
  | _ -> false

let is_if_then_else_expr expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ifthenelse _ -> true
  | _ -> false

type node =
  | Case of Parsetree.case
  | CoreType of Parsetree.core_type
  | ExprArgument of {expr: Parsetree.expression; loc: Location.t}
  | Expression of Parsetree.expression
  | ExprRecordRow of Longident.t Asttypes.loc * Parsetree.expression
  | ExtensionConstructor of Parsetree.extension_constructor
  | LabelDeclaration of Parsetree.label_declaration
  | ModuleBinding of Parsetree.module_binding
  | ModuleDeclaration of Parsetree.module_declaration
  | ModuleExpr of Parsetree.module_expr
  | ObjectField of Parsetree.object_field
  | PackageConstraint of Longident.t Asttypes.loc * Parsetree.core_type
  | Pattern of Parsetree.pattern
  | PatternRecordRow of Longident.t Asttypes.loc * Parsetree.pattern
  | RowField of Parsetree.row_field
  | SignatureItem of Parsetree.signature_item
  | StructureItem of Parsetree.structure_item
  | TypeDeclaration of Parsetree.type_declaration
  | ValueBinding of Parsetree.value_binding
  | JsxProp of Parsetree.jsx_prop

let get_loc node =
  let open Parsetree in
  match node with
  | Case case ->
    {
      case.pc_lhs.ppat_loc with
      loc_end =
        (match ParsetreeViewer.process_braces_attr case.pc_rhs with
        | None, _ -> case.pc_rhs.pexp_loc.loc_end
        | Some ({loc}, _), _ -> loc.Location.loc_end);
    }
  | CoreType ct -> ct.ptyp_loc
  | ExprArgument {loc} -> loc
  | Expression e -> (
    match e.pexp_attributes with
    | ({txt = "res.braces" | "ns.braces"; loc}, _) :: _ -> loc
    | _ -> e.pexp_loc)
  | ExprRecordRow (li, e) -> {li.loc with loc_end = e.pexp_loc.loc_end}
  | ExtensionConstructor ec -> ec.pext_loc
  | LabelDeclaration ld -> ld.pld_loc
  | ModuleBinding mb -> mb.pmb_loc
  | ModuleDeclaration md -> md.pmd_loc
  | ModuleExpr me -> me.pmod_loc
  | ObjectField field -> (
    match field with
    | Parsetree.Otag (lbl, _, typ) ->
      {lbl.loc with loc_end = typ.ptyp_loc.loc_end}
    | _ -> Location.none)
  | PackageConstraint (li, te) -> {li.loc with loc_end = te.ptyp_loc.loc_end}
  | Pattern p -> p.ppat_loc
  | PatternRecordRow (li, p) -> {li.loc with loc_end = p.ppat_loc.loc_end}
  | RowField rf -> (
    match rf with
    | Parsetree.Rtag ({loc}, _, _, _) -> loc
    | Rinherit {ptyp_loc} -> ptyp_loc)
  | SignatureItem si -> si.psig_loc
  | StructureItem si -> si.pstr_loc
  | TypeDeclaration td -> td.ptype_loc
  | ValueBinding vb -> vb.pvb_loc
  | JsxProp prop -> ParsetreeViewer.get_jsx_prop_loc prop

let rec walk_structure s t comments =
  match s with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | s -> walk_list (s |> List.map (fun si -> StructureItem si)) t comments

and walk_structure_item si t comments =
  match si.Parsetree.pstr_desc with
  | _ when comments = [] -> ()
  | Pstr_primitive value_description ->
    walk_value_description value_description t comments
  | Pstr_open open_description ->
    walk_open_description open_description t comments
  | Pstr_value (_, value_bindings) ->
    walk_value_bindings value_bindings t comments
  | Pstr_type (_, type_declarations) ->
    walk_type_declarations type_declarations t comments
  | Pstr_eval (expr, _) -> walk_expression expr t comments
  | Pstr_module module_binding -> walk_module_binding module_binding t comments
  | Pstr_recmodule module_bindings ->
    walk_list
      (module_bindings |> List.map (fun mb -> ModuleBinding mb))
      t comments
  | Pstr_modtype mod_typ_decl ->
    walk_module_type_declaration mod_typ_decl t comments
  | Pstr_attribute attribute -> walk_attribute attribute t comments
  | Pstr_extension (extension, _) -> walk_extension extension t comments
  | Pstr_include include_declaration ->
    walk_include_declaration include_declaration t comments
  | Pstr_exception extension_constructor ->
    walk_extension_constructor extension_constructor t comments
  | Pstr_typext type_extension -> walk_type_extension type_extension t comments

and walk_value_description vd t comments =
  let leading, trailing =
    partition_leading_trailing comments vd.pval_name.loc
  in
  attach t.leading vd.pval_name.loc leading;
  let after_name, rest =
    partition_adjacent_trailing vd.pval_name.loc trailing
  in
  attach t.trailing vd.pval_name.loc after_name;
  let before, inside, after = partition_by_loc rest vd.pval_type.ptyp_loc in
  attach t.leading vd.pval_type.ptyp_loc before;
  walk_core_type vd.pval_type t inside;
  attach t.trailing vd.pval_type.ptyp_loc after

and walk_type_extension te t comments =
  let leading, trailing =
    partition_leading_trailing comments te.ptyext_path.loc
  in
  attach t.leading te.ptyext_path.loc leading;
  let after_path, rest =
    partition_adjacent_trailing te.ptyext_path.loc trailing
  in
  attach t.trailing te.ptyext_path.loc after_path;

  (* type params *)
  let rest =
    match te.ptyext_params with
    | [] -> rest
    | type_params ->
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
        ~walk_node:walk_type_param ~newline_delimited:false type_params t rest
  in
  walk_list
    (te.ptyext_constructors |> List.map (fun ec -> ExtensionConstructor ec))
    t rest

and walk_include_declaration incl_decl t comments =
  let before, inside, after =
    partition_by_loc comments incl_decl.pincl_mod.pmod_loc
  in
  attach t.leading incl_decl.pincl_mod.pmod_loc before;
  walk_module_expr incl_decl.pincl_mod t inside;
  attach t.trailing incl_decl.pincl_mod.pmod_loc after

and walk_module_type_declaration mtd t comments =
  let leading, trailing =
    partition_leading_trailing comments mtd.pmtd_name.loc
  in
  attach t.leading mtd.pmtd_name.loc leading;
  match mtd.pmtd_type with
  | None -> attach t.trailing mtd.pmtd_name.loc trailing
  | Some mod_type ->
    let after_name, rest =
      partition_adjacent_trailing mtd.pmtd_name.loc trailing
    in
    attach t.trailing mtd.pmtd_name.loc after_name;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_mod_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after

and walk_module_binding mb t comments =
  let leading, trailing = partition_leading_trailing comments mb.pmb_name.loc in
  attach t.leading mb.pmb_name.loc leading;
  let after_name, rest = partition_adjacent_trailing mb.pmb_name.loc trailing in
  attach t.trailing mb.pmb_name.loc after_name;
  let leading, inside, trailing = partition_by_loc rest mb.pmb_expr.pmod_loc in
  (match mb.pmb_expr.pmod_desc with
  | Pmod_constraint _ ->
    walk_module_expr mb.pmb_expr t (List.concat [leading; inside])
  | _ ->
    attach t.leading mb.pmb_expr.pmod_loc leading;
    walk_module_expr mb.pmb_expr t inside);
  attach t.trailing mb.pmb_expr.pmod_loc trailing

and walk_signature signature t comments =
  match signature with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | _s ->
    walk_list (signature |> List.map (fun si -> SignatureItem si)) t comments

and walk_signature_item (si : Parsetree.signature_item) t comments =
  match si.psig_desc with
  | _ when comments = [] -> ()
  | Psig_value value_description ->
    walk_value_description value_description t comments
  | Psig_type (_, type_declarations) ->
    walk_type_declarations type_declarations t comments
  | Psig_typext type_extension -> walk_type_extension type_extension t comments
  | Psig_exception extension_constructor ->
    walk_extension_constructor extension_constructor t comments
  | Psig_module module_declaration ->
    walk_module_declaration module_declaration t comments
  | Psig_recmodule module_declarations ->
    walk_list
      (module_declarations |> List.map (fun md -> ModuleDeclaration md))
      t comments
  | Psig_modtype module_type_declaration ->
    walk_module_type_declaration module_type_declaration t comments
  | Psig_open open_description ->
    walk_open_description open_description t comments
  | Psig_include include_description ->
    walk_include_description include_description t comments
  | Psig_attribute attribute -> walk_attribute attribute t comments
  | Psig_extension (extension, _) -> walk_extension extension t comments

and walk_include_description id t comments =
  let before, inside, after = partition_by_loc comments id.pincl_mod.pmty_loc in
  attach t.leading id.pincl_mod.pmty_loc before;
  walk_mod_type id.pincl_mod t inside;
  attach t.trailing id.pincl_mod.pmty_loc after

and walk_module_declaration md t comments =
  let leading, trailing = partition_leading_trailing comments md.pmd_name.loc in
  attach t.leading md.pmd_name.loc leading;
  let after_name, rest = partition_adjacent_trailing md.pmd_name.loc trailing in
  attach t.trailing md.pmd_name.loc after_name;
  let leading, inside, trailing = partition_by_loc rest md.pmd_type.pmty_loc in
  attach t.leading md.pmd_type.pmty_loc leading;
  walk_mod_type md.pmd_type t inside;
  attach t.trailing md.pmd_type.pmty_loc trailing

and walk_node node tbl comments =
  match node with
  | Case c -> walk_case c tbl comments
  | CoreType ct -> walk_core_type ct tbl comments
  | ExprArgument ea -> walk_expr_argument ea.expr ea.loc tbl comments
  | Expression e -> walk_expression e tbl comments
  | ExprRecordRow (ri, e) -> walk_expr_record_row (ri, e) tbl comments
  | ExtensionConstructor ec -> walk_extension_constructor ec tbl comments
  | LabelDeclaration ld -> walk_label_declaration ld tbl comments
  | ModuleBinding mb -> walk_module_binding mb tbl comments
  | ModuleDeclaration md -> walk_module_declaration md tbl comments
  | ModuleExpr me -> walk_module_expr me tbl comments
  | ObjectField f -> walk_object_field f tbl comments
  | PackageConstraint (li, te) -> walk_package_constraint (li, te) tbl comments
  | Pattern p -> walk_pattern p tbl comments
  | PatternRecordRow (li, p) -> walk_pattern_record_row (li, p) tbl comments
  | RowField rf -> walk_row_field rf tbl comments
  | SignatureItem si -> walk_signature_item si tbl comments
  | StructureItem si -> walk_structure_item si tbl comments
  | TypeDeclaration td -> walk_type_declaration td tbl comments
  | ValueBinding vb -> walk_value_binding vb tbl comments
  | JsxProp prop -> walk_jsx_prop prop tbl comments

and walk_list : ?prev_loc:Location.t -> node list -> t -> Comment.t list -> unit
    =
 fun ?prev_loc l t comments ->
  match l with
  | _ when comments = [] -> ()
  | [] -> (
    match prev_loc with
    | Some loc -> attach t.trailing loc comments
    | None -> ())
  | node :: rest ->
    let curr_loc = get_loc node in
    let leading, inside, trailing = partition_by_loc comments curr_loc in
    (match prev_loc with
    | None ->
      (* first node, all leading comments attach here *)
      attach t.leading curr_loc leading
    | Some prev_loc ->
      (* Same line *)
      if prev_loc.loc_end.pos_lnum == curr_loc.loc_start.pos_lnum then (
        let after_prev, before_curr =
          partition_adjacent_trailing prev_loc leading
        in
        attach t.trailing prev_loc after_prev;
        attach t.leading curr_loc before_curr)
      else
        let on_same_line_as_prev, after_prev =
          partition_by_on_same_line prev_loc leading
        in
        attach t.trailing prev_loc on_same_line_as_prev;
        let leading, _inside, _trailing =
          partition_by_loc after_prev curr_loc
        in
        attach t.leading curr_loc leading);
    walk_node node t inside;
    walk_list ~prev_loc:curr_loc rest t trailing

(* The parsetree doesn't always contain location info about the opening or
 * closing token of a "list-of-things". This routine visits the whole list,
 * but returns any remaining comments that likely fall after the whole list. *)
and visit_list_but_continue_with_remaining_comments :
    'node.
    ?prev_loc:Location.t ->
    newline_delimited:bool ->
    get_loc:('node -> Location.t) ->
    walk_node:('node -> t -> Comment.t list -> unit) ->
    'node list ->
    t ->
    Comment.t list ->
    Comment.t list =
 fun ?prev_loc ~newline_delimited ~get_loc ~walk_node l t comments ->
  let open Location in
  match l with
  | _ when comments = [] -> []
  | [] -> (
    match prev_loc with
    | Some loc ->
      let after_prev, rest =
        if newline_delimited then partition_by_on_same_line loc comments
        else partition_adjacent_trailing loc comments
      in
      attach t.trailing loc after_prev;
      rest
    | None -> comments)
  | node :: rest ->
    let curr_loc = get_loc node in
    let leading, inside, trailing = partition_by_loc comments curr_loc in
    let () =
      match prev_loc with
      | None ->
        (* first node, all leading comments attach here *)
        attach t.leading curr_loc leading;
        ()
      | Some prev_loc ->
        (* Same line *)
        if prev_loc.loc_end.pos_lnum == curr_loc.loc_start.pos_lnum then
          let after_prev, before_curr =
            partition_adjacent_trailing prev_loc leading
          in
          let () = attach t.trailing prev_loc after_prev in
          let () = attach t.leading curr_loc before_curr in
          ()
        else
          let on_same_line_as_prev, after_prev =
            partition_by_on_same_line prev_loc leading
          in
          let () = attach t.trailing prev_loc on_same_line_as_prev in
          let leading, _inside, _trailing =
            partition_by_loc after_prev curr_loc
          in
          let () = attach t.leading curr_loc leading in
          ()
    in
    walk_node node t inside;
    visit_list_but_continue_with_remaining_comments ~prev_loc:curr_loc ~get_loc
      ~walk_node ~newline_delimited rest t trailing

and walk_value_bindings vbs t comments =
  walk_list (vbs |> List.map (fun vb -> ValueBinding vb)) t comments

and walk_open_description open_description t comments =
  let loc = open_description.popen_lid.loc in
  let leading, trailing = partition_leading_trailing comments loc in
  attach t.leading loc leading;
  attach t.trailing loc trailing

and walk_type_declarations type_declarations t comments =
  walk_list
    (type_declarations |> List.map (fun td -> TypeDeclaration td))
    t comments

and walk_type_param (typexpr, _variance) t comments =
  walk_core_type typexpr t comments

and walk_type_declaration (td : Parsetree.type_declaration) t comments =
  let before_name, rest =
    partition_leading_trailing comments td.ptype_name.loc
  in
  attach t.leading td.ptype_name.loc before_name;

  let after_name, rest = partition_adjacent_trailing td.ptype_name.loc rest in
  attach t.trailing td.ptype_name.loc after_name;

  (* type params *)
  let rest =
    match td.ptype_params with
    | [] -> rest
    | type_params ->
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
        ~walk_node:walk_type_param ~newline_delimited:false type_params t rest
  in

  (* manifest:  = typexpr *)
  let rest =
    match td.ptype_manifest with
    | Some typexpr ->
      let before_typ, inside_typ, after_typ =
        partition_by_loc rest typexpr.ptyp_loc
      in
      attach t.leading typexpr.ptyp_loc before_typ;
      walk_core_type typexpr t inside_typ;
      let after_typ, rest =
        partition_adjacent_trailing typexpr.Parsetree.ptyp_loc after_typ
      in
      attach t.trailing typexpr.ptyp_loc after_typ;
      rest
    | None -> rest
  in

  let rest =
    match td.ptype_kind with
    | Ptype_abstract | Ptype_open -> rest
    | Ptype_record label_declarations ->
      let () =
        if label_declarations = [] then attach t.inside td.ptype_loc rest
        else
          walk_list
            (label_declarations |> List.map (fun ld -> LabelDeclaration ld))
            t rest
      in
      []
    | Ptype_variant constructor_declarations ->
      walk_constructor_declarations constructor_declarations t rest
  in
  attach t.trailing td.ptype_loc rest

and walk_label_declarations lds t comments =
  visit_list_but_continue_with_remaining_comments
    ~get_loc:(fun ld -> ld.Parsetree.pld_loc)
    ~walk_node:walk_label_declaration ~newline_delimited:false lds t comments

and walk_label_declaration ld t comments =
  let before_name, rest = partition_leading_trailing comments ld.pld_name.loc in
  attach t.leading ld.pld_name.loc before_name;
  let after_name, rest = partition_adjacent_trailing ld.pld_name.loc rest in
  attach t.trailing ld.pld_name.loc after_name;
  let before_typ, inside_typ, after_typ =
    partition_by_loc rest ld.pld_type.ptyp_loc
  in
  attach t.leading ld.pld_type.ptyp_loc before_typ;
  walk_core_type ld.pld_type t inside_typ;
  attach t.trailing ld.pld_type.ptyp_loc after_typ

and walk_constructor_declarations cds t comments =
  visit_list_but_continue_with_remaining_comments
    ~get_loc:(fun cd -> cd.Parsetree.pcd_loc)
    ~walk_node:walk_constructor_declaration ~newline_delimited:false cds t
    comments

and walk_constructor_declaration cd t comments =
  let before_name, rest = partition_leading_trailing comments cd.pcd_name.loc in
  attach t.leading cd.pcd_name.loc before_name;
  let after_name, rest = partition_adjacent_trailing cd.pcd_name.loc rest in
  attach t.trailing cd.pcd_name.loc after_name;
  let rest = walk_constructor_arguments cd.pcd_args t rest in

  let rest =
    match cd.pcd_res with
    | Some typexpr ->
      let before_typ, inside_typ, after_typ =
        partition_by_loc rest typexpr.ptyp_loc
      in
      attach t.leading typexpr.ptyp_loc before_typ;
      walk_core_type typexpr t inside_typ;
      let after_typ, rest =
        partition_adjacent_trailing typexpr.Parsetree.ptyp_loc after_typ
      in
      attach t.trailing typexpr.ptyp_loc after_typ;
      rest
    | None -> rest
  in
  attach t.trailing cd.pcd_loc rest

and walk_constructor_arguments args t comments =
  match args with
  | Pcstr_tuple typexprs ->
    visit_list_but_continue_with_remaining_comments
      ~get_loc:(fun n -> n.Parsetree.ptyp_loc)
      ~walk_node:walk_core_type ~newline_delimited:false typexprs t comments
  | Pcstr_record label_declarations ->
    walk_label_declarations label_declarations t comments

and walk_value_binding vb t comments =
  let open Location in
  let vb =
    let open Parsetree in
    match (vb.pvb_pat, vb.pvb_expr) with
    | ( {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly ([], t)})},
        {pexp_desc = Pexp_constraint (expr, _typ)} ) ->
      {
        vb with
        pvb_pat =
          Ast_helper.Pat.constraint_
            ~loc:{pat.ppat_loc with loc_end = t.Parsetree.ptyp_loc.loc_end}
            pat t;
        pvb_expr = expr;
      }
    | ( {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly (_ :: _, t)})},
        {pexp_desc = Pexp_fun _} ) ->
      {
        vb with
        pvb_pat =
          {
            vb.pvb_pat with
            ppat_loc = {pat.ppat_loc with loc_end = t.ptyp_loc.loc_end};
          };
      }
    | ( ({
           ppat_desc =
             Ppat_constraint (pat, ({ptyp_desc = Ptyp_poly (_ :: _, t)} as typ));
         } as constrained_pattern),
        {pexp_desc = Pexp_newtype (_, {pexp_desc = Pexp_constraint (expr, _)})}
      ) ->
      (*
       * The location of the Ptyp_poly on the pattern is the whole thing.
       * let x:
       *   type t. (int, int) => int =
       *   (a, b) => {
       *     // comment
       *     a + b
       *   }
       *)
      {
        vb with
        pvb_pat =
          {
            constrained_pattern with
            ppat_desc = Ppat_constraint (pat, typ);
            ppat_loc =
              {constrained_pattern.ppat_loc with loc_end = t.ptyp_loc.loc_end};
          };
        pvb_expr = expr;
      }
    | _ -> vb
  in
  let pattern_loc = vb.Parsetree.pvb_pat.ppat_loc in
  let expr_loc = vb.Parsetree.pvb_expr.pexp_loc in
  let expr = vb.pvb_expr in

  let leading, inside, trailing = partition_by_loc comments pattern_loc in

  (* everything before start of pattern can only be leading on the pattern:
   *   let |* before *| a = 1 *)
  attach t.leading pattern_loc leading;
  walk_pattern vb.Parsetree.pvb_pat t inside;
  let after_pat, surrounding_expr =
    partition_adjacent_trailing pattern_loc trailing
  in
  attach t.trailing pattern_loc after_pat;
  let before_expr, inside_expr, after_expr =
    partition_by_loc surrounding_expr expr_loc
  in
  if is_block_expr expr then
    walk_expression expr t (List.concat [before_expr; inside_expr; after_expr])
  else (
    attach t.leading expr_loc before_expr;
    walk_expression expr t inside_expr;
    attach t.trailing expr_loc after_expr)

and walk_expression expr t comments =
  let open Location in
  match expr.Parsetree.pexp_desc with
  | _ when comments = [] -> ()
  | Pexp_constant _ ->
    let leading, trailing = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_ident longident ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pexp_let
      ( _recFlag,
        value_bindings,
        {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, None)} ) ->
    walk_value_bindings value_bindings t comments
  | Pexp_let (_recFlag, value_bindings, expr2) ->
    let comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun n ->
          if n.Parsetree.pvb_pat.ppat_loc.loc_ghost then n.pvb_expr.pexp_loc
          else n.Parsetree.pvb_loc)
        ~walk_node:walk_value_binding ~newline_delimited:true value_bindings t
        comments
    in
    if is_block_expr expr2 then walk_expression expr2 t comments
    else
      let leading, inside, trailing =
        partition_by_loc comments expr2.pexp_loc
      in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_sequence (expr1, expr2) ->
    let leading, inside, trailing = partition_by_loc comments expr1.pexp_loc in
    let comments =
      if is_block_expr expr1 then (
        let after_expr, comments =
          partition_by_on_same_line expr1.pexp_loc trailing
        in
        walk_expression expr1 t (List.concat [leading; inside; after_expr]);
        comments)
      else (
        attach t.leading expr1.pexp_loc leading;
        walk_expression expr1 t inside;
        let after_expr, comments =
          partition_by_on_same_line expr1.pexp_loc trailing
        in
        attach t.trailing expr1.pexp_loc after_expr;
        comments)
    in
    if is_block_expr expr2 then walk_expression expr2 t comments
    else
      let leading, inside, trailing =
        partition_by_loc comments expr2.pexp_loc
      in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_open (_override, longident, expr2) ->
    let leading, comments = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading
      {expr.pexp_loc with loc_end = longident.loc.loc_end}
      leading;
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    let after_longident, rest =
      partition_by_on_same_line longident.loc trailing
    in
    attach t.trailing longident.loc after_longident;
    if is_block_expr expr2 then walk_expression expr2 t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_extension
      ( {txt = "obj"},
        PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (rows, _)}, [])}]
      ) ->
    walk_list
      (rows |> List.map (fun (li, e, _) -> ExprRecordRow (li, e)))
      t comments
  | Pexp_extension extension -> walk_extension extension t comments
  | Pexp_letexception (extension_constructor, expr2) ->
    let leading, comments = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading
      {expr.pexp_loc with loc_end = extension_constructor.pext_loc.loc_end}
      leading;
    let leading, inside, trailing =
      partition_by_loc comments extension_constructor.pext_loc
    in
    attach t.leading extension_constructor.pext_loc leading;
    walk_extension_constructor extension_constructor t inside;
    let after_ext_constr, rest =
      partition_by_on_same_line extension_constructor.pext_loc trailing
    in
    attach t.trailing extension_constructor.pext_loc after_ext_constr;
    if is_block_expr expr2 then walk_expression expr2 t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_letmodule (string_loc, mod_expr, expr2) ->
    let leading, comments = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading
      {expr.pexp_loc with loc_end = mod_expr.pmod_loc.loc_end}
      leading;
    let leading, trailing =
      partition_leading_trailing comments string_loc.loc
    in
    attach t.leading string_loc.loc leading;
    let after_string, rest =
      partition_adjacent_trailing string_loc.loc trailing
    in
    attach t.trailing string_loc.loc after_string;
    let before_mod_expr, inside_mod_expr, after_mod_expr =
      partition_by_loc rest mod_expr.pmod_loc
    in
    attach t.leading mod_expr.pmod_loc before_mod_expr;
    walk_module_expr mod_expr t inside_mod_expr;
    let after_mod_expr, rest =
      partition_by_on_same_line mod_expr.pmod_loc after_mod_expr
    in
    attach t.trailing mod_expr.pmod_loc after_mod_expr;
    if is_block_expr expr2 then walk_expression expr2 t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_assert expr | Pexp_lazy expr ->
    if is_block_expr expr then walk_expression expr t comments
    else
      let leading, inside, trailing = partition_by_loc comments expr.pexp_loc in
      attach t.leading expr.pexp_loc leading;
      walk_expression expr t inside;
      attach t.trailing expr.pexp_loc trailing
  | Pexp_coerce (expr, (), typexpr) ->
    let leading, inside, trailing = partition_by_loc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    walk_expression expr t inside;
    let after_expr, rest = partition_adjacent_trailing expr.pexp_loc trailing in
    attach t.trailing expr.pexp_loc after_expr;
    let leading, inside, trailing = partition_by_loc rest typexpr.ptyp_loc in
    attach t.leading typexpr.ptyp_loc leading;
    walk_core_type typexpr t inside;
    attach t.trailing typexpr.ptyp_loc trailing
  | Pexp_constraint (expr, typexpr) ->
    let leading, inside, trailing = partition_by_loc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    walk_expression expr t inside;
    let after_expr, rest = partition_adjacent_trailing expr.pexp_loc trailing in
    attach t.trailing expr.pexp_loc after_expr;
    let leading, inside, trailing = partition_by_loc rest typexpr.ptyp_loc in
    attach t.leading typexpr.ptyp_loc leading;
    walk_core_type typexpr t inside;
    attach t.trailing typexpr.ptyp_loc trailing
  | Pexp_tuple []
  | Pexp_array []
  | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
    attach t.inside expr.pexp_loc comments
  | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
    walk_list
      (collect_list_exprs [] expr |> List.map (fun e -> Expression e))
      t comments
  | Pexp_construct (longident, args) -> (
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    match args with
    | Some expr ->
      let after_longident, rest =
        partition_adjacent_trailing longident.loc trailing
      in
      attach t.trailing longident.loc after_longident;
      walk_expression expr t rest
    | None -> attach t.trailing longident.loc trailing)
  | Pexp_variant (_label, None) -> ()
  | Pexp_variant (_label, Some expr) -> walk_expression expr t comments
  | Pexp_array exprs | Pexp_tuple exprs ->
    walk_list (exprs |> List.map (fun e -> Expression e)) t comments
  | Pexp_record (rows, spread_expr) ->
    if rows = [] then attach t.inside expr.pexp_loc comments
    else
      let comments =
        match spread_expr with
        | None -> comments
        | Some expr ->
          let leading, inside, trailing =
            partition_by_loc comments expr.pexp_loc
          in
          attach t.leading expr.pexp_loc leading;
          walk_expression expr t inside;
          let after_expr, rest =
            partition_adjacent_trailing expr.pexp_loc trailing
          in
          attach t.trailing expr.pexp_loc after_expr;
          rest
      in
      walk_list
        (rows |> List.map (fun (li, e, _) -> ExprRecordRow (li, e)))
        t comments
  | Pexp_field (expr, longident) ->
    let leading, inside, trailing = partition_by_loc comments expr.pexp_loc in
    let trailing =
      if is_block_expr expr then (
        let after_expr, rest =
          partition_adjacent_trailing expr.pexp_loc trailing
        in
        walk_expression expr t (List.concat [leading; inside; after_expr]);
        rest)
      else (
        attach t.leading expr.pexp_loc leading;
        walk_expression expr t inside;
        trailing)
    in
    let after_expr, rest = partition_adjacent_trailing expr.pexp_loc trailing in
    attach t.trailing expr.pexp_loc after_expr;
    let leading, trailing = partition_leading_trailing rest longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pexp_setfield (expr1, longident, expr2) ->
    let leading, inside, trailing = partition_by_loc comments expr1.pexp_loc in
    let rest =
      if is_block_expr expr1 then (
        let after_expr, rest =
          partition_adjacent_trailing expr1.pexp_loc trailing
        in
        walk_expression expr1 t (List.concat [leading; inside; after_expr]);
        rest)
      else
        let after_expr, rest =
          partition_adjacent_trailing expr1.pexp_loc trailing
        in
        attach t.leading expr1.pexp_loc leading;
        walk_expression expr1 t inside;
        attach t.trailing expr1.pexp_loc after_expr;
        rest
    in
    let before_longident, after_longident =
      partition_leading_trailing rest longident.loc
    in
    attach t.leading longident.loc before_longident;
    let after_longident, rest =
      partition_adjacent_trailing longident.loc after_longident
    in
    attach t.trailing longident.loc after_longident;
    if is_block_expr expr2 then walk_expression expr2 t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_ifthenelse (if_expr, then_expr, else_expr) -> (
    let leading, rest = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    let leading, inside, trailing = partition_by_loc rest if_expr.pexp_loc in
    let comments =
      if is_block_expr if_expr then (
        let after_expr, comments =
          partition_adjacent_trailing if_expr.pexp_loc trailing
        in
        walk_expression if_expr t (List.concat [leading; inside; after_expr]);
        comments)
      else (
        attach t.leading if_expr.pexp_loc leading;
        walk_expression if_expr t inside;
        let after_expr, comments =
          partition_adjacent_trailing if_expr.pexp_loc trailing
        in
        attach t.trailing if_expr.pexp_loc after_expr;
        comments)
    in
    let leading, inside, trailing =
      partition_by_loc comments then_expr.pexp_loc
    in
    let comments =
      if is_block_expr then_expr then (
        let after_expr, trailing =
          partition_adjacent_trailing then_expr.pexp_loc trailing
        in
        walk_expression then_expr t (List.concat [leading; inside; after_expr]);
        trailing)
      else (
        attach t.leading then_expr.pexp_loc leading;
        walk_expression then_expr t inside;
        let after_expr, comments =
          partition_adjacent_trailing then_expr.pexp_loc trailing
        in
        attach t.trailing then_expr.pexp_loc after_expr;
        comments)
    in
    match else_expr with
    | None -> ()
    | Some expr ->
      if is_block_expr expr || is_if_then_else_expr expr then
        walk_expression expr t comments
      else
        let leading, inside, trailing =
          partition_by_loc comments expr.pexp_loc
        in
        attach t.leading expr.pexp_loc leading;
        walk_expression expr t inside;
        attach t.trailing expr.pexp_loc trailing)
  | Pexp_while (expr1, expr2) ->
    let leading, inside, trailing = partition_by_loc comments expr1.pexp_loc in
    let rest =
      if is_block_expr expr1 then (
        let after_expr, rest =
          partition_adjacent_trailing expr1.pexp_loc trailing
        in
        walk_expression expr1 t (List.concat [leading; inside; after_expr]);
        rest)
      else (
        attach t.leading expr1.pexp_loc leading;
        walk_expression expr1 t inside;
        let after_expr, rest =
          partition_adjacent_trailing expr1.pexp_loc trailing
        in
        attach t.trailing expr1.pexp_loc after_expr;
        rest)
    in
    if is_block_expr expr2 then walk_expression expr2 t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walk_expression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_for (pat, expr1, expr2, _, expr3) ->
    let leading, inside, trailing = partition_by_loc comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walk_pattern pat t inside;
    let after_pat, rest = partition_adjacent_trailing pat.ppat_loc trailing in
    attach t.trailing pat.ppat_loc after_pat;
    let leading, inside, trailing = partition_by_loc rest expr1.pexp_loc in
    attach t.leading expr1.pexp_loc leading;
    walk_expression expr1 t inside;
    let after_expr, rest =
      partition_adjacent_trailing expr1.pexp_loc trailing
    in
    attach t.trailing expr1.pexp_loc after_expr;
    let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walk_expression expr2 t inside;
    let after_expr, rest =
      partition_adjacent_trailing expr2.pexp_loc trailing
    in
    attach t.trailing expr2.pexp_loc after_expr;
    if is_block_expr expr3 then walk_expression expr3 t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr3.pexp_loc in
      attach t.leading expr3.pexp_loc leading;
      walk_expression expr3 t inside;
      attach t.trailing expr3.pexp_loc trailing
  | Pexp_pack mod_expr ->
    let before, inside, after = partition_by_loc comments mod_expr.pmod_loc in
    attach t.leading mod_expr.pmod_loc before;
    walk_module_expr mod_expr t inside;
    attach t.trailing mod_expr.pmod_loc after
  | Pexp_match (expr1, [case; else_branch])
    when Res_parsetree_viewer.has_if_let_attribute expr.pexp_attributes ->
    let before, inside, after =
      partition_by_loc comments case.pc_lhs.ppat_loc
    in
    attach t.leading case.pc_lhs.ppat_loc before;
    walk_pattern case.pc_lhs t inside;
    let after_pat, rest =
      partition_adjacent_trailing case.pc_lhs.ppat_loc after
    in
    attach t.trailing case.pc_lhs.ppat_loc after_pat;
    let before, inside, after = partition_by_loc rest expr1.pexp_loc in
    attach t.leading expr1.pexp_loc before;
    walk_expression expr1 t inside;
    let after_expr, rest = partition_adjacent_trailing expr1.pexp_loc after in
    attach t.trailing expr1.pexp_loc after_expr;
    let before, inside, after = partition_by_loc rest case.pc_rhs.pexp_loc in
    let after =
      if is_block_expr case.pc_rhs then (
        let after_expr, rest =
          partition_adjacent_trailing case.pc_rhs.pexp_loc after
        in
        walk_expression case.pc_rhs t (List.concat [before; inside; after_expr]);
        rest)
      else (
        attach t.leading case.pc_rhs.pexp_loc before;
        walk_expression case.pc_rhs t inside;
        after)
    in
    let after_expr, rest =
      partition_adjacent_trailing case.pc_rhs.pexp_loc after
    in
    attach t.trailing case.pc_rhs.pexp_loc after_expr;
    let before, inside, after =
      partition_by_loc rest else_branch.pc_rhs.pexp_loc
    in
    let after =
      if is_block_expr else_branch.pc_rhs then (
        let after_expr, rest =
          partition_adjacent_trailing else_branch.pc_rhs.pexp_loc after
        in
        walk_expression else_branch.pc_rhs t
          (List.concat [before; inside; after_expr]);
        rest)
      else (
        attach t.leading else_branch.pc_rhs.pexp_loc before;
        walk_expression else_branch.pc_rhs t inside;
        after)
    in
    attach t.trailing else_branch.pc_rhs.pexp_loc after
  | Pexp_match (expr, cases) | Pexp_try (expr, cases) ->
    let before, inside, after = partition_by_loc comments expr.pexp_loc in
    let after =
      if is_block_expr expr then (
        let after_expr, rest =
          partition_adjacent_trailing expr.pexp_loc after
        in
        walk_expression expr t (List.concat [before; inside; after_expr]);
        rest)
      else (
        attach t.leading expr.pexp_loc before;
        walk_expression expr t inside;
        after)
    in
    let after_expr, rest = partition_adjacent_trailing expr.pexp_loc after in
    attach t.trailing expr.pexp_loc after_expr;
    walk_list (cases |> List.map (fun case -> Case case)) t rest
    (* unary expression: todo use parsetreeviewer *)
  | Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident
                {
                  txt =
                    Longident.Lident ("~+" | "~+." | "~-" | "~-." | "not" | "!");
                };
          };
        args = [(Nolabel, arg_expr)];
      } ->
    let before, inside, after = partition_by_loc comments arg_expr.pexp_loc in
    attach t.leading arg_expr.pexp_loc before;
    walk_expression arg_expr t inside;
    attach t.trailing arg_expr.pexp_loc after
  (* binary expression *)
  | Pexp_apply
      {
        funct =
          {
            pexp_desc =
              Pexp_ident
                {
                  txt =
                    Longident.Lident
                      ( ":=" | "||" | "&&" | "==" | "===" | "<" | ">" | "!="
                      | "!==" | "<=" | ">=" | "|>" | "+" | "+." | "-" | "-."
                      | "++" | "^" | "*" | "*." | "/" | "/." | "**" | "->"
                      | "<>" );
                };
          };
        args = [(Nolabel, operand1); (Nolabel, operand2)];
      } ->
    let before, inside, after = partition_by_loc comments operand1.pexp_loc in
    attach t.leading operand1.pexp_loc before;
    walk_expression operand1 t inside;
    let after_operand1, rest =
      partition_adjacent_trailing operand1.pexp_loc after
    in
    attach t.trailing operand1.pexp_loc after_operand1;
    let before, inside, after = partition_by_loc rest operand2.pexp_loc in
    attach t.leading operand2.pexp_loc before;
    walk_expression operand2 t inside;
    (* (List.concat [inside; after]); *)
    attach t.trailing operand2.pexp_loc after
  | Pexp_apply
      {
        funct =
          {
            pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")};
          };
        args = [(Nolabel, parent_expr); (Nolabel, member_expr)];
      } ->
    walk_list [Expression parent_expr; Expression member_expr] t comments
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
    walk_list
      [Expression parent_expr; Expression member_expr; Expression target_expr]
      t comments
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
    walk_list [Expression key_values] t comments
  | Pexp_apply {funct = call_expr; args = arguments} ->
    let before, inside, after = partition_by_loc comments call_expr.pexp_loc in
    let after =
      if is_block_expr call_expr then (
        let after_expr, rest =
          partition_adjacent_trailing call_expr.pexp_loc after
        in
        walk_expression call_expr t (List.concat [before; inside; after_expr]);
        rest)
      else (
        attach t.leading call_expr.pexp_loc before;
        walk_expression call_expr t inside;
        after)
    in
    let after_expr, rest =
      partition_adjacent_trailing call_expr.pexp_loc after
    in
    attach t.trailing call_expr.pexp_loc after_expr;
    walk_list
      (arguments
      |> List.map (fun (lbl, expr) ->
             let loc =
               match lbl with
               | Asttypes.Labelled {loc} | Optional {loc} ->
                 {loc with loc_end = expr.Parsetree.pexp_loc.loc_end}
               | _ -> expr.pexp_loc
             in
             ExprArgument {expr; loc}))
      t rest
  | Pexp_fun _ | Pexp_newtype _ -> (
    let _, parameters, return_expr = fun_expr expr in
    let comments =
      visit_list_but_continue_with_remaining_comments ~newline_delimited:false
        ~walk_node:walk_expr_parameter
        ~get_loc:(fun (_attrs, argLbl, expr_opt, pattern) ->
          let label_loc = Asttypes.get_lbl_loc argLbl in
          let open Parsetree in
          let start_pos =
            if label_loc <> Location.none then label_loc.loc_start
            else pattern.ppat_loc.loc_start
          in
          match expr_opt with
          | None -> {pattern.ppat_loc with loc_start = start_pos}
          | Some expr ->
            {
              pattern.ppat_loc with
              loc_start = start_pos;
              loc_end = expr.pexp_loc.loc_end;
            })
        parameters t comments
    in
    match return_expr.pexp_desc with
    | Pexp_constraint (expr, typ)
      when expr.pexp_loc.loc_start.pos_cnum >= typ.ptyp_loc.loc_end.pos_cnum ->
      let leading, inside, trailing = partition_by_loc comments typ.ptyp_loc in
      attach t.leading typ.ptyp_loc leading;
      walk_core_type typ t inside;
      let after_typ, comments =
        partition_adjacent_trailing typ.ptyp_loc trailing
      in
      attach t.trailing typ.ptyp_loc after_typ;
      if is_block_expr expr then walk_expression expr t comments
      else
        let leading, inside, trailing =
          partition_by_loc comments expr.pexp_loc
        in
        attach t.leading expr.pexp_loc leading;
        walk_expression expr t inside;
        attach t.trailing expr.pexp_loc trailing
    | _ ->
      if is_block_expr return_expr then walk_expression return_expr t comments
      else
        let leading, inside, trailing =
          partition_by_loc comments return_expr.pexp_loc
        in
        attach t.leading return_expr.pexp_loc leading;
        walk_expression return_expr t inside;
        attach t.trailing return_expr.pexp_loc trailing)
  | Pexp_jsx_element
      (Jsx_fragment
         {
           jsx_fragment_opening = opening_greater_than;
           jsx_fragment_children = children;
           jsx_fragment_closing = _closing_lesser_than;
         }) ->
    let opening_token = {expr.pexp_loc with loc_end = opening_greater_than} in
    let on_same_line, rest = partition_by_on_same_line opening_token comments in
    attach t.trailing opening_token on_same_line;
    let exprs =
      match children with
      | Parsetree.JSXChildrenSpreading e -> [e]
      | Parsetree.JSXChildrenItems xs -> xs
    in
    let xs = exprs |> List.map (fun e -> Expression e) in
    walk_list xs t rest
  | Pexp_jsx_element
      (Jsx_unary_element
         {
           jsx_unary_element_tag_name = tag_name;
           jsx_unary_element_props = props;
         }) -> (
    let closing_token_loc =
      ParsetreeViewer.unary_element_closing_token expr.pexp_loc
    in

    let after_opening_tag_name, rest =
      (* Either the first prop or the closing /> token *)
      let next_token =
        match props with
        | [] -> closing_token_loc
        | head :: _ -> ParsetreeViewer.get_jsx_prop_loc head
      in
      partition_adjacent_trailing_before_next_token_on_same_line tag_name.loc
        next_token comments
    in

    (* Only attach comments to the element name if they are on the same line *)
    attach t.trailing tag_name.loc after_opening_tag_name;
    match props with
    | [] ->
      let before_closing_token, _rest =
        partition_leading_trailing rest closing_token_loc
      in
      (* attach comments to the closing /> token *)
      attach t.leading closing_token_loc before_closing_token
      (* the _rest comments are going to be attached after the entire expression,
         dealt with in the parent node. *)
    | props ->
      let comments_for_props, _rest =
        partition_leading_trailing rest closing_token_loc
      in
      let prop_nodes = List.map (fun prop -> JsxProp prop) props in
      walk_list prop_nodes t comments_for_props)
  | Pexp_jsx_element
      (Jsx_container_element
         {
           jsx_container_element_tag_name_start = tag_name_start;
           jsx_container_element_props = props;
           jsx_container_element_opening_tag_end = opening_greater_than;
           jsx_container_element_children = children;
           jsx_container_element_closing_tag = closing_tag;
         }) -> (
    let opening_greater_than_loc =
      {
        loc_start = opening_greater_than;
        loc_end = opening_greater_than;
        loc_ghost = false;
      }
    in
    let after_opening_tag_name, rest =
      (* Either the first prop or the closing > token *)
      let next_token =
        match props with
        | [] -> opening_greater_than_loc
        | head :: _ -> ParsetreeViewer.get_jsx_prop_loc head
      in
      partition_adjacent_trailing_before_next_token_on_same_line
        tag_name_start.loc next_token comments
    in
    (* Only attach comments to the element name if they are on the same line *)
    attach t.trailing tag_name_start.loc after_opening_tag_name;
    let rest =
      match props with
      | [] ->
        let before_greater_than, rest =
          partition_leading_trailing rest opening_greater_than_loc
        in
        (* attach comments to the closing > token *)
        attach t.leading opening_greater_than_loc before_greater_than;
        rest
      | props ->
        let comments_for_props, rest =
          partition_leading_trailing rest opening_greater_than_loc
        in
        let prop_nodes = List.map (fun prop -> JsxProp prop) props in
        walk_list prop_nodes t comments_for_props;
        rest
    in

    (* comments after '>' on the same line should be attached to '>' *)
    let after_opening_greater_than, rest =
      partition_by_on_same_line opening_greater_than_loc rest
    in
    attach t.trailing opening_greater_than_loc after_opening_greater_than;

    let comments_for_children, _rest =
      match closing_tag with
      | None -> (rest, [])
      | Some closing_tag ->
        let closing_tag_loc =
          ParsetreeViewer.container_element_closing_tag_loc closing_tag
        in
        partition_leading_trailing rest closing_tag_loc
    in
    match children with
    | Parsetree.JSXChildrenItems [] -> (
      (* attach all comments to the closing tag if there are no children *)
      match closing_tag with
      | None ->
        (* if there is no closing tag, the comments will attached after the expression *)
        ()
      | Some closing_tag ->
        let closing_tag_loc =
          ParsetreeViewer.container_element_closing_tag_loc closing_tag
        in
        if
          opening_greater_than_loc.loc_end.pos_lnum
          < closing_tag_loc.loc_start.pos_lnum + 1
        then (
          (* In this case, there are no children but there are comments between the opening and closing tag,
             We can attach these the inside table, to easily print them later as indented comments
             For example:
             <div>
                // comment 1
                // comment 2
            </div>
          *)
          let inside_comments, leading_for_closing_tag =
            partition_between_lines opening_greater_than_loc.loc_end.pos_lnum
              closing_tag_loc.loc_start.pos_lnum comments_for_children
          in
          attach t.inside expr.pexp_loc inside_comments;
          attach t.leading closing_tag_loc leading_for_closing_tag)
        else
          (* if the closing tag is on the same line, attach comments to the opening tag *)
          attach t.leading closing_tag_loc comments_for_children)
    | children ->
      let children_nodes =
        match children with
        | Parsetree.JSXChildrenSpreading e -> [Expression e]
        | Parsetree.JSXChildrenItems xs -> List.map (fun e -> Expression e) xs
      in

      walk_list children_nodes t comments_for_children
    (* It is less likely that there are comments inside the closing tag, 
       so we don't process them right now,
       if you ever need this, feel free to update process _rest. 
       Comments after the closing tag will already be taking into account by the parent node. *)
    )
  | Pexp_await expr -> walk_expression expr t comments
  | Pexp_send _ -> ()

and walk_expr_parameter (_attrs, _argLbl, expr_opt, pattern) t comments =
  let leading, inside, trailing = partition_by_loc comments pattern.ppat_loc in
  attach t.leading pattern.ppat_loc leading;
  walk_pattern pattern t inside;
  match expr_opt with
  | Some expr ->
    let _afterPat, rest =
      partition_adjacent_trailing pattern.ppat_loc trailing
    in
    attach t.trailing pattern.ppat_loc trailing;
    if is_block_expr expr then walk_expression expr t rest
    else
      let leading, inside, trailing = partition_by_loc rest expr.pexp_loc in
      attach t.leading expr.pexp_loc leading;
      walk_expression expr t inside;
      attach t.trailing expr.pexp_loc trailing
  | None -> attach t.trailing pattern.ppat_loc trailing

and walk_expr_argument expr loc t comments =
  let leading, trailing = partition_leading_trailing comments loc in
  attach t.leading loc leading;
  let after_label, rest = partition_adjacent_trailing loc trailing in
  attach t.trailing loc after_label;
  let before, inside, after = partition_by_loc rest expr.pexp_loc in
  attach t.leading expr.pexp_loc before;
  walk_expression expr t inside;
  attach t.trailing expr.pexp_loc after

and walk_case (case : Parsetree.case) t comments =
  let before, inside, after = partition_by_loc comments case.pc_lhs.ppat_loc in
  (* cases don't have a location on their own, leading comments should go
   * after the bar on the pattern *)
  walk_pattern case.pc_lhs t (List.concat [before; inside]);
  let after_pat, rest =
    partition_adjacent_trailing case.pc_lhs.ppat_loc after
  in
  attach t.trailing case.pc_lhs.ppat_loc after_pat;
  let comments =
    match case.pc_guard with
    | Some expr ->
      let before, inside, after = partition_by_loc rest expr.pexp_loc in
      let after_expr, rest = partition_adjacent_trailing expr.pexp_loc after in
      if is_block_expr expr then
        walk_expression expr t (List.concat [before; inside; after_expr])
      else (
        attach t.leading expr.pexp_loc before;
        walk_expression expr t inside;
        attach t.trailing expr.pexp_loc after_expr);
      rest
    | None -> rest
  in
  if is_block_expr case.pc_rhs then walk_expression case.pc_rhs t comments
  else
    let before, inside, after =
      partition_by_loc comments case.pc_rhs.pexp_loc
    in
    attach t.leading case.pc_rhs.pexp_loc before;
    walk_expression case.pc_rhs t inside;
    attach t.trailing case.pc_rhs.pexp_loc after

and walk_expr_record_row (longident, expr) t comments =
  let before_longident, after_longident =
    partition_leading_trailing comments longident.loc
  in
  attach t.leading longident.loc before_longident;
  let after_longident, rest =
    partition_adjacent_trailing longident.loc after_longident
  in
  attach t.trailing longident.loc after_longident;
  let leading, inside, trailing = partition_by_loc rest expr.pexp_loc in
  attach t.leading expr.pexp_loc leading;
  walk_expression expr t inside;
  attach t.trailing expr.pexp_loc trailing

and walk_extension_constructor ext_constr t comments =
  let leading, trailing =
    partition_leading_trailing comments ext_constr.pext_name.loc
  in
  attach t.leading ext_constr.pext_name.loc leading;
  let after_name, rest =
    partition_adjacent_trailing ext_constr.pext_name.loc trailing
  in
  attach t.trailing ext_constr.pext_name.loc after_name;
  walk_extension_constructor_kind ext_constr.pext_kind t rest

and walk_extension_constructor_kind kind t comments =
  match kind with
  | Pext_rebind longident ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pext_decl (constructor_arguments, maybe_typ_expr) -> (
    let rest = walk_constructor_arguments constructor_arguments t comments in
    match maybe_typ_expr with
    | None -> ()
    | Some typexpr ->
      let before, inside, after = partition_by_loc rest typexpr.ptyp_loc in
      attach t.leading typexpr.ptyp_loc before;
      walk_core_type typexpr t inside;
      attach t.trailing typexpr.ptyp_loc after)

and walk_module_expr mod_expr t comments =
  match mod_expr.pmod_desc with
  | Pmod_ident longident ->
    let before, after = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc before;
    attach t.trailing longident.loc after
  | Pmod_structure [] -> attach t.inside mod_expr.pmod_loc comments
  | Pmod_structure structure -> walk_structure structure t comments
  | Pmod_extension extension -> walk_extension extension t comments
  | Pmod_unpack expr ->
    let before, inside, after = partition_by_loc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc before;
    walk_expression expr t inside;
    attach t.trailing expr.pexp_loc after
  | Pmod_constraint (modexpr, modtype) ->
    if modtype.pmty_loc.loc_start >= modexpr.pmod_loc.loc_end then (
      let before, inside, after = partition_by_loc comments modexpr.pmod_loc in
      attach t.leading modexpr.pmod_loc before;
      walk_module_expr modexpr t inside;
      let after, rest = partition_adjacent_trailing modexpr.pmod_loc after in
      attach t.trailing modexpr.pmod_loc after;
      let before, inside, after = partition_by_loc rest modtype.pmty_loc in
      attach t.leading modtype.pmty_loc before;
      walk_mod_type modtype t inside;
      attach t.trailing modtype.pmty_loc after)
    else
      let before, inside, after = partition_by_loc comments modtype.pmty_loc in
      attach t.leading modtype.pmty_loc before;
      walk_mod_type modtype t inside;
      let after, rest = partition_adjacent_trailing modtype.pmty_loc after in
      attach t.trailing modtype.pmty_loc after;
      let before, inside, after = partition_by_loc rest modexpr.pmod_loc in
      attach t.leading modexpr.pmod_loc before;
      walk_module_expr modexpr t inside;
      attach t.trailing modexpr.pmod_loc after
  | Pmod_apply (_callModExpr, _argModExpr) ->
    let mod_exprs = mod_expr_apply mod_expr in
    walk_list (mod_exprs |> List.map (fun me -> ModuleExpr me)) t comments
  | Pmod_functor _ -> (
    let parameters, return_mod_expr = mod_expr_functor mod_expr in
    let comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun (_, lbl, mod_type_option) ->
          match mod_type_option with
          | None -> lbl.Asttypes.loc
          | Some mod_type ->
            {lbl.loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end})
        ~walk_node:walk_mod_expr_parameter ~newline_delimited:false parameters t
        comments
    in
    match return_mod_expr.pmod_desc with
    | Pmod_constraint (mod_expr, mod_type)
      when mod_type.pmty_loc.loc_end.pos_cnum
           <= mod_expr.pmod_loc.loc_start.pos_cnum ->
      let before, inside, after = partition_by_loc comments mod_type.pmty_loc in
      attach t.leading mod_type.pmty_loc before;
      walk_mod_type mod_type t inside;
      let after, rest = partition_adjacent_trailing mod_type.pmty_loc after in
      attach t.trailing mod_type.pmty_loc after;
      let before, inside, after = partition_by_loc rest mod_expr.pmod_loc in
      attach t.leading mod_expr.pmod_loc before;
      walk_module_expr mod_expr t inside;
      attach t.trailing mod_expr.pmod_loc after
    | _ ->
      let before, inside, after =
        partition_by_loc comments return_mod_expr.pmod_loc
      in
      attach t.leading return_mod_expr.pmod_loc before;
      walk_module_expr return_mod_expr t inside;
      attach t.trailing return_mod_expr.pmod_loc after)

and walk_mod_expr_parameter parameter t comments =
  let _attrs, lbl, mod_type_option = parameter in
  let leading, trailing = partition_leading_trailing comments lbl.loc in
  attach t.leading lbl.loc leading;
  match mod_type_option with
  | None -> attach t.trailing lbl.loc trailing
  | Some mod_type ->
    let after_lbl, rest = partition_adjacent_trailing lbl.loc trailing in
    attach t.trailing lbl.loc after_lbl;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_mod_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after

and walk_mod_type mod_type t comments =
  match mod_type.pmty_desc with
  | Pmty_ident longident | Pmty_alias longident ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pmty_signature [] -> attach t.inside mod_type.pmty_loc comments
  | Pmty_signature signature -> walk_signature signature t comments
  | Pmty_extension extension -> walk_extension extension t comments
  | Pmty_typeof mod_expr ->
    let before, inside, after = partition_by_loc comments mod_expr.pmod_loc in
    attach t.leading mod_expr.pmod_loc before;
    walk_module_expr mod_expr t inside;
    attach t.trailing mod_expr.pmod_loc after
  | Pmty_with (mod_type, _withConstraints) ->
    let before, inside, after = partition_by_loc comments mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_mod_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after
    (* TODO: withConstraints*)
  | Pmty_functor _ ->
    let parameters, return_mod_type = functor_type mod_type in
    let comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun (_, lbl, mod_type_option) ->
          match mod_type_option with
          | None -> lbl.Asttypes.loc
          | Some mod_type ->
            if lbl.txt = "_" then mod_type.Parsetree.pmty_loc
            else {lbl.loc with loc_end = mod_type.Parsetree.pmty_loc.loc_end})
        ~walk_node:walk_mod_type_parameter ~newline_delimited:false parameters t
        comments
    in
    let before, inside, after =
      partition_by_loc comments return_mod_type.pmty_loc
    in
    attach t.leading return_mod_type.pmty_loc before;
    walk_mod_type return_mod_type t inside;
    attach t.trailing return_mod_type.pmty_loc after

and walk_mod_type_parameter (_, lbl, mod_type_option) t comments =
  let leading, trailing = partition_leading_trailing comments lbl.loc in
  attach t.leading lbl.loc leading;
  match mod_type_option with
  | None -> attach t.trailing lbl.loc trailing
  | Some mod_type ->
    let after_lbl, rest = partition_adjacent_trailing lbl.loc trailing in
    attach t.trailing lbl.loc after_lbl;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_mod_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after

and walk_pattern pat t comments =
  let open Location in
  match pat.Parsetree.ppat_desc with
  | _ when comments = [] -> ()
  | Ppat_alias (pat, alias) ->
    let leading, inside, trailing = partition_by_loc comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walk_pattern pat t inside;
    let after_pat, rest = partition_adjacent_trailing pat.ppat_loc trailing in
    attach t.leading pat.ppat_loc leading;
    attach t.trailing pat.ppat_loc after_pat;
    let before_alias, after_alias = partition_leading_trailing rest alias.loc in
    attach t.leading alias.loc before_alias;
    attach t.trailing alias.loc after_alias
  | Ppat_tuple []
  | Ppat_array []
  | Ppat_construct ({txt = Longident.Lident "()"}, _)
  | Ppat_construct ({txt = Longident.Lident "[]"}, _) ->
    attach t.inside pat.ppat_loc comments
  | Ppat_array patterns ->
    walk_list (patterns |> List.map (fun p -> Pattern p)) t comments
  | Ppat_tuple patterns ->
    walk_list (patterns |> List.map (fun p -> Pattern p)) t comments
  | Ppat_construct ({txt = Longident.Lident "::"}, _) ->
    walk_list
      (collect_list_patterns [] pat |> List.map (fun p -> Pattern p))
      t comments
  | Ppat_construct (constr, None) ->
    let before_constr, after_constr =
      partition_leading_trailing comments constr.loc
    in
    attach t.leading constr.loc before_constr;
    attach t.trailing constr.loc after_constr
  | Ppat_construct (constr, Some pat) ->
    let leading, trailing = partition_leading_trailing comments constr.loc in
    attach t.leading constr.loc leading;
    let after_constructor, rest =
      partition_adjacent_trailing constr.loc trailing
    in
    attach t.trailing constr.loc after_constructor;
    let leading, inside, trailing = partition_by_loc rest pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walk_pattern pat t inside;
    attach t.trailing pat.ppat_loc trailing
  | Ppat_variant (_label, None) -> ()
  | Ppat_variant (_label, Some pat) -> walk_pattern pat t comments
  | Ppat_type _ -> ()
  | Ppat_record (record_rows, _) ->
    walk_list
      (record_rows |> List.map (fun (li, p, _) -> PatternRecordRow (li, p)))
      t comments
  | Ppat_or _ ->
    walk_list
      (Res_parsetree_viewer.collect_or_pattern_chain pat
      |> List.map (fun pat -> Pattern pat))
      t comments
  | Ppat_constraint (pattern, typ) ->
    let before_pattern, inside_pattern, after_pattern =
      partition_by_loc comments pattern.ppat_loc
    in
    attach t.leading pattern.ppat_loc before_pattern;
    walk_pattern pattern t inside_pattern;
    let after_pattern, rest =
      partition_adjacent_trailing pattern.ppat_loc after_pattern
    in
    attach t.trailing pattern.ppat_loc after_pattern;
    let before_typ, inside_typ, after_typ =
      partition_by_loc rest typ.ptyp_loc
    in
    attach t.leading typ.ptyp_loc before_typ;
    walk_core_type typ t inside_typ;
    attach t.trailing typ.ptyp_loc after_typ
  | Ppat_lazy pattern | Ppat_exception pattern ->
    let leading, inside, trailing =
      partition_by_loc comments pattern.ppat_loc
    in
    attach t.leading pattern.ppat_loc leading;
    walk_pattern pattern t inside;
    attach t.trailing pattern.ppat_loc trailing
  | Ppat_unpack string_loc ->
    let leading, trailing =
      partition_leading_trailing comments string_loc.loc
    in
    attach t.leading string_loc.loc leading;
    attach t.trailing string_loc.loc trailing
  | Ppat_extension extension -> walk_extension extension t comments
  | _ -> ()

(* name: firstName *)
and walk_pattern_record_row row t comments =
  match row with
  (* punned {x}*)
  | ( {Location.txt = Longident.Lident ident; loc = longident_loc},
      {Parsetree.ppat_desc = Ppat_var {txt; _}} )
    when ident = txt ->
    let before_lbl, after_lbl =
      partition_leading_trailing comments longident_loc
    in
    attach t.leading longident_loc before_lbl;
    attach t.trailing longident_loc after_lbl
  | longident, pattern ->
    let before_lbl, after_lbl =
      partition_leading_trailing comments longident.loc
    in
    attach t.leading longident.loc before_lbl;
    let after_lbl, rest = partition_adjacent_trailing longident.loc after_lbl in
    attach t.trailing longident.loc after_lbl;
    let leading, inside, trailing = partition_by_loc rest pattern.ppat_loc in
    attach t.leading pattern.ppat_loc leading;
    walk_pattern pattern t inside;
    attach t.trailing pattern.ppat_loc trailing

and walk_row_field (row_field : Parsetree.row_field) t comments =
  match row_field with
  | Parsetree.Rtag ({loc}, _, _, _) ->
    let before, after = partition_leading_trailing comments loc in
    attach t.leading loc before;
    attach t.trailing loc after
  | Rinherit _ -> ()

and walk_core_type typ t comments =
  match typ.Parsetree.ptyp_desc with
  | _ when comments = [] -> ()
  | Ptyp_tuple typexprs ->
    walk_list (typexprs |> List.map (fun ct -> CoreType ct)) t comments
  | Ptyp_extension extension -> walk_extension extension t comments
  | Ptyp_package package_type -> walk_package_type package_type t comments
  | Ptyp_alias (typexpr, _alias) ->
    let before_typ, inside_typ, after_typ =
      partition_by_loc comments typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc before_typ;
    walk_core_type typexpr t inside_typ;
    attach t.trailing typexpr.ptyp_loc after_typ
  | Ptyp_poly (strings, typexpr) ->
    let comments =
      visit_list_but_continue_with_remaining_comments
        ~get_loc:(fun n -> n.Asttypes.loc)
        ~walk_node:(fun longident t comments ->
          let before_longident, after_longident =
            partition_leading_trailing comments longident.loc
          in
          attach t.leading longident.loc before_longident;
          attach t.trailing longident.loc after_longident)
        ~newline_delimited:false strings t comments
    in
    let before_typ, inside_typ, after_typ =
      partition_by_loc comments typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc before_typ;
    walk_core_type typexpr t inside_typ;
    attach t.trailing typexpr.ptyp_loc after_typ
  | Ptyp_variant (row_fields, _, _) ->
    walk_list (row_fields |> List.map (fun rf -> RowField rf)) t comments
  | Ptyp_constr (longident, typexprs) ->
    let before_longident, _afterLongident =
      partition_leading_trailing comments longident.loc
    in
    let after_longident, rest =
      partition_adjacent_trailing longident.loc comments
    in
    attach t.leading longident.loc before_longident;
    attach t.trailing longident.loc after_longident;
    walk_list (typexprs |> List.map (fun ct -> CoreType ct)) t rest
  | Ptyp_arrow _ ->
    let _, parameters, typexpr = arrow_type typ in
    let comments = walk_type_parameters parameters t comments in
    let before_typ, inside_typ, after_typ =
      partition_by_loc comments typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc before_typ;
    walk_core_type typexpr t inside_typ;
    attach t.trailing typexpr.ptyp_loc after_typ
  | Ptyp_object (fields, _) -> walk_typ_object_fields fields t comments
  | _ -> ()

and walk_typ_object_fields fields t comments =
  walk_list (fields |> List.map (fun f -> ObjectField f)) t comments

and walk_object_field field t comments =
  match field with
  | Otag (lbl, _, typexpr) ->
    let before_lbl, after_lbl = partition_leading_trailing comments lbl.loc in
    attach t.leading lbl.loc before_lbl;
    let after_lbl, rest = partition_adjacent_trailing lbl.loc after_lbl in
    attach t.trailing lbl.loc after_lbl;
    let before_typ, inside_typ, after_typ =
      partition_by_loc rest typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc before_typ;
    walk_core_type typexpr t inside_typ;
    attach t.trailing typexpr.ptyp_loc after_typ
  | _ -> ()

and walk_type_parameters type_parameters t comments =
  visit_list_but_continue_with_remaining_comments
    ~get_loc:(fun (_, lbl, typexpr) ->
      let lbl_loc = Asttypes.get_lbl_loc lbl in
      if lbl_loc <> Location.none then
        {lbl_loc with loc_end = typexpr.Parsetree.ptyp_loc.loc_end}
      else typexpr.ptyp_loc)
    ~walk_node:walk_type_parameter ~newline_delimited:false type_parameters t
    comments

and walk_type_parameter (_attrs, _lbl, typexpr) t comments =
  let before_typ, inside_typ, after_typ =
    partition_by_loc comments typexpr.ptyp_loc
  in
  attach t.leading typexpr.ptyp_loc before_typ;
  walk_core_type typexpr t inside_typ;
  attach t.trailing typexpr.ptyp_loc after_typ

and walk_package_type package_type t comments =
  let longident, package_constraints = package_type in
  let before_longident, after_longident =
    partition_leading_trailing comments longident.loc
  in
  attach t.leading longident.loc before_longident;
  let after_longident, rest =
    partition_adjacent_trailing longident.loc after_longident
  in
  attach t.trailing longident.loc after_longident;
  walk_package_constraints package_constraints t rest

and walk_package_constraints package_constraints t comments =
  walk_list
    (package_constraints
    |> List.map (fun (li, te) -> PackageConstraint (li, te)))
    t comments

and walk_package_constraint package_constraint t comments =
  let longident, typexpr = package_constraint in
  let before_longident, after_longident =
    partition_leading_trailing comments longident.loc
  in
  attach t.leading longident.loc before_longident;
  let after_longident, rest =
    partition_adjacent_trailing longident.loc after_longident
  in
  attach t.trailing longident.loc after_longident;
  let before_typ, inside_typ, after_typ =
    partition_by_loc rest typexpr.ptyp_loc
  in
  attach t.leading typexpr.ptyp_loc before_typ;
  walk_core_type typexpr t inside_typ;
  attach t.trailing typexpr.ptyp_loc after_typ

and walk_extension extension t comments =
  let id, payload = extension in
  let before_id, after_id = partition_leading_trailing comments id.loc in
  attach t.leading id.loc before_id;
  let after_id, rest = partition_adjacent_trailing id.loc after_id in
  attach t.trailing id.loc after_id;
  walk_payload payload t rest

and walk_attribute (id, payload) t comments =
  let before_id, after_id = partition_leading_trailing comments id.loc in
  attach t.leading id.loc before_id;
  let after_id, rest = partition_adjacent_trailing id.loc after_id in
  attach t.trailing id.loc after_id;
  walk_payload payload t rest

and walk_payload payload t comments =
  match payload with
  | PStr s -> walk_structure s t comments
  | _ -> ()

and walk_jsx_prop prop t comments =
  match prop with
  | Parsetree.JSXPropPunning _ ->
    (* this is covered by walk_list, as the location for the prop is cover there. *)
    ()
  | Parsetree.JSXPropValue (name, _, value) ->
    if name.loc.loc_end.pos_lnum == value.pexp_loc.loc_start.pos_lnum then
      (* In the rare case that comments are found between name=value,
         where both are on the same line,
         we assign them to the value, and not to the name. *)
      walk_list [Expression value] t comments
    else
      (* otherwise we attach comments that come directly after the name to the name *)
      let after_name, rest = partition_by_on_same_line name.loc comments in
      attach t.trailing name.loc after_name;
      walk_list [Expression value] t rest
  | Parsetree.JSXPropSpreading (_, value) ->
    (* We assign all comments to the spreaded expression *)
    walk_list [Expression value] t comments
