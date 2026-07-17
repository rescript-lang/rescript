module E = Js_exp_make
module S = Js_stmt_make
open J

let field_ident_name i label =
  if Js_dump_property.property_key (Lit label) = label then label
  else "__rest_field" ^ string_of_int i

let ignored_ident i = Ext_ident.create ("__unused" ^ string_of_int i)

let materialize_fields source fields tail =
  match source.J.expression_desc with
  | Var (Id source_ident) ->
    let used_fields = Hashtbl.create 7 in
    let field_names =
      List.mapi (fun i field -> (field.J.record_rest_label, i)) fields
    in
    let find_field_index label = List.assoc_opt label field_names in
    let get_field_ident label =
      match Hashtbl.find_opt used_fields label with
      | Some ident -> ident
      | None ->
        let i =
          match find_field_index label with
          | Some i -> i
          | None -> assert false
        in
        let ident = Ext_ident.create (field_ident_name i label) in
        Hashtbl.add used_fields label ident;
        ident
    in
    let replace =
      {
        Js_record_map.super with
        expression =
          (fun self expr ->
            match expr.expression_desc with
            | Static_index ({expression_desc = Var (Id ident); _}, label, _)
              when Ident.same ident source_ident
                   && find_field_index label <> None ->
              E.var (get_field_ident label)
            | _ -> Js_record_map.super.expression self expr);
      }
    in
    let tail = replace.block replace tail in
    let fields =
      List.mapi
        (fun i field ->
          match field.J.record_rest_ident with
          | Some _ -> field
          | None ->
            let ident =
              match Hashtbl.find_opt used_fields field.record_rest_label with
              | Some ident -> ident
              | None -> ignored_ident i
            in
            {field with record_rest_ident = Some ident})
        fields
    in
    (fields, tail)
  | _ ->
    let fields =
      List.mapi
        (fun i field ->
          match field.J.record_rest_ident with
          | Some _ -> field
          | None -> {field with record_rest_ident = Some (ignored_ident i)})
        fields
    in
    (fields, tail)

let pass =
  let super = Js_record_map.super in
  let block (self : Js_record_map.iter) = function
    | ({
         statement_desc =
           Variable
             ({
                value =
                  Some
                    ({expression_desc = Record_rest (fields, source); _} as
                     value);
                _;
              } as variable);
         _;
       } as statement)
      :: tail ->
      let source = self.expression self source in
      let tail = self.block self tail in
      let fields, tail = materialize_fields source fields tail in
      {
        statement with
        statement_desc =
          Variable
            {
              variable with
              value =
                Some {value with expression_desc = Record_rest (fields, source)};
            };
      }
      :: tail
    | ({
         statement_desc =
           Return
             ({expression_desc = Record_rest (fields, source); _} as rest_expr);
         _;
       } as statement)
      :: tail ->
      let rest = Ext_ident.create "rest" in
      let source = self.expression self source in
      let tail = self.block self tail in
      let fields, return =
        materialize_fields source fields
          [{statement with statement_desc = Return (E.var rest)}]
      in
      S.define_variable ~kind:Strict rest
        {rest_expr with expression_desc = Record_rest (fields, source)}
      :: (return @ tail)
    | statement :: tail -> self.statement self statement :: self.block self tail
    | [] -> []
  in
  {super with block}

let program program = pass.program pass program
