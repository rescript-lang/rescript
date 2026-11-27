module E = Js_exp_make
module S = Js_stmt_make
open J

(* [let name = c.name; let {name: _, ...rest} = c] reads [c.name] twice. A read
   of an excluded field in the declarations right before the rest becomes a
   binding of the rest's destructuring: [let {name, ...rest} = c]. Reads of the
   source anywhere else stay reads of the source, as it may be written in
   between. *)

(* The binding made by [statement] when it reads an excluded field of the
   rest's source and nothing else. *)
let field_binding ~source ~fields statement =
  match (statement, source.expression_desc) with
  | ( {
        statement_desc =
          Variable
            {
              ident;
              property = Strict | StrictOpt | Alias;
              value =
                Some
                  {
                    expression_desc =
                      Static_index ({expression_desc = Var read}, label, _);
                  };
            };
        comment = None;
      },
      Var source )
    when Js_op_util.same_vident read source
         && List.exists
              (fun {record_rest_label; record_rest_ident} ->
                record_rest_label = label && record_rest_ident = None)
              fields ->
    Some (ident, label)
  | _ -> None

let bind_field fields (ident, label) =
  List.map
    (fun field ->
      if field.record_rest_label = label then
        {field with record_rest_ident = Some ident}
      else field)
    fields

(* An excluded field that isn't bound gets a placeholder name, so that the rest
   object leaves it out. *)
let name_unbound_fields fields =
  List.mapi
    (fun i field ->
      match field.record_rest_ident with
      | Some _ -> field
      | None ->
        let ident = Ext_ident.create ("__unused" ^ string_of_int i) in
        {field with record_rest_ident = Some ident})
    fields

(* [reads] are the declarations before a rest, last one first. Gives the fields
   of the rest with the bindings the declarations make, and the remaining
   declarations before and after the rest: those after the first one that
   became a binding may use it. *)
let bind_fields ~source ~fields reads =
  let rec bind fields ~pending ~after = function
    | [] -> (name_unbound_fields fields, pending, after)
    | read :: reads -> (
      match field_binding ~source ~fields read with
      | Some binding ->
        bind
          (bind_field fields binding)
          ~pending:[] ~after:(pending @ after) reads
      | None -> bind fields ~pending:(read :: pending) ~after reads)
  in
  bind fields ~pending:[] ~after:[] reads

let pass =
  let super = Js_record_map.super in
  (* [reads] are the declarations before the statements, last one first, that
     only read a variable, so that a rest can move before them. *)
  let rec block (self : Js_record_map.iter) reads = function
    | ({
         statement_desc =
           Variable
             ({
                value =
                  Some
                    ({expression_desc = Record_rest (fields, source)} as value);
              } as variable);
       } as statement)
      :: tail -> (
      let source = self.expression self source in
      let fields, before, after = bind_fields ~source ~fields reads in
      let value =
        Some {value with expression_desc = Record_rest (fields, source)}
      in
      let rest =
        {statement with statement_desc = Variable {variable with value}}
      in
      match source.expression_desc with
      | Var _ ->
        (* The copy may run getters, which can write what a later copy reads:
           a later rest moves only the reads after this one. *)
        before @ (rest :: block self (List.rev after) tail)
      | _ -> before @ (rest :: after) @ block self [] tail)
    | ({
         statement_desc =
           Return
             ({expression_desc = Record_rest (fields, source)} as rest_expr);
       } as statement)
      :: tail ->
      let rest = Ext_ident.create "rest" in
      let source = self.expression self source in
      let fields, before, after = bind_fields ~source ~fields reads in
      before
      @ S.define_variable ~kind:Strict rest
          {rest_expr with expression_desc = Record_rest (fields, source)}
        :: after
      @ {statement with statement_desc = Return (E.var rest)}
        :: block self [] tail
    | ({
         statement_desc =
           Variable
             {
               property = Strict | StrictOpt | Alias;
               value =
                 Some
                   {
                     expression_desc =
                       ( Static_index ({expression_desc = Var _}, _, _)
                       | Array_index
                           ( {expression_desc = Var _},
                             {expression_desc = Number _} ) );
                   };
             };
       } as statement)
      :: tail ->
      block self (statement :: reads) tail
    | statement :: tail ->
      List.rev_append reads (self.statement self statement :: block self [] tail)
    | [] -> List.rev reads
  in
  {super with block = (fun self statements -> block self [] statements)}

let program program = pass.program pass program
