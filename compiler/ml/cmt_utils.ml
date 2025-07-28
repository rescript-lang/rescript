type deprecated_used_context = FunctionCall | Reference

type deprecated_used = {
  source_loc: Location.t;
  deprecated_text: string;
  migration_template: Parsetree.expression option;
  migration_in_pipe_chain_template: Parsetree.expression option;
  context: deprecated_used_context option;
}

type cmt_extra_info = {deprecated_used: deprecated_used list}

let record_deprecated_used :
    (?deprecated_context:deprecated_used_context ->
    ?migration_template:Parsetree.expression ->
    ?migration_in_pipe_chain_template:Parsetree.expression ->
    Location.t ->
    string ->
    unit)
    ref =
  ref
    (fun
      ?deprecated_context
      ?migration_template
      ?migration_in_pipe_chain_template
      _
      _
    ->
      ignore deprecated_context;
      ignore migration_template;
      ignore migration_in_pipe_chain_template)
type action_type =
  | ApplyFunction of {function_name: Longident.t}
  | ApplyCoercion of {coerce_to_name: Longident.t}
  | RemoveSwitchCase
  | RemoveOpen
  | RemoveAwait
  | AddAwait
  | ReplaceWithVariantConstructor of {constructor_name: Longident.t}
  | ReplaceWithPolymorphicVariantConstructor of {constructor_name: string}
  | RewriteObjectToRecord
  | RewriteArrayToTuple
  | RewriteIdentToModule of {module_name: string}
  | RewriteIdent of {new_ident: Longident.t}
  | PrefixVariableWithUnderscore
  | RemoveUnusedVariable
  | RemoveUnusedType
  | RemoveUnusedModule
  | RemoveRecFlag
  | RemoveRecordSpread
  | ForceOpen

(* TODO: 
- Unused var in patterns (and aliases )*)

type cmt_action = {loc: Location.t; action: action_type; description: string}

let action_to_string = function
  | ApplyFunction {function_name} ->
    Printf.sprintf "ApplyFunction(%s)"
      (Longident.flatten function_name |> String.concat ".")
  | ApplyCoercion {coerce_to_name} ->
    Printf.sprintf "ApplyCoercion(%s)"
      (Longident.flatten coerce_to_name |> String.concat ".")
  | RemoveSwitchCase -> "RemoveSwitchCase"
  | RemoveOpen -> "RemoveOpen"
  | RemoveAwait -> "RemoveAwait"
  | AddAwait -> "AddAwait"
  | RewriteObjectToRecord -> "RewriteObjectToRecord"
  | RewriteArrayToTuple -> "RewriteArrayToTuple"
  | RewriteIdentToModule {module_name} ->
    Printf.sprintf "RewriteIdentToModule(%s)" module_name
  | PrefixVariableWithUnderscore -> "PrefixVariableWithUnderscore"
  | RemoveUnusedVariable -> "RemoveUnusedVariable"
  | RemoveUnusedType -> "RemoveUnusedType"
  | RemoveUnusedModule -> "RemoveUnusedModule"
  | ReplaceWithVariantConstructor {constructor_name} ->
    Printf.sprintf "ReplaceWithVariantConstructor(%s)"
      (constructor_name |> Longident.flatten |> String.concat ".")
  | ReplaceWithPolymorphicVariantConstructor {constructor_name} ->
    Printf.sprintf "ReplaceWithPolymorphicVariantConstructor(%s)"
      constructor_name
  | RewriteIdent {new_ident} ->
    Printf.sprintf "RewriteIdent(%s)"
      (Longident.flatten new_ident |> String.concat ".")
  | RemoveRecFlag -> "RemoveRecFlag"
  | ForceOpen -> "ForceOpen"
  | RemoveRecordSpread -> "RemoveRecordSpread"

let _add_possible_action : (cmt_action -> unit) ref = ref (fun _ -> ())
let add_possible_action action = !_add_possible_action action

let emit_possible_actions_from_warning loc w =
  match w with
  | Warnings.Unused_open _ ->
    add_possible_action {loc; action = RemoveOpen; description = "Remove open"}
  | Unused_match | Unreachable_case ->
    add_possible_action
      {loc; action = RemoveSwitchCase; description = "Remove switch case"}
  | Unused_var _ | Unused_var_strict _ | Unused_value_declaration _ ->
    add_possible_action
      {
        loc;
        action = PrefixVariableWithUnderscore;
        description = "Prefix with `_`";
      };
    add_possible_action
      {
        loc;
        action = RemoveUnusedVariable;
        description = "Remove unused variable";
      }
  | Unused_type_declaration _ ->
    add_possible_action
      {loc; action = RemoveUnusedType; description = "Remove unused type"}
  | Unused_module _ ->
    add_possible_action
      {loc; action = RemoveUnusedModule; description = "Remove unused module"}
  | Unused_rec_flag ->
    add_possible_action
      {loc; action = RemoveRecFlag; description = "Remove rec flag"}
  | Open_shadow_identifier _ | Open_shadow_label_constructor _ ->
    add_possible_action {loc; action = ForceOpen; description = "Force open"}
  | Useless_record_with ->
    add_possible_action
      {loc; action = RemoveRecordSpread; description = "Remove `...` spread"}
    (* 
    
    === TODO === 
    
    *)
  | Fragile_literal_pattern ->
    (* Use explicit pattern matching instead of literal *) ()
  | Unused_pat -> (* Remove pattern *) ()
  | Unused_argument -> (* Remove unused argument or prefix with underscore *) ()
  | Nonoptional_label _ -> (* Add `?` to make argument optional *) ()
  | Bs_toplevel_expression_unit _ ->
    (* Assign to let _ = or pipe to ignore() *) ()
  | _ -> ()

let _ =
  Warnings.emit_possible_actions_from_warning :=
    emit_possible_actions_from_warning
