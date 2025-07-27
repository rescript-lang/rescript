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
  | RewriteIdent of {new_ident: Longident.t}
  | PrefixVariableWithUnderscore
  | RemoveUnusedVariable

type cmt_action = {loc: Location.t; action: action_type; description: string}

let _add_possible_action : (cmt_action -> unit) ref = ref (fun _ -> ())
let add_possible_action action = !_add_possible_action action

let emit_possible_actions_from_warning loc w =
  match w with
  | Warnings.Unused_open _ ->
    add_possible_action {loc; action = RemoveOpen; description = "Remove open"}
  | Unused_match | Unreachable_case ->
    add_possible_action
      {loc; action = RemoveSwitchCase; description = "Remove switch case"}
  | Unused_var _ | Unused_var_strict _ ->
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
  | _ -> ()

let _ =
  Warnings.emit_possible_actions_from_warning :=
    emit_possible_actions_from_warning
