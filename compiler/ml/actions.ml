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
  | RewriteArgType of {to_type: [`Labelled | `Optional | `Unlabelled]}
  | PrefixVariableWithUnderscore
  | RemoveUnusedVariable
  | RemoveUnusedType
  | RemoveUnusedModule
  | RemoveRecFlag
  | RemoveRecordSpread
  | ForceOpen
  | AssignToUnderscore
  | PipeToIgnore
  | PartiallyApplyFunction
  | InsertMissingArguments of {missing_args: Asttypes.Noloc.arg_label list}
  | ChangeRecordFieldOptional of {optional: bool}
  | UnwrapOptionMapRecordField of {field_name: Longident.t}

(* TODO: 
- Unused var in patterns (and aliases )*)

type action = {loc: Location.t; action: action_type; description: string}

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
  | AssignToUnderscore -> "AssignToUnderscore"
  | PipeToIgnore -> "PipeToIgnore"
  | RewriteArgType {to_type} -> (
    match to_type with
    | `Labelled -> "RewriteArgType(Labelled)"
    | `Optional -> "RewriteArgType(Optional)"
    | `Unlabelled -> "RewriteArgType(Unlabelled)")
  | PartiallyApplyFunction -> "PartiallyApplyFunction"
  | InsertMissingArguments {missing_args} ->
    Printf.sprintf "InsertMissingArguments(%s)"
      (missing_args
      |> List.map (fun arg ->
             match arg with
             | Asttypes.Noloc.Labelled txt -> "~" ^ txt
             | Asttypes.Noloc.Optional txt -> "?" ^ txt
             | Asttypes.Noloc.Nolabel -> "<unlabelled>")
      |> String.concat ", ")
  | ChangeRecordFieldOptional {optional} ->
    Printf.sprintf "ChangeRecordFieldOptional(%s)"
      (if optional then "true" else "false")
  | UnwrapOptionMapRecordField {field_name} ->
    Printf.sprintf "UnwrapOptionMapRecordField(%s)"
      (Longident.flatten field_name |> String.concat ".")

let _add_possible_action : (action -> unit) ref = ref (fun _ -> ())
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
  | Bs_toplevel_expression_unit _ ->
    add_possible_action
      {loc; action = PipeToIgnore; description = "Pipe to ignore()"};
    add_possible_action
      {loc; action = AssignToUnderscore; description = "Assign to let _ ="}
  | Nonoptional_label _ ->
    add_possible_action
      {
        loc;
        action = RewriteArgType {to_type = `Labelled};
        description = "Make argument optional";
      }
    (* 
    
    === TODO === 
    
    *)
  | Unused_pat -> (* Remove pattern *) ()
  | Unused_argument -> (* Remove unused argument or prefix with underscore *) ()
  | Unused_constructor _ -> (* Remove unused constructor *) ()
  | Bs_unused_attribute _ -> (* Remove unused attribute *) ()
  | _ -> ()

let _ =
  Warnings.emit_possible_actions_from_warning :=
    emit_possible_actions_from_warning
