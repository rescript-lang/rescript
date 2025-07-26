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

type cmt_action = {loc: Location.t; action: action_type; description: string}

let _add_possible_action : (cmt_action -> unit) ref = ref (fun _ -> ())
let add_possible_action action = !_add_possible_action action
