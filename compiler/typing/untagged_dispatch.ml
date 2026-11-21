open Variant_runtime

type test =
  | Equals of literal_tag
  | Typeof of block_type
  | Instance of Instance.t
  | Not_null
  | Not of test
  | And of test * test
  | Or of test * test

type 'a tree =
  | Run of 'a
  | Fallback
  | Skip
  | If of test * 'a tree * 'a tree
  | Block of 'a tree
  | Switch_value of (literal_tag * 'a clause) list * 'a tree
  | Switch_typeof of (block_type * 'a clause) list * 'a tree

and 'a clause = Falls_through of 'a option | Body of 'a tree

type 'a input = {
  facts: matching_facts;
  literal_arms: (literal_tag * 'a) list;
  payload_arms: (block_type * 'a) list;
  literals_complete: bool;
  payloads_complete: bool;
  fallback: 'a option;
}

(* The kinds [switch (typeof x)] tells apart; functions and objects are
   checked after the classes *)
let is_primitive = function
  | NumberType | StringType | BigintType | BooleanType | SymbolType -> true
  | InstanceType _ | FunctionType | ObjectType | UnknownType -> false

(* The cases of a switch: arms with one body share it, the earlier ones
   falling through to the last *)
let clauses ~same ~shares arms =
  Ext_list.flat_map
    (Ext_list.stable_group arms (fun (key1, body1) (key2, body2) ->
         shares key1 key2 && same body1 body2))
    (fun group ->
      Ext_list.map_last group (fun last (key, body) ->
          if last then (key, Body (Run body))
          else (key, Falls_through (Some body))))

(* A subclass is checked before its superclasses, otherwise the declaration
   order decides *)
let rec subclasses_first = function
  | [] -> []
  | instances ->
    let first =
      List.find
        (fun i ->
          not (List.exists (fun j -> Instance.is_subclass j ~of_:i) instances))
        instances
    in
    first :: subclasses_first (List.filter (fun i -> i <> first) instances)

(* The payload cases, after the literals: one [switch (typeof x)] over the
   primitive kinds, whose default is an [if] chain over the classes
   (subclasses first), arrays, functions and objects. Primitives never reach a
   class check, and a class instance never reaches the function or object arm
   before its own. In a complete match ([has_default] false) the last arm is
   not checked. *)
let payload_part ~same ~block_cases ~null_guard ~fallback ~has_default arms =
  match (arms, has_default) with
  | [], true -> Fallback
  | [], false -> Skip
  | [(_, body)], false -> Run body
  | _ -> (
    (* An arm with the default's body is the default *)
    let arms =
      match fallback with
      | Some fallback when has_default ->
        List.filter (fun (_, body) -> not (same fallback body)) arms
      | Some _ | None -> arms
    in
    let all_clauses =
      clauses ~same arms ~shares:(fun kind1 kind2 ->
          is_primitive kind1 && is_primitive kind2)
    in
    let primitive_clauses, other_clauses =
      List.partition (fun (kind, _) -> is_primitive kind) all_clauses
    in
    let arm block_type =
      List.find_map
        (fun (kind, clause) ->
          match clause with
          | Body tree when kind = block_type -> Some tree
          | Body _ | Falls_through _ -> None)
        other_clauses
    in
    let instances =
      subclasses_first
        (Ext_list.filter_map block_cases (function
          | InstanceType i -> Some i
          | _ -> None))
    in
    let is_class = function
      | Instance.Class _ -> true
      | Array -> false
    in
    (* [null], when it did not go to the default before *)
    let is_object =
      if null_guard then And (Typeof ObjectType, Not_null)
      else Typeof ObjectType
    in
    (* [Some body] for an arm, [None] for the default *)
    let entries =
      List.filter_map
        (fun i ->
          match arm (InstanceType i) with
          | Some body -> Some (Some (Instance i), is_class i, Some body)
          | None when has_default -> Some (Some (Instance i), is_class i, None)
          | None -> None)
        instances
      @ (match arm FunctionType with
        | Some body -> [(Some (Typeof FunctionType), false, Some body)]
        | None -> [])
      @ (match arm ObjectType with
        | Some body -> [(Some is_object, false, Some body)]
        | None -> [])
      @
      match arm UnknownType with
      | Some body -> [(None, false, Some body)]
      | None -> []
    in
    (* Checks leading to the default are needed only before an arm *)
    let rec drop_trailing_defaults = function
      | [] -> []
      | entries -> (
        match List.rev entries with
        | (_, _, None) :: rest -> drop_trailing_defaults (List.rev rest)
        | _ -> entries)
    in
    let entries = drop_trailing_defaults entries in
    (* [if (c1) default else if (c2) body else default] is
       [if (c2 && !c1) body else default], and [c1] then runs only on values
       passing [c2], never on primitives *)
    let entries =
      match List.rev entries with
      | (Some check, is_class, Some body) :: rest when has_default ->
        let rec absorb check = function
          | (Some previous, _, None) :: rest ->
            absorb (And (check, Not previous)) rest
          | rest -> List.rev ((Some check, is_class, Some body) :: rest)
        in
        absorb check rest
      | _ -> entries
    in
    (* Consecutive checks leading to the default share one [if] *)
    let rec merge_defaults = function
      | (Some c1, class1, None) :: (Some c2, class2, None) :: rest ->
        merge_defaults ((Some (Or (c1, c2)), class1 || class2, None) :: rest)
      | entry :: rest -> entry :: merge_defaults rest
      | [] -> []
    in
    let entries = merge_defaults entries in
    (* An arm may compile to no statement, so arms are counted here *)
    let has_other_arms = entries <> [] in
    (* The last arm of a complete match is not checked *)
    let entries, final =
      match List.rev entries with
      | _ when has_default -> (entries, Fallback)
      | (_, _, body) :: rest -> (List.rev rest, Option.value body ~default:Skip)
      | [] -> (entries, Skip)
    in
    let has_class_check =
      List.exists (fun (_, is_class, _) -> is_class) entries
    in
    let rec chain = function
      | [] -> final
      | (check, _, body) :: rest -> (
        let body = Option.value body ~default:Fallback in
        match check with
        | Some check -> If (check, body, chain rest)
        | None -> body)
    in
    (* The primitive kinds of the type without an arm go to the default
       directly when the chain has class checks *)
    let unmatched_primitives =
      if has_default && has_class_check then
        let kinds =
          List.filter
            (fun kind ->
              is_primitive kind
              && not (List.exists (fun (k, _) -> k = kind) primitive_clauses))
            block_cases
        in
        Ext_list.mapi kinds (fun index kind ->
            if index = List.length kinds - 1 then (kind, Body Fallback)
            else (kind, Falls_through None))
      else []
    in
    let primitive_clauses = primitive_clauses @ unmatched_primitives in
    match primitive_clauses with
    | [] -> Block (chain entries)
    | _ :: _ when not has_other_arms -> (
      (* No other arm: in a complete match the last primitive arm is not
         checked *)
      match List.rev primitive_clauses with
      | (_, last) :: rest when not has_default ->
        let default =
          match last with
          | Body tree -> tree
          | Falls_through _ -> Skip
        in
        (* The cases falling through to that arm go to the default too *)
        let rec without_fall_through = function
          | (_, Falls_through _) :: rest -> without_fall_through rest
          | rest -> rest
        in
        Switch_typeof (List.rev (without_fall_through rest), default)
      | _ -> Switch_typeof (primitive_clauses, Fallback))
    | _ :: _ -> Switch_typeof (primitive_clauses, chain entries))

type value = Literal of literal_tag | Object | Array

type 'a outcome = Arm of 'a | Default | Nothing

(* What [typeof] returns for a value, as a kind *)
let kind_of = function
  | Literal (String _) -> Some StringType
  | Literal (Int _ | Float _) -> Some NumberType
  | Literal (BigInt _) -> Some BigintType
  | Literal (Bool _) -> Some BooleanType
  | Literal Null | Object | Array -> Some ObjectType
  | Literal Undefined -> None

let select ?(on_test = ignore) tree value =
  (* JavaScript semantics, including the short circuits of [&&] and [||] *)
  let rec holds test =
    on_test test;
    match (test, value) with
    | Equals literal, Literal v -> same_literal_value v literal
    | Equals _, (Object | Array) -> false
    | Typeof kind, _ -> kind_of value = Some kind
    | Instance Array, _ -> value = Array
    | Instance (Class _), _ -> false
    | Not_null, _ -> value <> Literal Null
    | Not test, _ -> not (holds test)
    | And (test1, test2), _ -> holds test1 && holds test2
    | Or (test1, test2), _ -> holds test1 || holds test2
  in
  let rec run = function
    | Run body -> Arm body
    | Fallback -> Default
    | Skip -> Nothing
    | If (test, then_, else_) -> if holds test then run then_ else run else_
    | Block tree -> run tree
    | Switch_value (clauses, default) -> (
      match value with
      | Literal v ->
        from_clauses
          (fun literal -> same_literal_value v literal)
          clauses default
      | Object | Array -> run default)
    | Switch_typeof (clauses, default) -> (
      match kind_of value with
      | Some kind -> from_clauses (fun k -> k = kind) clauses default
      | None -> run default)
  (* A matching case runs its body, or falls through to the next one *)
  and from_clauses : 'k. ('k -> bool) -> ('k * _ clause) list -> _ =
   fun matches clauses default ->
    let rec fall = function
      | (_, Body tree) :: _ -> run tree
      | (_, Falls_through _) :: rest -> fall rest
      | [] -> run default
    in
    let rec find = function
      | [] -> run default
      | (key, _) :: _ as clauses when matches key -> fall clauses
      | _ :: rest -> find rest
    in
    find clauses
  in
  run tree

(* Two trees that run the same bodies after the same tests *)
let rec same_tree ~same tree1 tree2 =
  match (tree1, tree2) with
  | Run body1, Run body2 -> same body1 body2
  | Fallback, Fallback | Skip, Skip -> true
  | If (test1, then1, else1), If (test2, then2, else2) ->
    test1 = test2 && same_tree ~same then1 then2 && same_tree ~same else1 else2
  | Block tree1, Block tree2 -> same_tree ~same tree1 tree2
  | Switch_value (clauses1, default1), Switch_value (clauses2, default2) ->
    same_clauses ~same clauses1 clauses2 && same_tree ~same default1 default2
  | Switch_typeof (clauses1, default1), Switch_typeof (clauses2, default2) ->
    same_clauses ~same clauses1 clauses2 && same_tree ~same default1 default2
  | ( ( Run _ | Fallback | Skip | If _ | Block _ | Switch_value _
      | Switch_typeof _ ),
      _ ) ->
    false

and same_clauses : 'k. same:_ -> ('k * _) list -> ('k * _) list -> bool =
 fun ~same clauses1 clauses2 ->
  List.length clauses1 = List.length clauses2
  && List.for_all2
       (fun (key1, clause1) (key2, clause2) ->
         key1 = key2
         &&
         match (clause1, clause2) with
         | Falls_through _, Falls_through _ -> true
         | Body tree1, Body tree2 -> same_tree ~same tree1 tree2
         | (Falls_through _ | Body _), _ -> false)
       clauses1 clauses2

(* A test whose branches run the same is not run, and a case of a switch that
   runs what its default runs is left to the default, with the cases falling
   through to it *)
let rec simplify ~same tree =
  match tree with
  | Run _ | Fallback | Skip -> tree
  | If (test, then_, else_) ->
    let then_ = simplify ~same then_ and else_ = simplify ~same else_ in
    if same_tree ~same then_ else_ then then_ else If (test, then_, else_)
  | Block tree -> Block (simplify ~same tree)
  | Switch_value (clauses, default) -> (
    let default = simplify ~same default in
    match simplify_clauses ~same ~default clauses with
    | [] -> default
    | clauses -> Switch_value (clauses, default))
  | Switch_typeof (clauses, default) -> (
    let default = simplify ~same default in
    match simplify_clauses ~same ~default clauses with
    | [] -> default
    | clauses -> Switch_typeof (clauses, default))

and simplify_clauses : 'k. same:_ -> default:_ -> ('k * _) list -> ('k * _) list
    =
 fun ~same ~default clauses ->
  (* The cases falling through to a body, then the body *)
  let rec groups pending = function
    | [] -> if pending = [] then [] else [List.rev pending]
    | ((_, Falls_through _) as clause) :: rest ->
      groups (clause :: pending) rest
    | ((_, Body _) as clause) :: rest ->
      List.rev (clause :: pending) :: groups [] rest
  in
  groups [] clauses
  |> List.concat_map (fun group ->
      match List.rev group with
      | (key, Body tree) :: members ->
        let tree = simplify ~same tree in
        if same_tree ~same tree default then []
        else List.rev ((key, Body tree) :: members)
      | _ -> group)

let plan ~same
    ({
       facts = {block_types = block_cases; literal_tags = literal_cases};
       literal_arms;
       payload_arms;
       literals_complete;
       payloads_complete;
       fallback;
     } :
      'a input) =
  let literal_default = if literals_complete then None else fallback in
  let payload_has_default = (not payloads_complete) && fallback <> None in
  if literals_complete && literal_arms = [] then
    simplify ~same
      (payload_part ~same ~block_cases ~null_guard:false ~fallback
         ~has_default:payload_has_default payload_arms)
  else
    (* The literals first, compared with [===] by one switch whose default is
       the payload dispatch. A literal without an arm goes to the match's
       default, never to a payload arm *)
    let named = List.map fst literal_arms in
    let unnamed =
      List.filter (fun literal -> not (List.mem literal named)) literal_cases
    in
    (* A literal without an arm skips the switch when the payload dispatch
       takes it to the default without a class check, which could throw when
       the class does not exist. [null] passes the object check only without
       [x !== null], which is added when [null] skips. *)
    let skips_the_switch =
      match literal_default with
      | None -> fun _ -> false
      | Some _ -> (
        let payload =
          payload_part ~same ~block_cases ~null_guard:(List.mem Null unnamed)
            ~fallback ~has_default:true payload_arms
        in
        fun literal ->
          let class_check = ref false in
          let on_test = function
            | Instance (Class _) -> class_check := true
            | _ -> ()
          in
          match select ~on_test payload (Literal literal) with
          | Default -> not !class_check
          | Arm _ | Nothing -> false)
    in
    (* Values no payload arm takes are the literals without an arm: a complete
       payload dispatch checks its last arm, and these literals skip the
       switch *)
    let payload_has_default =
      payload_has_default
      || (literal_default <> None && List.exists skips_the_switch unnamed)
    in
    let null_guard =
      literal_default <> None && payload_has_default
      && List.exists
           (fun literal -> literal = Null && skips_the_switch literal)
           unnamed
    in
    let unnamed =
      match literal_default with
      | Some fallback ->
        Ext_list.filter_map unnamed (fun literal ->
            if payload_has_default && skips_the_switch literal then None
            else Some (literal, fallback))
      | None -> []
    in
    let literal_arms = literal_arms @ unnamed in
    let payload =
      simplify ~same
        (payload_part ~same ~block_cases ~null_guard ~fallback
           ~has_default:payload_has_default payload_arms)
    in
    (* A payload dispatch that runs one body unchecked takes the literals with
       that body too *)
    let literal_arms =
      match payload with
      | Run body ->
        List.filter (fun (_, other) -> not (same body other)) literal_arms
      | _ -> literal_arms
    in
    if List.length literal_arms <= 2 then
      (* An [if] per body, testing its literals with [===] *)
      let rec group = function
        | [] -> []
        | (literal, body) :: rest ->
          let same_body, others =
            List.partition (fun (_, other) -> same body other) rest
          in
          (literal :: List.map fst same_body, body) :: group others
      in
      List.fold_right
        (fun (literals, body) rest ->
          let test =
            match literals with
            | first :: others ->
              List.fold_left
                (fun test literal -> Or (test, Equals literal))
                (Equals first) others
            | [] -> assert false
          in
          If (test, Run body, rest))
        (group literal_arms) payload
    else
      simplify ~same
        (Switch_value
           (clauses ~same literal_arms ~shares:(fun _ _ -> true), payload))
