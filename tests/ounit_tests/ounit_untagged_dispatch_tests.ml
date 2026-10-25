open OUnit

module D = Untagged_dispatch
module R = Variant_runtime

let rec test_to_string : D.test -> string = function
  | Equals literal -> "x === " ^ R.literal_value_key literal
  | Typeof block_type ->
    "typeof x === " ^ Option.value (R.typeof_name block_type) ~default:"?"
  | Instance instance -> "x instanceof " ^ R.Instance.to_string instance
  | Not_null -> "x !== null"
  | Not test -> "!(" ^ test_to_string test ^ ")"
  | And (test1, test2) ->
    "(" ^ test_to_string test1 ^ " && " ^ test_to_string test2 ^ ")"
  | Or (test1, test2) ->
    "(" ^ test_to_string test1 ^ " || " ^ test_to_string test2 ^ ")"

let rec tree_to_string : string D.tree -> string = function
  | Run body -> body
  | Fallback -> "fallback"
  | Skip -> "skip"
  | If (test, then_, else_) ->
    "if (" ^ test_to_string test ^ ") " ^ tree_to_string then_ ^ " else "
    ^ tree_to_string else_
  | Block tree -> "{" ^ tree_to_string tree ^ "}"
  | Switch_value (clauses, default) ->
    "switch (x) {"
    ^ String.concat ""
        (List.map
           (fun (literal, clause) ->
             R.literal_value_key literal ^ ": " ^ clause_to_string clause ^ "; ")
           clauses)
    ^ "default: " ^ tree_to_string default ^ "}"
  | Switch_typeof (clauses, default) ->
    "switch (typeof x) {"
    ^ String.concat ""
        (List.map
           (fun (block_type, clause) ->
             Option.value (R.typeof_name block_type) ~default:"?"
             ^ ": " ^ clause_to_string clause ^ "; ")
           clauses)
    ^ "default: " ^ tree_to_string default ^ "}"

and clause_to_string : string D.clause -> string = function
  | Falls_through _ -> "falls through"
  | Body tree -> tree_to_string tree

let class_ ?(ancestors = []) name : R.block_type =
  InstanceType (Class {name; ancestors})

(* The plan of a match with these arms on a variant with these literal and
   payload cases; [fallback] is the match's default *)
let plan ~literals ~kinds ?(fallback : string option) literal_arms payload_arms
    =
  D.plan ~same:String.equal
    {
      facts = {tag_name = None; block_types = kinds; literal_tags = literals};
      literal_arms;
      payload_arms;
      literals_complete = List.length literal_arms = List.length literals;
      payloads_complete = List.length payload_arms = List.length kinds;
      fallback;
    }

let ( =~ ) expected tree =
  OUnit.assert_equal ~printer:(fun s -> s) expected (tree_to_string tree)

(* Random variants and matches, for comparing the plan with the rules *)
let random_case state =
  let pick list = List.nth list (Random.State.int state (List.length list)) in
  let some_of list = List.filter (fun _ -> Random.State.bool state) list in
  let literals =
    some_of
      (* A literal of each kind *)
      [
        R.String "a";
        String "b";
        Int 1;
        Float "1.5";
        Bool true;
        Null;
        Undefined;
        BigInt "2";
      ]
  in
  let kinds =
    if Random.State.int state 10 = 0 then [R.UnknownType]
    else
      List.filter
        (fun kind ->
          (* A boolean payload needs a boolean that is not a literal case *)
          kind <> R.BooleanType || not (List.mem (R.Bool true) literals))
        (some_of
           [
             R.StringType;
             NumberType;
             BigintType;
             BooleanType;
             SymbolType;
             FunctionType;
             ObjectType;
             InstanceType Array;
             class_ "Date";
             class_ "Animal";
             class_ "Dog" ~ancestors:["Animal"];
           ])
  in
  let arms cases =
    List.filter_map
      (fun case ->
        if Random.State.int state 5 < 3 then Some (case, pick ["b0"; "b1"; "b2"])
        else None)
      cases
  in
  (literals, kinds, arms literals, arms kinds)

(* Rules 7 to 9: a literal case takes its arm, any other value the arm of its
   kind, and a case without an arm the default. [None] for a value of no
   case. *)
let oracle ~literals ~kinds ~literal_arms ~payload_arms (value : D.value) =
  let of_arm arms case : string D.outcome =
    match List.assoc_opt case arms with
    | Some body -> Arm body
    | None -> Default
  in
  let declared (kind : R.block_type) = List.mem kind kinds in
  let kind_case : R.block_type option =
    if declared UnknownType then Some UnknownType
    else
      match value with
      | Literal (String _) -> Some StringType
      | Literal (Int _ | Float _) -> Some NumberType
      | Literal (BigInt _) -> Some BigintType
      | Literal (Bool _) -> Some BooleanType
      | Literal (Null | Undefined) -> None
      | Object -> Some ObjectType
      | Array ->
        if declared (InstanceType Array) then Some (InstanceType Array)
        else Some ObjectType
  in
  match value with
  | Literal literal when List.exists (R.same_literal_value literal) literals ->
    Some (of_arm literal_arms literal)
  | _ -> (
    match kind_case with
    | Some kind when declared kind -> Some (of_arm payload_arms kind)
    | Some _ | None -> None)

(* One value of each case of the variant *)
let values ~literals ~kinds : D.value list =
  List.map (fun literal -> D.Literal literal) literals
  @ List.concat_map
      (fun (kind : R.block_type) : D.value list ->
        match kind with
        | StringType -> [Literal (String "fresh")]
        | NumberType -> [Literal (Int 98765)]
        | BigintType -> [Literal (BigInt "77")]
        | BooleanType -> [Literal (Bool false)]
        | ObjectType -> [Object]
        | InstanceType Array -> [Array]
        | UnknownType -> [Object; Array; Literal (String "fresh")]
        | SymbolType | FunctionType | InstanceType (Class _) -> [])
      kinds

let outcome_to_string : string D.outcome -> string = function
  | Arm body -> body
  | Default -> "default"
  | Nothing -> "nothing"

let value_to_string : D.value -> string = function
  | Literal literal -> R.literal_value_key literal
  | Object -> "object"
  | Array -> "array"

let suites =
  __FILE__
  >::: [
         ( "every plan takes the arms rules 7 to 9 give" >:: fun _ ->
           let state = Random.State.make [|8955|] in
           for _ = 1 to 3000 do
             let literals, kinds, literal_arms, payload_arms =
               random_case state
             in
             let complete =
               List.length literal_arms = List.length literals
               && List.length payload_arms = List.length kinds
             in
             let tree =
               plan ~literals ~kinds
                 ?fallback:(if complete then None else Some "default")
                 literal_arms payload_arms
             in
             List.iter
               (fun value ->
                 let outcome = D.select tree value in
                 match
                   oracle ~literals ~kinds ~literal_arms ~payload_arms value
                 with
                 | Some expected ->
                   (* A literal case leading to the default runs its body *)
                   OUnit.assert_equal
                     ~msg:(value_to_string value ^ " in " ^ tree_to_string tree)
                     ~printer:Fun.id
                     (outcome_to_string expected)
                     (outcome_to_string outcome)
                 | None -> ())
               (values ~literals ~kinds)
           done );
         ( "a literal no payload arm takes skips the switch" >:: fun _ ->
           "switch (typeof x) {number: int; default: fallback}"
           =~ plan ~literals:[String "a"; String "b"] ~kinds:[NumberType]
                ~fallback:"default" []
                [(NumberType, "int")] );
         ( "a literal a payload arm takes is tested first" >:: fun _ ->
           "if (x === string:a) a else string"
           =~ plan ~literals:[String "a"] ~kinds:[StringType]
                [(String "a", "a")]
                [(StringType, "string")] );
         ( "a skipping null adds a null check to the object arm" >:: fun _ ->
           "switch (typeof x) {string: string; default: if ((typeof x === \
            object && x !== null)) object else fallback}"
           =~ plan ~literals:[Null] ~kinds:[StringType; ObjectType]
                ~fallback:"default" []
                [(StringType, "string"); (ObjectType, "object")] );
         ( "a subclass is checked before its superclass" >:: fun _ ->
           "switch (typeof x) {string: string; default: if ((x instanceof \
            Animal && !(x instanceof Dog))) animal else fallback}"
           =~ plan ~literals:[]
                ~kinds:
                  [
                    class_ "Animal";
                    class_ "Dog" ~ancestors:["Animal"];
                    StringType;
                  ]
                ~fallback:"default" []
                [(class_ "Animal", "animal"); (StringType, "string")] );
         ( "the last arm of a complete match is not checked" >:: fun _ ->
           "{if (x instanceof Date) date else regexp}"
           =~ plan ~literals:[]
                ~kinds:[class_ "Date"; class_ "RegExp"]
                []
                [(class_ "Date", "date"); (class_ "RegExp", "regexp")] );
         ( "literals with one body share a case" >:: fun _ ->
           "switch (x) {string:b: y; string:a: falls through; string:c: x; \
            default: string}"
           =~ plan
                ~literals:[String "a"; String "b"; String "c"]
                ~kinds:[StringType]
                [(String "a", "x"); (String "b", "y"); (String "c", "x")]
                [(StringType, "string")] );
         ( "two literals with one body are one test" >:: fun _ ->
           "if ((x === number:0x1p+0 || x === number:0x1p+1)) one else number"
           =~ plan ~literals:[Int 1; Int 2] ~kinds:[NumberType]
                [(Int 1, "one"); (Int 2, "one")]
                [(NumberType, "number")] );
         ( "primitive kinds without an arm go to the default before class checks"
         >:: fun _ ->
           "switch (typeof x) {string: string; number: fallback; default: if \
            (x instanceof Date) date else fallback}"
           =~ plan ~literals:[]
                ~kinds:[StringType; NumberType; class_ "Date"]
                ~fallback:"default" []
                [(StringType, "string"); (class_ "Date", "date")] );
       ]
