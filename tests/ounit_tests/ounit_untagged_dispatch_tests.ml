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

let suites =
  __FILE__
  >::: [
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
