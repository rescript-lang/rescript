open OUnit

let ( =~ ) = OUnit.assert_equal

let constructor_switch ~untagged ~with_block_case =
  let runtime : Variant_runtime.block_runtime =
    {tag = {name = "Color"; literal = None}; tag_name = None; untagged}
  in
  let literal : Variant_runtime.tag =
    {name = "Primary"; literal = Some (String "primary")}
  in
  let block : Variant_runtime.block =
    {runtime; block_type = (if untagged then Some StringType else None)}
  in
  let literal_action = Lambda.const (Const_string "literal") in
  let block_action = Lambda.const (Const_string "payload") in
  let default_action = Lambda.const (Const_string "default") in
  let arg =
    Lambda.const
      (Const_block
         ( Blk_constructor {name = "Color"; num_nonconst = 1; runtime},
           [Const_string "primary"] ))
  in
  let layout =
    Variant_runtime.make_layout
      ~configuration:{unboxed = untagged; tag_name = None}
      [|Constant literal; Block block|]
  in
  let sw : Lambda.lambda_switch =
    {
      sw_consts_full = true;
      sw_consts = [(Switch_constructor (Constant literal), literal_action)];
      sw_blocks_full = with_block_case;
      sw_blocks =
        (if with_block_case then
           [(Switch_constructor (Block block), block_action)]
         else []);
      sw_failaction = (if with_block_case then None else Some default_action);
      sw_dispatch = Switch_variant (Variant_runtime.matching_facts layout);
    }
  in
  let result = Lambda.switch arg sw in
  if untagged then
    match result with
    | Lswitch (actual_arg, actual_sw) ->
      arg =~ actual_arg;
      sw =~ actual_sw
    | _ -> assert_failure "untagged payload must retain runtime dispatch"
  else (if with_block_case then block_action else default_action) =~ result

let suites =
  __FILE__
  >::: [
         ( "untagged switch keeps literal dispatch" >:: fun _ ->
           constructor_switch ~untagged:true ~with_block_case:true );
         ( "untagged switch does not prematurely select default" >:: fun _ ->
           constructor_switch ~untagged:true ~with_block_case:false );
         ( "tagged switch still folds" >:: fun _ ->
           constructor_switch ~untagged:false ~with_block_case:true );
         ( "tagged switch still folds to default" >:: fun _ ->
           constructor_switch ~untagged:false ~with_block_case:false );
         ( "typed string constants" >:: fun _ ->
           Lambda.const_string "value" =~ Lambda.Const_string "value" );
         ( "compiler-generated strings normalize malformed bytes" >:: fun _ ->
           let constant = Lambda.const_string "a\xffé" in
           constant =~ Lambda.Const_string "aÿé";
           match
             Lambda.prim ~primitive:Lambda.Pstringlength
               ~args:[Lambda.const constant]
               Location.none
           with
           | Lambda.Lconst (Lambda.Const_int length) -> 3l =~ length
           | _ -> OUnit.assert_failure "expected a folded string length" );
       ]
