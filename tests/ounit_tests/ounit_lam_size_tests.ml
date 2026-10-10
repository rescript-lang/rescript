open OUnit

let ( =~ ) = assert_equal ~printer:string_of_int

let x = Ident.create "x"

(* [Lstaticraise] counts 1 plus its arguments and a variable counts 1, so this
   lambda has exactly size [n + 1]. *)
let raise_of_vars n = Lambda.staticraise 0 (List.init n (fun _ -> Lambda.var x))

(* A loop is too big to inline, whatever its size. *)
let loop = Lambda.while_ (Lambda.var x) (Lambda.var x)

let size_upto = Lam_analysis.size_upto

let int i = Lambda.const_int i

(* A constant list of [n] elements, nested [n] levels deep *)
let constant_list n =
  let rec go i acc =
    if i < 0 then acc
    else go (i - 1) (Lambda.Const_block (Blk_tuple, [int i; acc]))
  in
  go (n - 1) (int 0)

let suites =
  __FILE__
  >::: [
         ( "returns the size below the limit and the limit otherwise"
         >:: fun _ ->
           (* the limits used by the inlining heuristics *)
           List.iter
             (fun limit ->
               for n = 0 to limit + 3 do
                 size_upto ~limit (raise_of_vars n) =~ min (n + 1) limit
               done)
             [Lam_analysis.small_inline_size; Lam_analysis.exit_inline_size; 10]
         );
         ( "adds up nested lambdas" >:: fun _ ->
           (* 1 + (1 + 2) + (1 + 1) = 6 *)
           let lam = Lambda.let_ Strict x (raise_of_vars 2) (raise_of_vars 1) in
           size_upto ~limit:10 lam =~ 6;
           size_upto ~limit:7 lam =~ 6;
           size_upto ~limit:6 lam =~ 6;
           size_upto ~limit:5 lam =~ 5 );
         ( "counts a construct that is too big to inline as the limit"
         >:: fun _ ->
           size_upto ~limit:10 loop =~ 10;
           size_upto ~limit:5
             (Lambda.staticraise 0 [Lambda.var x; loop; Lambda.var x])
           =~ 5;
           (* the old unbounded size was 1000 for these *)
           size_upto ~limit:1000 loop =~ 1000 );
         ( "counts the leaves of constants" >:: fun _ ->
           let tuple = Lambda.Const_block (Blk_tuple, [int 1; int 2; int 3]) in
           size_upto ~limit:10 (Lambda.const tuple) =~ 3;
           size_upto ~limit:10 (Lambda.const (Lambda.Const_some (int 1))) =~ 1;
           size_upto ~limit:2 (Lambda.const tuple) =~ 2 );
         ( "stops inside large constants" >:: fun _ ->
           (* would need a million nested calls without the cutoff *)
           size_upto ~limit:5 (Lambda.const (constant_list 1_000_000)) =~ 5 );
       ]
