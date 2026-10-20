open OUnit

let suites =
  __FILE__
  >::: [
         ( "integer pattern constants agree with Lambda" >:: fun _ ->
           List.iter
             (fun (literal, expected) ->
               match Typecore.constant (Pconst_integer (literal, None)) with
               | Ok (Asttypes.Const_int value as typed) ->
                 assert_equal expected value;
                 assert_equal
                   (Lam_constant.Const_int
                      {i = Int32.of_int expected; comment = None})
                   (Lam_constant_convert.convert_constant
                      (Lambda.Const_base typed))
               | _ -> assert_failure ("integer literal was rejected: " ^ literal))
             [
               ("-0xFFFF_FFFF", 1);
               ("0xFFFF_FFFF", -1);
               ("2147483647", 2147483647);
               ("-2147483648", -2147483648);
             ] );
         ( "overflow parsing keeps its existing host limits" >:: fun _ ->
           let result =
             Typecore.constant (Pconst_integer ("2147483649", None))
           in
           assert_equal
             (if Sys.int_size > 32 then Ok (Asttypes.Const_int (-2147483647))
              else Error (Typecore.Literal_overflow "int"))
             result;
           assert_equal (Error (Typecore.Literal_overflow "int"))
             (Typecore.constant
                (Pconst_integer ("999999999999999999999999999", None))) );
       ]
