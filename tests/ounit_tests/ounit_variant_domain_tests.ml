open Variant_runtime
open Variant_domain
let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let suites =
  __FILE__
  >::: [
         ( "number literals compare JavaScript values" >:: fun _ ->
           OUnit.assert_bool "float equals integer"
             (literal_equal (Int 1) (Float "1.0"));
           OUnit.assert_bool "negative zero"
             (literal_equal (Float "-0.0") (Int 0));
           OUnit.assert_bool "exponent spellings"
             (literal_equal (Float "1e20") (Float "100000000000000000000"));
           if Sys.word_size = 64 then
             OUnit.assert_bool "int32 narrowing"
               (literal_equal (Int (Int64.to_int 4294967297L)) (Int 1));
           OUnit.assert_bool "number and bigint differ"
             (not (literal_equal (Int 1) (BigInt "1"))) );
         ( "bigint normalization preserves arbitrary precision" >:: fun _ ->
           OUnit.assert_equal (BigInt "255")
             (normalize_literal (BigInt "0xF_F"));
           OUnit.assert_equal (BigInt "10")
             (normalize_literal (BigInt "+0b1010"));
           OUnit.assert_equal (BigInt "-63")
             (normalize_literal (BigInt "-0o77"));
           OUnit.assert_equal (BigInt "0") (normalize_literal (BigInt "-00_0"));
           OUnit.assert_equal (BigInt "18446744073709551616")
             (normalize_literal (BigInt "0x10000000000000000")) );
         ( "bigint canonicalization validates signs and prefixes" >:: fun _ ->
           OUnit.assert_equal (BigInt "-255")
             (normalize_literal (BigInt "-0xF_Fn"));
           OUnit.assert_equal (BigInt "1") (normalize_literal (BigInt "+000_1"));
           OUnit.assert_equal (BigInt "0") (normalize_literal (BigInt "-0b0n"));
           List.iter
             (fun value ->
               OUnit.assert_raises
                 (Invalid_argument "Variant_domain: invalid bigint literal")
                 (fun () -> ignore (normalize_literal (BigInt value))))
             ["++1"; "-+1"; "+-1"; "--1"; "+_+1"; "00x1"];
           List.iter
             (fun value ->
               OUnit.assert_raises
                 (Invalid_argument "Variant_domain: empty bigint literal")
                 (fun () -> ignore (normalize_literal (BigInt value))))
             [""; "_"; "+"; "-"; "n"; "0x"; "-0o"];
           OUnit.assert_raises
             (Invalid_argument "Variant_domain: invalid bigint digit")
             (fun () -> ignore (normalize_literal (BigInt "0b2"))) );
         ( "canonical domains remove redundant atoms" >:: fun _ ->
           let domain =
             normalize
               {
                 literals =
                   [String "x"; Int 1; Float "1.0"; Bool true; String "x"];
                 shapes =
                   [
                     FloatType;
                     IntType;
                     InstanceType Instance.File;
                     InstanceType Instance.Blob;
                   ];
               }
           in
           OUnit.assert_equal [String "x"; Bool true] domain.literals;
           OUnit.assert_equal
             [IntType; InstanceType Instance.Blob]
             domain.shapes;
           OUnit.assert_equal domain (normalize domain);
           OUnit.assert_equal domain (union domain domain) );
         ( "overlap categories preserve fallback priority" >:: fun _ ->
           OUnit.assert_equal
             [Literal_shape (String "x", StringType)]
             (overlaps
                (singleton_literal (String "x"))
                (singleton_shape StringType));
           OUnit.assert_equal
             [Literal_shape (String "x", StringType)]
             (overlaps
                (singleton_shape StringType)
                (singleton_literal (String "x")));
           OUnit.assert_equal [Shared_literal (Int 1)]
             (overlaps
                (singleton_literal (Int 1))
                (singleton_literal (Float "1.0")));
           OUnit.assert_equal
             [Shared_shape (IntType, IntType)]
             (overlaps (singleton_shape FloatType) (singleton_shape IntType)) );
         ( "objects and instances are separated, subclasses overlap" >:: fun _ ->
           OUnit.assert_equal []
             (overlaps
                (singleton_shape ObjectType)
                (singleton_shape (InstanceType Instance.Array)));
           OUnit.assert_bool "File inherits Blob"
             (shapes_overlap (InstanceType Instance.File)
                (InstanceType Instance.Blob));
           OUnit.assert_bool "distinct instances"
             (not
                (shapes_overlap (InstanceType Instance.Date)
                   (InstanceType Instance.RegExp)));
           OUnit.assert_bool "null is not ordinary object"
             (not (contains_literal (singleton_shape ObjectType) Null)) );
         ( "typed array family overlaps and subsumes its members" >:: fun _ ->
           let family = InstanceType Instance.TypedArray in
           let member = InstanceType Instance.Uint8Array in
           OUnit.assert_bool "family overlaps member"
             (shapes_overlap family member);
           OUnit.assert_bool "member overlaps family"
             (shapes_overlap member family);
           OUnit.assert_bool "DataView excluded"
             (not (shapes_overlap family (InstanceType Instance.DataView)));
           OUnit.assert_bool "ArrayBuffer excluded"
             (not (shapes_overlap family (InstanceType Instance.ArrayBuffer)));
           OUnit.assert_equal
             {literals = []; shapes = [InstanceType Instance.DataView; family]}
             (normalize
                {
                  literals = [];
                  shapes =
                    [
                      member;
                      family;
                      InstanceType Instance.DataView;
                      InstanceType Instance.Int16Array;
                    ];
                }) );
         ( "membership covers broad shapes" >:: fun _ ->
           let domain =
             union (singleton_literal Undefined) (singleton_shape IntType)
           in
           OUnit.assert_bool "number" (contains_literal domain (Float "1.5"));
           OUnit.assert_bool "undefined" (contains_literal domain Undefined);
           OUnit.assert_bool "not string"
             (not (contains_literal domain (String "1")));
           OUnit.assert_bool "empty" (is_empty {literals = []; shapes = []}) );
         ( "invalid domain atoms and numeric values are rejected" >:: fun _ ->
           List.iter
             (fun value ->
               OUnit.assert_raises
                 (Invalid_argument "Variant_domain: nonfinite number literal")
                 (fun () -> ignore (normalize_literal (Float value))))
             ["nan"; "infinity"; "-infinity"];
           OUnit.assert_raises
             (Invalid_argument "Variant_domain: invalid domain shape")
             (fun () -> ignore (singleton_shape UnknownType)) );
         ( "nonfinite runtime values still belong to number shapes" >:: fun _ ->
           List.iter
             (fun value ->
               let literal = Float value in
               OUnit.assert_bool "broad number"
                 (contains_literal (singleton_shape IntType) literal);
               OUnit.assert_bool "finite literal exclusion"
                 (not (contains_literal (singleton_literal (Int 1)) literal));
               OUnit.assert_equal (value <> "nan")
                 (literal_equal literal literal))
             ["nan"; "infinity"; "-infinity"] );
         ( "fallback exclusions are part of artifact compatibility" >:: fun _ ->
           let case excluded =
             Block
               (Untagged
                  {
                    tag = {name = "Case"; literal = None};
                    block_type =
                      ObjectDiscriminant
                        {
                          field_name = "kind";
                          domain = singleton_shape StringType;
                          excluded;
                        };
                  })
           in
           let empty = {literals = []; shapes = []} in
           OUnit.assert_bool "same domain"
             (object_discriminants_compatible (case empty) (case empty));
           OUnit.assert_bool "literal exclusions change meaning"
             (not
                (object_discriminants_compatible (case empty)
                   (case (singleton_literal (String "x")))));
           OUnit.assert_bool "instance exclusions change meaning"
             (not
                (object_discriminants_compatible (case empty)
                   (case (singleton_shape (InstanceType Instance.Array))))) );
       ]
