let hex_table =
  [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; |]
  [@ocamlformat "disable"]

let convert_decimal_to_hex ~str_decimal =
  try
    let int_num = int_of_string str_decimal in
    let c1 = Array.get hex_table (int_num lsr 4) in
    let c2 = Array.get hex_table (int_num land 15) in
    "x" ^ String.concat "" [String.make 1 c1; String.make 1 c2]
  with Invalid_argument _ | Failure _ -> str_decimal
