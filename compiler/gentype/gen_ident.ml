module Int_map = Map.Make (struct
  type t = int

  let compare (x : int) (y : int) = compare x y
end)

type type_vars_gen = {
  (* Generate fresh identifiers *)
  mutable type_name_map: string Int_map.t;
  mutable type_name_counter: int;
}

let create_type_vars_gen () =
  {type_name_map = Int_map.empty; type_name_counter = 0}

let js_type_name_for_anonymous_type_id ~type_vars_gen id =
  try type_vars_gen.type_name_map |> Int_map.find id
  with Not_found ->
    type_vars_gen.type_name_counter <- type_vars_gen.type_name_counter + 1;
    let name = "T" ^ string_of_int type_vars_gen.type_name_counter in
    type_vars_gen.type_name_map <-
      type_vars_gen.type_name_map |> Int_map.add id name;
    name
