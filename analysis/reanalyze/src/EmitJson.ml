let items = ref 0
let start () =
  items := 0;
  Printf.printf "["
let finish () = Printf.printf "\n]\n"
let emit_close () = "\n}"
let json_string text = Yojson.Safe.to_string (`String text)

let emit_item ~ppf ~name ~kind ~file ~range ~message =
  let open Format in
  items := !items + 1;
  let start_line, start_character, end_line, end_character = range in
  fprintf ppf "%s{\n" (if !items = 1 then "\n" else ",\n");
  fprintf ppf "  \"name\": %s,\n" (json_string name);
  fprintf ppf "  \"kind\": %s,\n" (json_string kind);
  fprintf ppf "  \"file\": %s,\n" (json_string file);
  fprintf ppf "  \"range\": [%d,%d,%d,%d],\n" start_line start_character
    end_line end_character;
  fprintf ppf "  \"message\": %s" (json_string message)

let loc_to_pos (loc : Location.t) =
  (loc.loc_start.pos_lnum - 1, loc.loc_start.pos_cnum - loc.loc_start.pos_bol)

let emit_annotate ~pos ~text ~action =
  let line, character = pos in
  Format.asprintf
    ",\n\
    \  \"annotate\": { \"line\": %d, \"character\": %d, \"text\": %s, \
     \"action\": %s}"
    line character (json_string text) (json_string action)
