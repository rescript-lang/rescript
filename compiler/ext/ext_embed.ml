let get_embed_tag (name : string) : string option =
  let prefix = "embed." in
  let plen = String.length prefix in
  if String.length name > plen && String.sub name 0 plen = prefix then
    Some (String.sub name plen (String.length name - plen))
  else None

let is_valid_embed_id (s : string) : bool =
  let len = String.length s in
  if len = 0 then false
  else
    let lead = s.[0] in
    let is_letter = function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false
    in
    let is_ident_char = function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    if not (is_letter lead) then false
    else
      let rec loop i =
        if i >= len then true
        else if is_ident_char s.[i] then loop (i + 1)
        else false
      in
      loop 1

let invalid_id_error_message =
  "Invalid `id` for embed. Embed `id` must start with a letter, and only \
   contain letters, digits, and underscores."

let missing_id_error_message = "Embed config record must include `id: string`."

let invalid_payload_error_message =
  "Embed payload must be either a string literal or a record literal."

let normalize_tag_for_symbol (tag : string) : string =
  (* Embed tags are already validated by the parser as extension identifiers
     (attr-id with optional dot-separated segments). We only need to make the
     tag segment safe for inclusion in a single identifier by mapping '.' to
     '_'. *)
  let b = Bytes.of_string tag in
  for i = 0 to Bytes.length b - 1 do
    if Bytes.get b i = '.' then Bytes.set b i '_'
  done;
  Bytes.unsafe_to_string b
