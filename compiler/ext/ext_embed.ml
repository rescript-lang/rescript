let get_embed_tag (name : string) : string option =
  let prefix = "embed." in
  let plen = String.length prefix in
  if String.length name > plen && String.sub name 0 plen = prefix then
    Some (String.sub name plen (String.length name - plen))
  else None

