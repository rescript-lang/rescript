let uses_crlf source =
  match String.index_opt source '\n' with
  | Some index -> index > 0 && source.[index - 1] = '\r'
  | None -> false

let to_lf source =
  if not (String.contains source '\r') then source
  else
    let length = String.length source in
    let converted = Buffer.create length in
    String.iteri
      (fun index character ->
        if
          not
            (character = '\r' && index + 1 < length && source.[index + 1] = '\n')
        then Buffer.add_char converted character)
      source;
    Buffer.contents converted

let to_crlf text =
  let converted = Buffer.create (String.length text) in
  String.iter
    (fun character ->
      if character = '\n' then Buffer.add_char converted '\r';
      Buffer.add_char converted character)
    text;
  Buffer.contents converted

let preserve source print =
  let printed = print (to_lf source) in
  if uses_crlf source then to_crlf printed else printed
