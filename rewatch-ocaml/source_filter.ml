type t = {pattern: string; regex: Re.re}

let unsupported = Error "unsupported regular expression"

(* A filter that compiles with different semantics can silently select the
   wrong source set. Keep the accepted grammar to the CLI's shared safe subset;
   the regex parser below rejects unsupported syntax outside this check. *)
let validate_compatibility pattern =
  let length = String.length pattern in
  let rec loop ~in_class ~class_start index =
    if index >= length then Ok ()
    else
      match pattern.[index] with
      | '\\' when index + 1 < length ->
        let escaped = pattern.[index + 1] in
        (match escaped with
        | '0' .. '9' | 'Q' | 'E' | 'G' | 'Z' | 'e' | 'o' -> unsupported
        | ('b' | 'W') when in_class -> unsupported
        | _ -> loop ~in_class ~class_start (index + 2))
      | '(' when not in_class && index + 2 < length
                 && pattern.[index + 1] = '?'
                 && pattern.[index + 2] = '#' ->
        unsupported
      | '[' when not in_class ->
        loop ~in_class:true ~class_start:(index + 1) (index + 1)
      | '[' when index + 1 >= length || pattern.[index + 1] <> ':' ->
        unsupported
      | ']' when in_class && index > class_start ->
        loop ~in_class:false ~class_start:0 (index + 1)
      | ('&' | '-' | '~' as operator) when in_class
                                            && index + 1 < length
                                            && pattern.[index + 1] = operator ->
        unsupported
      | '-' when in_class && index > class_start && index + 1 < length
                 && pattern.[index + 1] <> ']' ->
        let left = pattern.[index - 1] in
        let right = pattern.[index + 1] in
        let escaped_left = index >= 2 && pattern.[index - 2] = '\\' in
        if
          escaped_left || left = '\\' || right = '\\' || left > right
        then unsupported
        else loop ~in_class ~class_start (index + 1)
      | _ -> loop ~in_class ~class_start (index + 1)
  in
  loop ~in_class:false ~class_start:0 0

let compile pattern =
  match validate_compatibility pattern with
  | Error _ as error -> error
  | Ok () -> (
    match Re.Perl.re_result pattern with
    | Ok regex -> Ok {pattern; regex = Re.compile regex}
    | Error `Parse_error -> Error "invalid regular expression"
    | Error `Not_supported -> unsupported)

let pattern filter = filter.pattern
let matches_basename filter path = Re.execp filter.regex (Filename.basename path)
