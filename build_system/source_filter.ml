type t = {pattern: string; regex: Re.re}

(* Patterns use Re's Perl syntax. Constructs Re cannot match, such as
   backreferences, are rejected rather than ignored. *)
let compile pattern =
  match Re.Perl.re_result pattern with
  | Ok regex -> Ok {pattern; regex = Re.compile regex}
  | Error `Parse_error -> Error "invalid regular expression"
  | Error `Not_supported -> Error "unsupported regular expression"

let pattern filter = filter.pattern
let matches_basename filter path =
  Re.execp filter.regex (Filename.basename path)
