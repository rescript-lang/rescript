/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Pierre Chambart, OCamlPro */
/* Mark Shinwell and Leo White, Jane Street Europe */
/*  */
/* Copyright 2015--2016 OCamlPro SAS */
/* Copyright 2015--2016 Jane Street Group LLC */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

@raises(exit)
let fatal = err => {
  prerr_endline(err)
  exit(2)
}

module Make = (
  S: {
    module Key: {
      type t
      let of_string: string => t
      module Map: Map.S with type key = t
    }

    module Value: {
      type t
      let of_string: string => t
    }
  },
) => {
  type parsed = {
    base_default: S.Value.t,
    base_override: S.Key.Map.t<S.Value.t>,
    user_default: option<S.Value.t>,
    user_override: S.Key.Map.t<S.Value.t>,
  }

  let default = v => {
    base_default: v,
    base_override: S.Key.Map.empty,
    user_default: None,
    user_override: S.Key.Map.empty,
  }

  let set_base_default = (value, t) => {...t, base_default: value}

  let add_base_override = (key, value, t) => {
    ...t,
    base_override: S.Key.Map.add(key, value, t.base_override),
  }

  let reset_base_overrides = t => {...t, base_override: S.Key.Map.empty}

  let set_user_default = (value, t) => {...t, user_default: Some(value)}

  let add_user_override = (key, value, t) => {
    ...t,
    user_override: S.Key.Map.add(key, value, t.user_override),
  }

  exception Parse_failure(exn)

  @raises([Invalid_argument, Parse_failure])
  let parse_exn = (str, ~update) => {
    /* Is the removal of empty chunks really relevant here? */
    /* (It has been added to mimic the old Misc.String.split.) */
    let values = String.split_on_char(',', str) |> List.filter(\"<>"(""))
    let parsed = List.fold_left((acc, value) =>
      switch String.index(value, '=') {
      | exception Not_found =>
        switch S.Value.of_string(value) {
        | value => set_user_default(value, acc)
        | exception exn => throw(Parse_failure(exn))
        }
      | equals =>
        let key_value_pair = value
        let length = String.length(key_value_pair)
        assert (equals >= 0 && equals < length)
        if equals == 0 {
          throw(Parse_failure(Failure("Missing key in argument specification")))
        }
        let key = {
          let key = String.sub(key_value_pair, 0, equals)
          try S.Key.of_string(key) catch {
          | exn => throw(Parse_failure(exn))
          }
        }

        let value = {
          let value = String.sub(key_value_pair, equals + 1, length - equals - 1)

          try S.Value.of_string(value) catch {
          | exn => throw(Parse_failure(exn))
          }
        }

        add_user_override(key, value, acc)
      }
    , update.contents, values)

    update := parsed
  }

  @raises([Invalid_argument, exit])
  let parse = (str, help_text, update) =>
    switch parse_exn(str, ~update) {
    | () => ()
    | exception Parse_failure(exn) =>
      fatal(Printf.sprintf("%s: %s", Printexc.to_string(exn), help_text))
    }

  type parse_result =
    | Ok
    | Parse_failed(exn)

  @raises(Invalid_argument)
  let parse_no_error = (str, update) =>
    switch parse_exn(str, ~update) {
    | () => Ok
    | exception Parse_failure(exn) => Parse_failed(exn)
    }

  let get = (~key, parsed) =>
    switch S.Key.Map.find(key, parsed.user_override) {
    | value => value
    | exception Not_found =>
      switch parsed.user_default {
      | Some(value) => value
      | None =>
        switch S.Key.Map.find(key, parsed.base_override) {
        | value => value
        | exception Not_found => parsed.base_default
        }
      }
    }
}
