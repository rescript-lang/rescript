/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Damien Doligez, projet Para, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* ********************************************************************* */

type key = string
type doc = string
type usage_msg = string
type anon_fun = string => unit

type rec spec =
  | Unit(unit => unit) /* Call the function with unit argument */
  | Bool(bool => unit) /* Call the function with a bool argument */
  | Set(ref<bool>) /* Set the reference to true */
  | Clear(ref<bool>) /* Set the reference to false */
  | String(string => unit) /* Call the function with a string argument */
  | Set_string(ref<string>) /* Set the reference to the string argument */
  | Int(int => unit) /* Call the function with an int argument */
  | Set_int(ref<int>) /* Set the reference to the int argument */
  | Float(float => unit) /* Call the function with a float argument */
  | Set_float(ref<float>) /* Set the reference to the float argument */
  | Tuple(list<spec>) /* Take several arguments according to the
   spec list */

  | Symbol(list<string>, string => unit)
  /* Take one of the symbols as argument and
   call the function with the symbol. */
  | Rest(string => unit) /* Stop interpreting keywords and call the
 function with each remaining argument */

exception Bad(string)
exception Help(string)

type error =
  | Unknown(string)
  | Wrong(string, string, string) /* option, actual, expected */
  | Missing(string)
  | Message(string)

exception Stop(error) /* used internally */

let rec assoc3 = (x, l) =>
  switch l {
  | list{} => throw(Not_found)
  | list{(y1, y2, y3), ...t} if y1 == x => y2
  | list{_, ...t} => assoc3(x, t)
  }

/* let make_symlist prefix sep suffix l = */
/* match l with */
/* | [] -> "<none>" */
/* | h::t -> (List.fold_left (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix */
/* ;; */

/* let print_spec buf (key, spec, doc) = */
/* if String.length doc > 0 then */
/* match spec with */
/* | Symbol (l, _) -> */
/* bprintf buf "  %s %s%s\n" key (make_symlist "{" "|" "}" l) doc */
/* | _ -> */
/* bprintf buf "  %s %s\n" key doc */
/* ;; */

let help_action = () => throw(Stop(Unknown("-help")))
let v = speclist => {
  ignore(assoc3("-help", speclist))
  list{}
}

let f = (g, speclist) => g(assoc3("-help", speclist))
let add_help = speclist => {
  let add1 = try {
    ignore(assoc3("-help", speclist))
    list{}
  } catch {
  | Not_found => list{("-help", Unit(help_action), " Display this list of options")}
  }
  and add2 = try {
    ignore(assoc3("--help", speclist))
    list{}
  } catch {
  | Not_found => list{("--help", Unit(help_action), " Display this list of options")}
  }

  \"@"(speclist, \"@"(add1, add2))
}

/* FIXME- not compatible with strict mode */
/* let f x x   =  x */
