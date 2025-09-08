let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

/* FIXME: paren needed here 
    {[ x##xh#= (g z )  ]}
*/
let f0 = (x: {@set "_open": int}) => {
  let old = x["_open"]
  x["_open"] = old + 1
  x["_open"]
}

let f1 = (x: {@set "_in": int}) => {
  let old = x["_in"]
  x["_in"] = old + 1
  x["_in"]
}

let f2 = (x: {@set "_MAX_LENGTH": int}) => {
  let old = x["_MAX_LENGTH"]
  x["_MAX_LENGTH"] = old + 1
  x["_MAX_LENGTH"]
}

let f3 = (x: {@set "_Capital": int}) => {
  let old = x["_Capital"]
  x["_Capital"] = old + 1
  x["_Capital"]
}

let f4 = (x: {@set "_open__": int}) => {
  let old = x["_open__"]
  x["_open__"] = old + 1
  x["_open__"]
}

let f5 = (x: {@set "open__": int}) => {
  let old = x["open__"]
  x["open__"] = old + 1
  x["open__"]
}

/* < _ : int > -> is a syntax error */

let f6 = (x: {@set "_'x": int}) => {
  let old = x["_'x"]
  x["_'x"] = old + 1
  x["_'x"]
}

let f7 = (x: {@set "_Capital__": int}) => {
  let old = x["_Capital__"]
  x["_Capital__"] = old + 1
  x["_Capital__"]
}

let f8 = (x: {@set "_MAX__": int}) => {
  let old = x["_MAX__"]
  x["_MAX__"] = old + 1
  x["_MAX__"]
}

let f9 = (x: {@set "__": int}) => {
  let old = x["__"]
  x["__"] = old + 1
  x["__"]
}

let f10 = (x: {@set "__x": int}) => {
  let old = x["__x"]
  x["__x"] = old + 1
  x["__x"]
}

/* triple _ */
let f11 = (x: {@set "___": int}) => {
  let old = x["___"]
  x["___"] = old + 1
  x["___"]
}
/* quad _ */
let f12 = (x: {@set "____": int}) => {
  let old = x["____"]
  x["____"] = old + 1
  x["____"]
}

let () = {
  eq(__LOC__, f0(%raw("{_open:0}")), 1)
  eq(__LOC__, f1(%raw("{_in:0}")), 1)
  eq(__LOC__, f2(%raw("{_MAX_LENGTH:0}")), 1)
  eq(__LOC__, f3(%raw("{_Capital:0}")), 1)
  eq(__LOC__, f4(%raw("{_open__:0}")), 1)
  eq(__LOC__, f5(%raw("{open__:0}")), 1)
  eq(__LOC__, f6(%raw(`{ "_'x" :0} `)), 1)
  eq(__LOC__, f7(%raw("{_Capital__:0}")), 1)
  eq(__LOC__, f8(%raw("{_MAX__:0}")), 1)
  eq(__LOC__, f9(%raw("{__:0}")), 1)
  eq(__LOC__, f10(%raw("{__x:0}")), 1)
  eq(__LOC__, f11(%raw("{___:0}")), 1)
  eq(__LOC__, f12(%raw("{____:0}")), 1)
}

Mt.from_pair_suites(__LOC__, suites.contents)
