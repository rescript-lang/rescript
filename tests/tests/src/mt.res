@val external describe: (string, unit => unit) => unit = "describe"

@val external it: (string, unit => unit) => unit = "it"

@val external it_promise: (string, unit => Js.Promise.t<_>) => unit = "it"

@val @module("assert") external eq: ('a, 'a) => unit = "deepEqual"

@val @module("assert") external neq: ('a, 'a) => unit = "notDeepEqual"

@val @module("assert") external strict_eq: ('a, 'a) => unit = "strictEqual"

@val @module("assert") external strict_neq: ('a, 'a) => unit = "notStrictEqual"

@val @module("assert") external ok: bool => unit = "ok"

@val @module("assert") external fail: ('a, 'a, Js.undefined<string>, string) => unit = "fail"

@val @variadic external dump: array<'a> => unit = "console.log"

/** There is a problem --
    it does not return [unit]
*/
@val @module("assert")
external throws: (unit => unit) => unit = "throws"

let assert_equal = eq
let assert_notequal = neq
let assert_strict_equal = strict_eq
let assert_strict_notequal = strict_neq
let assert_ok = a => ok(a)
let assert_fail = msg => fail((), (), Js.Undefined.return(msg), "")

@module("process") external argv: array<string> = "argv"
@module("path") external basename: string => string = "basename"

let is_mocha = () =>
  switch Belt.List.fromArray(argv) {
  | list{_node, mocha, ..._} =>
    let exec = basename(mocha)
    exec == "mocha" || exec == "_mocha"
  | _ => false
  }
/* assert -- raises an AssertionError which mocha handls better
 */

let from_suites = %raw(`
function from_suites(name, suites) {
  var match = Belt_List.fromArray(Process.argv);
  if (match && is_mocha(undefined)) {
    describe(name, (function () {
            return Belt_List.forEach(suites, (function (param) {
                          var partial_arg = param[1];
                          it(param[0], (function () {
                                  return partial_arg(undefined);
                                }));
                        }));
          }));
    return ;
  }
  
}
`)

type rec eq =
  | Eq('a, 'a): eq
  | Neq('a, 'a): eq
  | StrictEq('a, 'a): eq
  | StrictNeq('a, 'a): eq
  | Ok(bool): eq
  | Approx(float, float): eq
  | ApproxThreshold(float, float, float): eq
  | ThrowAny(unit => unit): eq
  | Fail(unit): eq
  | FailWith(string): eq
/* TODO: | Exception : exn -> (unit -> unit) -> _ eq */

type pair_suites = list<(string, unit => eq)>
type promise_suites = list<(string, Js.Promise.t<eq>)>
let close_enough = (~threshold=0.0000001 /* epsilon_float */, a, b) => abs_float(a -. b) < threshold

let node_from_pair_suites = (name: string, suites: pair_suites) => {
  Js.log((name, "testing"))
  suites->Belt.List.forEach(((name, code)) =>
    switch code() {
    | Eq(a, b) => Js.log((name, a, "eq?", b))
    | Neq(a, b) => Js.log((name, a, "neq?", b))
    | StrictEq(a, b) => Js.log((name, a, "strict_eq?", b))
    | StrictNeq(a, b) => Js.log((name, a, "strict_neq?", b))
    | Approx(a, b) => Js.log((name, a, "~", b))
    | ApproxThreshold(t, a, b) => Js.log((name, a, "~", b, " (", t, ")"))
    | ThrowAny(fn) => ()
    | Fail(_) => Js.log("failed")
    | FailWith(msg) => Js.log("failed: " ++ msg)
    | Ok(a) => Js.log((name, a, "ok?"))
    }
  )
}

let handleCode = spec =>
  switch spec {
  | Eq(a, b) => assert_equal(a, b)
  | Neq(a, b) => assert_notequal(a, b)
  | StrictEq(a, b) => assert_strict_equal(a, b)
  | StrictNeq(a, b) => assert_strict_notequal(a, b)
  | Ok(a) => assert_ok(a)
  | Approx(a, b) =>
    if !close_enough(a, b) {
      assert_equal(a, b)
    } /* assert_equal gives better ouput */
  | ApproxThreshold(t, a, b) =>
    if !close_enough(~threshold=t, a, b) {
      assert_equal(a, b)
    } /* assert_equal gives better ouput */
  | ThrowAny(fn) => throws(fn)
  | Fail(_) => assert_fail("failed")
  | FailWith(msg) => assert_fail(msg)
  }

let from_pair_suites = %raw(`
function from_pair_suites(name, suites) {
  var match = Belt_List.fromArray(Process.argv);
  if (match) {
    if (is_mocha(undefined)) {
      describe(name, (function () {
              return Belt_List.forEach(suites, (function (param) {
                            var code = param[1];
                            it(param[0], (function () {
                                    return handleCode(code(undefined));
                                  }));
                          }));
            }));
      return ;
    } else {
      console.log([
            name,
            "testing"
          ]);
      return Belt_List.forEach(suites, (function (param) {
                    var name = param[0];
                    var fn = param[1](undefined);
                    switch (fn.TAG) {
                      case "Eq" :
                          console.log([
                                name,
                                fn._0,
                                "eq?",
                                fn._1
                              ]);
                          return ;
                      case "Neq" :
                          console.log([
                                name,
                                fn._0,
                                "neq?",
                                fn._1
                              ]);
                          return ;
                      case "StrictEq" :
                          console.log([
                                name,
                                fn._0,
                                "strict_eq?",
                                fn._1
                              ]);
                          return ;
                      case "StrictNeq" :
                          console.log([
                                name,
                                fn._0,
                                "strict_neq?",
                                fn._1
                              ]);
                          return ;
                      case "Ok" :
                          console.log([
                                name,
                                fn._0,
                                "ok?"
                              ]);
                          return ;
                      case "Approx" :
                          console.log([
                                name,
                                fn._0,
                                "~",
                                fn._1
                              ]);
                          return ;
                      case "ApproxThreshold" :
                          console.log([
                                name,
                                fn._1,
                                "~",
                                fn._2,
                                " (",
                                fn._0,
                                ")"
                              ]);
                          return ;
                      case "ThrowAny" :
                          return ;
                      case "Fail" :
                          console.log("failed");
                          return ;
                      case "FailWith" :
                          console.log("failed: " + fn._0);
                          return ;
                      
                    }
                  }));
    }
  }
  
}
`)
let val_unit = Js.Promise.resolve()

let from_promise_suites = %raw(`

function from_promise_suites(name, suites) {
  var match = Belt_List.fromArray(Process.argv);
  if (match) {
    if (is_mocha(undefined)) {
      describe(name, (function () {
              return Belt_List.forEach(suites, (function (param) {
                            var code = param[1];
                            it(param[0], (function () {
                                    var arg1 = function (x) {
                                      handleCode(x);
                                      return val_unit;
                                    };
                                    return code.then(arg1);
                                  }));
                          }));
            }));
    } else {
      console.log("promise suites");
    }
    return ;
  }
  
}
`)

let old_from_promise_suites_donotuse = (name, suites: list<(string, Js.Promise.t<_>)>) =>
  switch Belt.List.fromArray(argv) {
  | list{cmd, ..._} =>
    if is_mocha() {
      describe(name, () =>
        suites->Belt.List.forEach(((name, code)) =>
          it_promise(
            name,
            _ =>
              Js.Promise.then_(
                x => {
                  handleCode(x)
                  val_unit
                },
                code,
              ),
          )
        )
      )
    } else {
      Js.log("promise suites")
    } /* TODO */
  | _ => ()
  }

/*
Note that [require] is a file local value,
we need type [require]

let is_top : unit -> bool = [%raw{|
function (_){
console.log('hi');
if (typeof require === "undefined"){
  return false
} else {
  console.log("hey",require.main.filename);
  return require.main === module;
}
}
|}]

let from_pair_suites_non_top name suites =
    if not @@ is_top () then
      from_pair_suites name suites
*/

let eq_suites = (~test_id, ~suites, loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Eq(x, y)), ...suites.contents}
}

let bool_suites = (~test_id, ~suites, loc, x) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Ok(x)), ...suites.contents}
}

let throw_suites = (~test_id, ~suites, loc, x) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => ThrowAny(x)),
      ...suites.contents,
    }
}
