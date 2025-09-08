/** Imperative style */
let rec fib = x =>
  switch x {
  | 1 | 2 => 1
  | n => fib(n - 1) + fib(n - 2)
  }

/** Imperative style */
/** List map */
let sum = n => {
  let v = ref(0)
  for i in 0 to n {
    v := v.contents + i
  }
  v.contents
}

/** List map */
type rec list<'a> =
  | Nil
  | Cons('a, list<'a>)

let rec map = (f, x) =>
  switch x {
  | Nil => Nil
  | Cons(x, xs) => Cons(f(x), map(f, xs))
  }

/** Test curry and uncurry calling convention */
let test_curry = (x, y) => x + y
let f = test_curry(32, ...)

/** Create a typed binding for react */
type t
type element
@val external document: t = "document"
@send external getElementById: (t, string) => element = "getElementById"

/** Phantom types */
type config
type component
type attrs
type component_class

/** make a json object */
@obj
external config: (~display_name: string=?, ~render: unit => component, unit) => config = ""
/** make a json object */
@obj
external attrs: (~alt: string=?, ~autoPlay: bool=?, unit) => attrs = ""
external str: string => component = "%identity"

type vdom
@module("react") @val external vdom: vdom = "DOM"

/* FIXME: investigate 
   cases:
   {[
     [@@module "package1" "same_name"]
     [@@module "package2" "same_name"]
   ]}
   {[
     [@@module "package" "name1"]
     [@@module "package" "name2"]
   ]}
*/
@send @variadic external h1: (vdom, ~attrs: attrs=?, array<component>) => component = "h1"
@send @variadic external h2: (vdom, ~attrs: attrs=?, array<component>) => component = "h2"

@send @variadic external h3: (vdom, ~attrs: attrs=?, array<component>) => component = "h3"

@send @variadic external h4: (vdom, ~attrs: attrs=?, array<component>) => component = "h4"

@send @variadic external div: (vdom, ~attrs: attrs=?, array<component>) => component = "div"

@val("createClass") @module("react") external createClass: config => component_class = "createClass"
@val("render") @module("react-dom") external render: (component_class, element) => unit = ""

render(
  createClass(
    config(
      ~render=_ =>
        div(
          vdom,
          ~attrs=attrs(~alt="pic", ()),
          [h1(vdom, [str("hello react")]), h2(vdom, [str("type safe!")])],
        ),
      (),
    ),
  ),
  getElementById(document, "hi"),
)
