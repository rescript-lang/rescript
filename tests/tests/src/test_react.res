/* TODO: binding -- document.getElementById -- to mount node */

type html_element

@val("console.log") external log: 'a => unit = ""
let v = log(32)
type t
type element
@val external document: t = "document"
@send external getElementById: (t, string) => element = "getElementById"

type config
type component
@obj external config: (~display_name: string=?, ~render: unit => component, unit) => config = ""

type attrs
@obj external attrs: (~alt: string=?, ~autoPlay: bool=?, unit) => attrs = ""

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

type component_class
@val("createClass") @module("react") external createClass: config => component_class = "createClass"

@val("render") @module("react-dom")
external render: (component_class, element) => unit =
  "" /* TODO: error checking -- attributes todo */

render(
  createClass(
    config(
      ~render=_ =>
        div(
          vdom,
          ~attrs=attrs(~alt="pic", ()),
          [
            h1(vdom, [str("hello react")]),
            h2(vdom, [str("type safe!")]),
            h3(vdom, [str("type safe!")]),
          ],
        ),
      (),
    ),
  ),
  getElementById(document, "hi"),
)

let u = 33

@module external make: unit => unit = "xxx"
@module("xxx") external make2: unit => unit = "xx"
@module("xxx") external make3: unit => unit = "xxx"
@module("a/b/c") external make4: unit => unit = "x"
@module("a/b/c") external make5: unit => unit = "y"

@module("b/c") external make6: unit => unit = "x"
@module("b/c") external make7: unit => unit = "y"

@module("c") external make8: unit => unit = "x"
@module("c") external make9: unit => unit = "y"

let f = () => {
  make()
  make2()
  make3()
  make4()
  make5()
  make6()
  make7()
  make8()
  make9()
}
