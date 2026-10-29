@@config({flags: ["-bs-jsx", "4"]})

// Children are evaluated after the other props, as JSX writes them, although
// JsxDOM.domProps declares children before most attributes (#9104)
let note = (s: string) => s->String.toUpperCase

let element = <div title={note("t")} key={note("k")}>{React.string(note("c"))}</div>

let elements =
  <div title={note("t")}>
    {React.string(note("c1"))}
    {React.string(note("c2"))}
  </div>

type props = {children: string, a: string}

module ChildrenFirst = {
  @val external make: React.component<props> = "ChildrenFirst"
}

let component = <ChildrenFirst a={note("a")}>{note("c")}</ChildrenFirst>
