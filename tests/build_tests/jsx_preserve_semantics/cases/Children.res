let cond = ref(true)
let items = ["a", "b"]

let singleString = <div> {React.string("a")} </div>
let twoStrings =
  <div>
    {React.string("a")}
    {React.string("b")}
  </div>
let singleElement =
  <div>
    <span />
  </div>
let fragmentSingle =
  <>
    <input />
  </>
let fragmentSingleString = <> {React.string("x")} </>
let fragmentTwo =
  <>
    <input />
    <b />
  </>
let emptyFragment = <> </>
let mapped = <ul> {items->Array.map(i => <li key=i> {React.string(i)} </li>)->React.array} </ul>
let mappedWithSibling =
  <ul>
    <li> {React.string("first")} </li>
    {items->Array.map(i => <li key=i> {React.string(i)} </li>)->React.array}
  </ul>
let seqChild =
  <div>
    {
      cond := false
      React.string("s")
    }
  </div>
let ternaryChild = <div> {cond.contents ? <a /> : <b />} </div>
let nullChild = <div> {React.null} </div>
let intChild = <div> {React.int(1)} </div>
let floatChild = <div> {React.float(1.5)} </div>
let nested =
  <div>
    <div>
      <div>
        <span />
      </div>
    </div>
  </div>
let manyChildren =
  <div>
    <a />
    <b />
    <i />
    <u />
    <s />
  </div>

module Comp = {
  @react.component
  let make = (~children) => <div> children </div>
}
let componentSingleChild =
  <Comp>
    <span />
  </Comp>
let componentChildren =
  <Comp>
    <span />
    <b />
  </Comp>
let componentStringChild = <Comp> {React.string("t")} </Comp>

module NoChildren = {
  @react.component
  let make = () => React.null
}
let noChildren = <NoChildren />

// Children passed explicitly as a prop
let childrenProp = <Comp children={<span />} />

// Element bound to a variable and used as a child
let el = <span />
let varChild = <div> el </div>
let varChildren =
  <div>
    el
    el
  </div>

// Optional-typed child via a variable
let optEl: option<React.element> = Some(<span />)
let optionChild = <div> {optEl->Option.getOr(React.null)} </div>

// Keyed elements with children
let keyedChildren =
  <div key="k">
    <a />
    <b />
  </div>
let keyedSingleChild =
  <div key="k">
    <a />
  </div>
let keyedNoChild = <div key="k" />
let optKey: option<string> = Some("ok")
let optionalKey = <div key=?optKey />
let optionalKeyNone = <div key=?None />
let keyedComponent =
  <Comp key="k">
    <span />
  </Comp>
