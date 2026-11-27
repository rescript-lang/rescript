@@config({flags: ["-bs-jsx", "4"]})

// Records with many optional fields, as JSX props records are

type big = {
  mutable count: int,
  @as("data-id") dataId?: string,
  a0?: int,
  a1?: int,
  a2?: int,
  a3?: int,
  a4?: int,
  a5?: int,
  a6?: int,
  a7?: int,
  a8?: int,
  a9?: int,
  a10?: int,
  a11?: int,
  a12?: int,
  a13?: int,
  a14?: int,
  a15?: int,
  a16?: int,
  a17?: int,
  a18?: int,
  a19?: int,
  a20?: int,
  a21?: int,
  a22?: int,
  a23?: int,
  name: string,
}

let omitted: big = {count: 1, name: "n"}
let written: big = {count: 1, dataId: "x", a5: 5, a23: 23, name: "n"}
let writtenOpt = (x: option<int>): big => {count: 1, a3: ?x, name: "n"}
let spread = (b: big) => {...b, a3: 3}
let spreadOpt = (b: big, x: option<int>) => {...b, a4: ?x, dataId: "y"}
let spreadMutable = (b: big) => {...b, count: b.count + 1}

// Every field optional, so no field is required
type allOptional = {
  x?: int,
  @as("y-name") y?: string,
  mutable z?: bool,
}

let empty: allOptional = {}
let someFields: allOptional = {x: 1, y: "y"}
let allOptionalSpread = (r: allOptional) => {...r, z: true}

// A small record with an optional field still uses object spread
type small = {s: int, t?: string}

let smallSpread = (r: small) => {...r, s: 2}

// A small record without optional fields: a spread copies the fields one by
// one
type plain = {p: int, q: string}

let plainSpread = (r: plain) => {...r, p: 2}

// JSX props of host elements are JsxDOM.domProps, all optional
let host = <div className="c" id="i" dataTestId="t">{React.string("x")}</div>
let hostNoProps = <div />
let hostOptional = (value: option<string>) => <input ?value type_="text" />
let hostSpread = (props: JsxDOM.domProps) => <div {...props} />

let hostSpreadProps = (props: JsxDOM.domProps) =>
  <div {...props} className="c" key="k">{React.string("x")}</div>

let nested =
  <div className="outer">
    <span id="a">{React.string("a")}</span>
    <p>
      {React.string("b")}
      <br />
    </p>
  </div>

let keyed = (items: array<string>) =>
  items->Array.map(item => <li key=item>{React.string(item)}</li>)->React.array
