type a = {x: int}

type b = {...a, y: string}

type c = {...b, z: string}

let v: c = {x: 10, y: "", z: ""}

type vw = {v: float, w: float}

type cvw = {...c, ...vw}

let v2: cvw = {x: 10, y: "", z: "", v: 1.0, w: 2.0}

type globalProps = {
  id?: string,
  name?: string,
  title?: string,
  className?: string,
}

type anchorProps = {
  ...globalProps,
  download?: string,
  href?: string,
  target?: [#_self | #_blank | #_parent | #_top],
}

// globalProps only case?
type divProps = {...globalProps}

type svgProps = {
  ...globalProps,
  x?: string,
  y?: string,
}

let x: svgProps = {x: "test", name: "test"}

// uncomment this to reveal a parser error
// type copiedSvgProps = {...svgProps}

module MultipleDotDotDots = {
  type t1 = {x: int}
  type t2 = {y: string}
  type t = {...t1, ...t2}
  let x: t = {x: 10, y: "abc"}
}
