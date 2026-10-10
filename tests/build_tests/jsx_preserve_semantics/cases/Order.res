// The order in which the tag and the props are evaluated, observed through a
// getter on the tag
%%raw(`
var orderLog = [];
Object.defineProperty(globalThis, "LoggedComp", {
  configurable: true,
  get() {
    orderLog.push("tag");
    return function LoggedComp(props) { return null };
  },
});
`)

type props = {a: string, b: string}
module Logged = {
  @val external make: React.component<props> = "LoggedComp"
}

let log: array<string> = %raw(`orderLog`)
let getProps = (): props => {
  log->Array.push("props")
  {a: "a", b: "b"}
}
let note = (s: string) => {
  log->Array.push(s)
  s
}

// A spread of a small record without optional fields copies its fields
let copySpread = <Logged {...getProps()} b={note("b")} />
let copySpreadOrder = log->Array.copy

let props = <Logged a={note("a")} b={note("b")} />
let propsOrder = log->Array.copy
