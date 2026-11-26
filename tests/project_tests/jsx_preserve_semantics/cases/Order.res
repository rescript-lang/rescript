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

module LoggedDict = {
  @val external make: React.component<dict<string>> = "LoggedComp"
}

// A dict spread is one props object: its children are evaluated before the
// fields after them
let dictBase: dict<string> = dict{}

let dictChildren =
  <LoggedDict {...dict{...dictBase, "children": note("children"), "title": note("title")}} />

let dictChildrenOrder = log->Array.copy

// Children are evaluated after the other props, as JSX writes them, even when
// the props type declares children first, and the key last
let domChildren = <div title={note("title")} key={note("key")}>{React.string(note("child"))}</div>
let domChildrenOrder = log->Array.copy

type childrenFirstProps = {children: string, a: string}

module ChildrenFirst = {
  @val external make: React.component<childrenFirstProps> = "LoggedComp"
}

let componentChildren = <ChildrenFirst a={note("a")}>{note("child")}</ChildrenFirst>
let componentChildrenOrder = log->Array.copy

let domManyChildren =
  <div title={note("title")}>
    {React.string(note("first"))}
    {React.string(note("second"))}
  </div>

let domManyChildrenOrder = log->Array.copy
