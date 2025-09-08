let user = {
  "name": "Steve",
  "age": 30
}

let coord = {"x": 1, "y": 2}

let person = {
  "name": {
    "first": "Bob",
    "last": "Zhmith"
  },
  "age": 32
}

// print parens around constrained expr in rhs
let user = {"name": (ceo.name: string)}
// braces should be preserved on rhs
let user = {"name": {ceo.name}}
let user = {"name": {
  ceo.name
}}
// braced + constrained expr
let user = {"name": {(ceo.name: string)}}

React.jsx(
  ReactDOM.stringToComponent("div"),
  {
    let ariaCurrent = #page
    {
      "aria-current": (ariaCurrent : [
      | #page
      | #step
      | #location
      | #date
      | #time
      | #"true"
      | #"false"
      ])
    }
  }
)

React.jsx(
  ReactDOM.stringToComponent("div"),
  {
    let children = {msg->React.string}
    {"children": (children: React.element)}
  }
)

(@warning("-3") React.jsx)(
  ReactDOM.stringToComponent("div"),
  {
    let \"data-foo" = "payload"
    {"data-foo": (\"data-foo": string)}
  }
)

// comments
let x = {/* foo */ "foo": "bar"}
let x = {"foo": /* foo */ "bar"}
let x = {"foo": "bar" /* foo */ }

let x = {
	// foo
	"foo": "bar",
	// bar
	"bar": "baz",
	// baz
	"baz": baz
}

let x = {
	"foo": "bar", // foo
	"bar": "baz", // bar
	"baz": baz // baz
}

let x = {
	"foo": /* foo */ "bar",
	"bar": /* bar */ "baz",
	"baz": /* bar */ baz
}

let x = {
	/* foo */ "foo": "bar",
	/* bar */ "bar": "baz",
	/* bar */ "baz": baz
}

let x = {
	"foo": "bar" /* foo */,
	"bar": "baz" /* bar */,
	"baz": baz /* bar */
}