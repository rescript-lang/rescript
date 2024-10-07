// empty dict
let x = dict{}

// one value
let x = dict{"foo": "bar"}

// two values
let x = dict{"foo": "bar", "bar": "baz"}

let baz = "foo"
let x = dict{"foo": "bar", "bar": "baz", "baz": baz}

// multiline
let x = dict{
	"foo": "bar",
	"bar": "baz",
	"baz": baz
}

// comments
let x = dict{/* foo */ "foo": "bar"}
let x = dict{"foo": /* foo */ "bar"}
let x = dict{"foo": "bar" /* foo */ }

let x = dict{
	// foo
	"foo": "bar",
	// bar
	"bar": "baz",
	// baz
	"baz": baz
}

let x = dict{
	"foo": "bar", // foo
	"bar": "baz", // bar
	"baz": baz // baz
}

let x = dict{
	"foo": /* foo */ "bar",
	"bar": /* bar */ "baz",
	"baz": /* bar */ baz
}

let x = dict{
	/* foo */ "foo": "bar",
	/* bar */ "bar": "baz",
	/* bar */ "baz": baz
}

let x = dict{
	"foo": "bar" /* foo */,
	"bar": "baz" /* bar */,
	"baz": baz /* bar */
}
