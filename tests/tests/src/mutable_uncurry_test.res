@@config({
  flags: [
    /* "-bs-diagnose" */
  ],
})

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eqs = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let eq = ({contents: (x: int)}, {contents: y}) => x == y

let eq2 = (x, {contents: y}) => x.contents == y

eqs(__LOC__, false, eq(ref(1), ref(2)))
eqs(__LOC__, true, eq(ref(2), ref(2)))

let ut3 = ({contents: x0}, {contents: x1}, {contents: x2}) => (x0, x1, x2)
let t3 = ({contents: x0}, {contents: x1}, {contents: x2}) => (x0, x1, x2)

let ut4 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}) => (x0, x1, x2, x3)

let t4 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}) => (x0, x1, x2, x3)

let ut5 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}, {contents: x4}) => (
  x0,
  x1,
  x2,
  x3,
  x4,
)

let t5 = ({contents: x0}, {contents: x1}, {contents: x2}, {contents: x3}, {contents: x4}) => (
  x0,
  x1,
  x2,
  x3,
  x4,
)

let nested0 = ({contents: x0}, {contents: x1}, {contents: x2}) => {
  let a = x0 + x1 + x2
  ({contents: x0}, {contents: x1}, {contents: x2}) => a + x0 + x1 + x2
}

let nested1 = ({contents: x0}, {contents: x1}, {contents: x2}) => {
  let a = x0 + x1 + x2
  ({contents: x0}, {contents: x1}, {contents: x2}) => a + x0 + x1 + x2
}

eqs(__LOC__, ut3(ref(1), ref(2), ref(3)), (1, 2, 3))
eqs(__LOC__, t3(ref(1), ref(2), ref(3)), (1, 2, 3))

eqs(__LOC__, ut5(ref(1), ref(2), ref(3), ref(1), ref(1)), (1, 2, 3, 1, 1))
Mt.from_pair_suites(__FILE__, suites.contents)
