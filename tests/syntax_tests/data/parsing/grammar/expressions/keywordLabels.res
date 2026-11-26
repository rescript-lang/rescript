// A reserved word is a name where the grammar expects a label, a variant tag
// or a lowercase JSX tag name.

// Field access and update
let a = r.type
let a = r.M.type
let a = r.true.false.let
r.type = 1
r.M.open = 2

// Record expressions
let a = {type: 1}
let a = {...r, type: 1, open: 2}
let a = {x: 1, type: ?v}
let a = {M.type: 1}
let a = {M.N.type: 1, x: 2}
let a = {x: 1, M.module: 2}
let a = {break: 1, continue: 2, true: 3, false: 4}
let a = {
  and: 1,
  as: 1,
  assert: 1,
  await: 1,
  break: 1,
  constraint: 1,
  continue: 1,
  else: 1,
  exception: 1,
  external: 1,
  false: 1,
  for: 1,
  if: 1,
  in: 1,
  include: 1,
  let: 1,
  module: 1,
  mutable: 1,
  of: 1,
  open: 1,
  private: 1,
  rec: 1,
  switch: 1,
  true: 1,
  try: 1,
  type: 1,
  when: 1,
  while: 1,
}

// Blocks that start with a keyword are still blocks
let a = {true}
let a = {false ? 1 : 2}
let a = {
  open M
  x
}
let a = {if x {1} else {2}}
let a = () => {break}

// Argument labels
let f = (~type as kind, ~for as target=?, ~in as i=1) => kind
let a = f(~type="a", ~for=?target, ~in=1)
let a = f(~let=1, ~true=false)

// Polymorphic variant tags
let a = #type
let a = #module(1, 2)
let a = x == #let ? 1 : 2
let a = x == #let? 1 : 2
let a = r.let? 1 : 2

// JSX props and lowercase tag names
let a = <input type="text" for=?id />
let a = <div data-type="a" aria-for="b" data-open />
let a = <svg> <switch> <g /> </switch> </svg>
let a = <switch />
let a = <if> {children} </if>
