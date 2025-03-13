@@jsxConfig({version:4})
// Error: spreadProps should be first in order than other props
// let c0 = <A x="x" {...p} />

// Error: multiple spreadProps not allowed
// let c0 = <A x="x" {...p0} {...p1} />

// only spread props
let c1 = <A {...p} />

// reversed order
let c2 = <A {...p} x="x" />

let c3 = <div {...p} />

let c4 = <div {...p} x="x" key="k" />

let c5 = <div {...p} key="k"><br /><br /></div>

// both need to be parsed
let c6 = <A {...(params->Obj.magic)} />
let c7 = <A {...params->Obj.magic} />
