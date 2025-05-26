let () = /* c0 */ sideEffect1() /* c1 */

let () = {
  /* c0 */ sideEffect1() /* c1 */
}

let () = {
  /* c0 */ sideEffect1() /* c1 */
  /* c2 */ sideEffect2() /* c3 */
}

let () = {
  /* c0 */ sideEffect1() // c1
  // c2

  // c3
  /* c4 */ sideEffect2() // c5
}

let () = {
  /* c0 */ sideEffect1() /* c1 */
  /* c2 */ sideEffect2() /* c3 */
  /* c4 */ sideEffect3() /* c5 */
}

let () = {
  /* c0 */ sideEffect1() /* c1 */
  // k

  // k2
  /* k3 */
  // k4

  // k5
  /* c2 */ sideEffect2() /* c3 */
  // k
  
  // k2
  /* k3 */
  // k4

  // k5
  /* c4 */ sideEffect3() /* c5 */
}

let () = {
  /* c0 */ sideEffect1() /* c1 */
  /* c2 */ sideEffect2() /* c3 */
  /* c4 */ sideEffect3() /* c5 */
  /* c6 */ sideEffect4() /* c7 */
}

let () = {
  /* c0 */ let a /* inside */ = 1 /* c1 */
  /* c2 */ sideEffect2() /* c3 */
}


let () = {
  /* c0 */ let a /* inside */ = 1 /* c1 */
  // k

  // k2
  /* k3 */ /* k4 */

  // k5
  /* c2 */ sideEffect2() /* c3 */
}

let () = {
  /* c0 */ let a /* inside a */ = 1 /* c1 */
  /* c2 */ let b /* inside b */ = 2 /* c3 */
  /* c4 */ a + b /* c5 */
}

let () = {
  /* c0 */ let a /* inside a */ = 1 /* c1 */
  // k

  // k2
  /* k3 */ /* k4 */

  // k5
  /* c2 */ let b /* inside b */ = 2 /* c3 */
  // k

  // k2
  /* k3 */ /* k4 */

  // k5
  /* c4 */ a + b /* c5 */
}

let () = {
  /* c0 */ let a /* inside a */ = 1 /* c1 */
  /* c2 */ let b /* inside b */ = 2 /* c3 */
  /* c4 */ let c /* inside c */ = 3  /* c5 */
  /* c5 */ a + b + c /* c6 */
}

let () = {
  /* c0 */ exception /* inside */ Exit /* c1 */
  /* c2 */ throw(Exit) /* c3 */
}

let () = {
  /* c0 */ exception /* inside */ Exit // c1
  // k

  // k2
  /* c2 */ throw(Exit) // c3
}

let () = {
  /* c0 */ exception /* inside */ Exit /* c1 */
  /* c2 */ exception /* inside */ Terminate /* c3 */
  /* c4 */ throw(Exit) /* c5 */
}

let () = {
  /* c0 */ exception /* inside */ Exit // c1
  // k

  // k2

  // k3
  /* c2 */ exception /* inside */ Terminate // c3
  // k

  // k2

  // k3
  /* c4 */ throw(Exit) /* c5 */
}

let () = {
  /* c0 */ exception /* inside */ Exit /* c1 */
  /* c2 */ exception /* inside */ Terminate /* c3 */
  /* c4 */ exception /* inside */ Oom /* c5 */
  /* c6 */ throw(Exit) /* c7 */
}

let () = {
  /* c0 */ module /* c1 */ L /* c2 */ = /* c3 */ Logger /* c4 */
  /* c5 */ L.log() /* c6 */
}

let () = {
  /* c0 */ module /* c1 */ L /* c2 */ = /* c3 */ Logger // c4
  // k

  // k2

  // k3
  /* c5 */ L.log() /* c6 */
}

let () = {
  /* c0 */ module /* c1 */ L /* c2 */ = /* c3 */ Logger /* c4 */
  /* c5 */ module /* c6 */ L2 /* c7 */ = /* c8 */ Logger2 /* c9 */
  /* c10 */ L.log() /* c11 */
  /* c12 */ L2.log() /* c13 */
}

let () = {
  /* c0 */ module /* c1 */ L /* c2 */ = /* c3 */ Logger /* c4 */
  // k

  /* k1 */

  // k2
  /* c5 */ module /* c6 */ L2 /* c7 */ = /* c8 */ Logger2 /* c9 */
  // k

  /* k1 */

  // k2
  /* c10 */ L.log() /* c11 */
  // k

  /* k1 */

  // k2
  /* c12 */ L2.log() /* c13 */
}

let () = {
  /* c0 */ open /* c1 */ Belt /* c2 */
  /* c3 */ doSomething() /* c4 */
}

let () = {
  /* c0 */ open /* c1 */ Belt // c2
  // k

  /* k2 */

  // k3
  /* c3 */ doSomething() // c4
}

let () = {
  /* c0 */ open /* c1 */ Belt /* c2 */
  /* c3 */ open /* c4 */ React /* c5 */
  /* c6 */ doSomething() /* c7 */
}

let () = {
  /* c0 */ open /* c1 */ Belt /* c2 */
  // k

  /* k2 */

  // k3
  /* c3 */ open /* c4 */ React /* c5 */
  // k

  /* k2 */

  // k3
  /* c6 */ doSomething() /* c7 */
}

while true {
  /* c0 */ let () = sideEffect1() // c1
  // c2

  /* c3 */ let () = sideEffect2() // c4
  // c5

  // c6
  /* c7 */ sideEffect2() // c8

  // here
}

for i in 0 to 10 {
  /* c0 */ let () = sideEffect1() // c1
  // c2

  /* c3 */ let () = sideEffect2() // c4
  // c5

  // c6
  /* c7 */ sideEffect2() // c8

  // here
}


switch color {
| Blue =>
  /* c0 */ let () = sideEffect1() // c1
  // c2

  /* c3 */ let () = sideEffect2() // c4
  // c5

  // c6
  /* c7 */ sideEffect2() // c8

| Red =>
  /* c0 */ let () = sideEffect1() // c1
  // c2

  /* c3 */ let () = sideEffect2() // c4
  // c5

  // c6
  /* c7 */ sideEffect2() // c8
}

try danger() catch {
| Exc =>

  /* c0 */ let () = sideEffect1() // c1
  // c2

  /* c3 */ let () = sideEffect2() // c4
  // c5

  // c6
  /* c7 */ sideEffect2() // c8
}

if {
  // here
  let a = true
  // okok

  // there
  let /* inside */ b = false // trailing

  // between
  true // test

  /* ws */
  // after
} {
 // here
  let a = true
  // okok

  // there
  let /* inside */ b = false
  false // here
} else {
 // here
  let a = true
  // okok

  // there
  let b = false
  true
} // trailing

if {
  // here
  let a = true
  // okok

  // there
  let /* inside */ b = false // trailing

  // between
  true // test

  /* ws */
  // after
} {
 // here
  let a = true
  // okok

  // there
  let /* inside */ b = false
  false // here
} else if {
  let u = universe() // c0
  // c1

  // c2
  let /* inside */ gc = forceGc() // stop the world
  // c3

  // c4
  f(u, gc) // c5
  /* c6 */
  // c7
} { 
 // here
  let a = true
  // okok

  // there
  let b = false
  true
} // trailing

assert({
  // here 
  open /* inside */ Matrix
  // c

  // c2
  compare(m1, m2)
  // after

  // test
})

user.name = {
  // here 
  open /* inside */ Names
  // c

  // c2
  defaultName
  // after

  // test
}

switch {
  // here
  open Matrix
  // there

  // test
  arityCheck()
  // trailing

  // hmm
} {
| One => ()
| Two => ()
}

try {
  // here
  open Matrix
  // there

  // test
  arityCheck()
  // trailing

  // hmm
} catch {
| One => ()
| Two => ()
}

let () = {
  // here
  open Matrix
  // there

  // test
  arityCheck
  // trailing

  // hmm
}(arg1, arg2)

let name = {
  // here
  open Users
  // there

  // test
  defaultUser
  // trailing

  // hmm
}.name

let () = {
  // here
  open Users
  // there

  // test
  defaultUser
  // trailing

  // hmm
}.name = {
  // here
  open Names
  // there

  // test
  defaultName
  // trailing

  // hmm
}

while {
  // comment
  open Conditions
  // inside

  // inside2
  check()
  // foo

  // bar
} {
  Js.log("test")
}

let multiply = (/* c0 */ m1 /* c1 */, /* c2 */ m2 /* c3 */) => {
  // here
  open Matrix4D

  let m3 = makeUninitializedUnsafe()
  // there
  
  // over there
  m3
  /* trailing */
  // test
}

switch x {
| Blue when /* c0 */ multicore.enabled /* c1 */ === /* c2 */ true /* c3 */ => ()
| Red when {
  // c0
  open /* c1 */ Multicore // c2
  // c3

  // c4
  isEnabled() // c5
  /* c6 */

  // c7

 } => ()
}

let catch = 34

let promiseCatch = x => Js.Promise.catch(x)

// aggregating intersections
let () = {
  let s = Sphere.make()
  let i1 = make(~t=1., ~sphere=s)
  let i2 = make(~t=2., ~sphere=s)

//  let is = interesections
  
//  assert 
}

let () = {
  let s = Sphere.make()
  let i1 = make(~t=1., ~sphere=s)
  let i2 = make(~t=2., ~sphere=s)

  ()
//  let is = interesections
  
//  assert 
}
