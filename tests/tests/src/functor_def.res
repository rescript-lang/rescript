let v = ref(0)

let f = (x, x) => {
  incr(v)
  x + x
}

let return = () => v.contents

module Make = (
  U: {
    type t
    let say: (int, int) => int
  },
) => {
  /* let () = Js.log "no inline" */
  let h = (x, x) => {
    Js.log(f(x, x))
    U.say(x, x)
  }
}
