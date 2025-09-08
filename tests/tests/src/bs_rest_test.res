%%raw(`
function x(v){return [v]}
`)

@val external f: 'a => array<'a> = "x"

let u = f("3")
let v = f(3)

include (
  {
    @val external xxx: 'a => array<'a> = "x"
  }: {
    let xxx: 'a => array<'a>
  }
)

let u = xxx(3)
let xx = xxx("3")
