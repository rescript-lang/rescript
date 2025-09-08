let \"-" = \"-."
let \"+" = \"+."
let \"*" = \"*."
let \"/" = \"/."

let newton = (~f, ~fPrimed, ~initial, ~threshold) => {
  let current = ref(initial)
  let iterateMore = (previous, next) => {
    let delta = next >= previous ? next - previous : previous - next
    current := next
    !(delta < threshold)
  }
  @progress(iterateMore)
  let rec loop = () => {
    let previous = current.contents
    let next = previous - f(previous) / fPrimed(previous)
    if iterateMore(previous, next) {
      loop()
    } else {
      current.contents
    }
  }
  loop()
}
let f = x => x * x * x - 2.0 * x * x - 11.0 * x + 12.0

let fPrimed = x => 3.0 * x * x - 4.0 * x - 11.0

let result = newton(~f, ~fPrimed, ~initial=5.0, ~threshold=0.0003)

Js.log2(result, f(result))

