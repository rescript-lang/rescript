let x = [1, 2]
let y = try x->Array.getUnsafe(3) catch {
| Invalid_argument(msg) =>
  Js.log(msg)
  0
}
