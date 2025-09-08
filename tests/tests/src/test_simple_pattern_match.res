@module("node:os")
external platform: unit => string = "platform"

let (a, b) = switch platform() {
| "linux" | "darwin" => (1, 2)
| _ => (3, 4)
}
