@module("node:os")
external platform: unit => string = "platform"

let version = switch platform() {
| "linux" => 1
| "darwin" => 2
| _ => 3
}
