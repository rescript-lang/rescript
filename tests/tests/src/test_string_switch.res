@module("node:os")
external platform: unit => string = "platform"

let version = switch platform() {
| "linux" => 1
| "darwin" => 2
| _ => 3
}

let classifyEquivalentEscape = (value, selectedCase) =>
  switch value {
  | "a" if selectedCase == 0 => 0
  | "\x61" if selectedCase == 1 => 1
  | "\u0061" if selectedCase == 2 => 2
  | "\u{61}" if selectedCase == 3 => 3
  | "\x61" => 4
  | _ => 5
  }

let classifyEquivalentSurrogateEscape = (value, selectedCase) =>
  switch value {
  | "😀" if selectedCase == 0 => 0
  | "\uD83D\uDE00" if selectedCase == 1 => 1
  | "\u{1f600}" => 2
  | _ => 3
  }
