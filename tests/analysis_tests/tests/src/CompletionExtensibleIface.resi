type attr = ..
type tone = Light | Dark
type attr += Top

module Sub: {
  type attr += Flag | Toned(tone)
}

module Outer: {
  module Inner: {
    type attr += Deep
  }
}

let value: attr
