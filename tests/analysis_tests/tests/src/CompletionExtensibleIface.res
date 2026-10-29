type attr = ..
type attr += Top

module Sub = {
  type attr += Flag
}

module Outer = {
  module Inner = {
    type attr += Deep
  }
}

let value: attr = Top
