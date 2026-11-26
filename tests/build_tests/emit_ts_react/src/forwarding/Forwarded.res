let increment = n => n + 1

type counter = {mutable count: int}

let bumpInPlace = previous => {
  previous.count = previous.count + 1
  previous
}
