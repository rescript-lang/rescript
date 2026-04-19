let flag = true
let other = false

let direct = if flag {
  1
} else {
  2
}
let ternary = flag ? "yes" : "no"

let chained = if flag {
  1
} else if other {
  2
} else {
  3
}

let onlyWhen = if flag {
  Console.log("hit")
}
