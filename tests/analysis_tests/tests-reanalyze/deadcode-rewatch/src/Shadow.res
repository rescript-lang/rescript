@genType
let test = () => 3

@genType
let test = () => "a"

module M = {
  @genType
  let test = () => 3

  let test = () => "a"
}

