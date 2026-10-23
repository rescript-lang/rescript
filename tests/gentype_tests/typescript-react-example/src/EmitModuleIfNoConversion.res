@genType
type t =
  | A
  | B({name: string})

module X = {
  @genType
  let foo = (t: t) =>
    switch t {
    | A => Console.log("A")
    | B({name}) => Console.log("B" ++ name)
    }

  @genType let x = 42
}

// No field requires converion: emit module Y
module Y = {
  @genType let x = ""
}
