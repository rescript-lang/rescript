@genType
type t =
  | A
  | B({name: string})

// foo requires converion: don't emit module X
module X = {
  @genType
  let foo = (t: t) =>
    switch t {
    | A => Js.log("A")
    | B({name}) => Js.log("B" ++ name)
    }

  @genType let x = 42
}

// No field requires converion: emit module Y
module Y = {
  @genType let x = ""
}
