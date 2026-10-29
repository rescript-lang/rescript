module Id: {
  @unboxed type t = private Text(string) | Number(int)
} = {
  @unboxed type t = Text(string) | Number(int)
}

let fromString = ("a" :> Id.t)
