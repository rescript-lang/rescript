@module("path") @variadic external join: array<string> => string = "join"

let () = Js.log(join([".", __MODULE__]))

/* let f x = join x */
