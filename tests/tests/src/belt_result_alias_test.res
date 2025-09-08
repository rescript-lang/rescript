/* Make sure Belt.Result.t / Pervasives.result can be used interchangiably */

let _ = Ok("Test")->Belt.Result.map(r => "Value: " ++ r)

let _ = {
  open Belt.Result
  Error("error")->map(r => "Value: " ++ r)->getWithDefault("success")
}
