@genType
type someType = {"crop": option<string>, "fp-z": option<string>}

@genType
type someType2 = {
  crop: option<string>,
  @as("fp-z")
  fpz: option<string>,
}

let st: someType = {"crop": None, "fp-z": None}

let st2: someType2 = {crop: None, fpz: None}
