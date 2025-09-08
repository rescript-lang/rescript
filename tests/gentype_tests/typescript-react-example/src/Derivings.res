@genType @deriving(accessors)
type action =
  | Click
  | Submit(string)
  | Cancel
