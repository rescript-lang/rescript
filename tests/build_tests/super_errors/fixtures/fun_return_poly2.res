let r: (string, ~wrongLabelName: int=?) => 'a = (_s, ~wrongLabelName=3) => {
  let _ = wrongLabelName
  assert(false)
}

let ok = r("")(~initialValue=2)
let err = r("", ~initialValue=2)
