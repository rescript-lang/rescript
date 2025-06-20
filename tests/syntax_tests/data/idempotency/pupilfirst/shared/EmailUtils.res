let regularExpression =
  /^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i

let isInvalid = (allowBlank, email) =>
  if email->String.trim->String.length > 0 {
    !(email->Js.Re.test_(regularExpression))
  } else {
    !allowBlank
  }
