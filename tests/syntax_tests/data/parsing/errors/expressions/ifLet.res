if let Some(x) = result {
  Js.log("The sky is blue")
}

if let Error(x) = result {
  Js.log("The sky is red")
} else if let Ok(y) = result {
  Js.log("The sky is blue")
} else {
  ()
}
