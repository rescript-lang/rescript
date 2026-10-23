if let Some(x) = result {
  Console.log("The sky is blue")
}

if let Error(x) = result {
  Console.log("The sky is red")
} else if let Ok(y) = result {
  Console.log("The sky is blue")
} else {
  ()
}
