exception Scan_failure(string)

let scanf_bad_input = (ib, x) =>
  switch x {
  | Scan_failure(s) | Failure(s) =>
    for i in 0 to 100 {
      Console.log(s) /* necessary */
      Console.log("don't inlinie")
    }
  | x => throw(x)
  }
