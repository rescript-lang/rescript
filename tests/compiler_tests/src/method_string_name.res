let f = {"Content-Type": 3}

Console.log(f["Content-Type"])

let ff = x => {
  x["Hi"]
  // x## "hi#";
  x["Content-Type"] = "hello"
  Console.log(({"Content-Type": "hello"})["Content-Type"])
}
