let f = {"Content-Type": 3}

Js.log(f["Content-Type"])

let ff = x => {
  x["Hi"]
  // x## "hi#";
  x["Content-Type"] = "hello"
  Js.log(({"Content-Type": "hello"})["Content-Type"])
}
