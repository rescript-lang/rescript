let handle = (ev: [#Click | #KeyDown]) =>
  switch ev {
  | #Click => Console.log("clicked")
  | #KeyDown => Console.log("key down")
  }

let _ = handle(#Resize)
