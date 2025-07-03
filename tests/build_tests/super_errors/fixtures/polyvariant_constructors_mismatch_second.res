let handle = (ev: [#Click | #KeyDown]) =>
  switch ev {
  | #Click => Js.log("clicked")
  | #KeyDown => Js.log("key down")
  }

let _ = handle(#Resize)
