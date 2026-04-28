let panicTest = () => {
  let caught = try panic("uh oh") catch {
  | JsExn(err) => JsExn.message(err)
  }

  Test.run(__POS_OF__("Should resolve test"), caught, \"===", Some("Panic! uh oh"))
}

panicTest()
