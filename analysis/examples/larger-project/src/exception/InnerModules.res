@raises(exit)
let wrapExitTop = x => exit(x)

module M1 = {
  @raises(exit)
  let wrapExitM1 = x => exit(x)

  @raises(exit)
  let callLocally = x => wrapExitM1(x)

  @raises(exit)
  let callTop = x => wrapExitTop(x)

  module M2 = {
    @raises(exit)
    let wrapExitM2 = x => exit(x)

    @raises(exit)
    let callM1 = x => wrapExitM1(x)

    @raises(exit)
    let callTop = x => wrapExitTop(x)
  }
}

@raises(exit)
let callM1 = x => M1.wrapExitM1(x)
