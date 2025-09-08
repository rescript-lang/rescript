@raises(exit)
let testTop = x => InnerModules.wrapExitTop(x)

@raises(exit)
let testM1 = x => InnerModules.M1.wrapExitM1(x)

@raises(exit)
let testM2 = x => InnerModules.M1.M2.wrapExitM2(x)
