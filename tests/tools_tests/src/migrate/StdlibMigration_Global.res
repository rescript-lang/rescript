let t1: Js.Global.timeoutId = Js.Global.setTimeout(() => (), 1000)
let t2: Js.Global.timeoutId = Js.Global.setTimeoutFloat(() => (), 1000.0)

Js.Global.clearTimeout(t1)

let i1: Js.Global.intervalId = Js.Global.setInterval(() => (), 2000)
let i2: Js.Global.intervalId = Js.Global.setIntervalFloat(() => (), 2000.0)

Js.Global.clearInterval(i1)

let e1 = Js.Global.encodeURI("https://rescript-lang.org?array=[someValue]")
let d1 = Js.Global.decodeURI("https://rescript-lang.org?array=%5BsomeValue%5D")

let e2 = Js.Global.encodeURIComponent("array=[someValue]")
let d2 = Js.Global.decodeURIComponent("array%3D%5BsomeValue%5D")
