type t<'a0, 'a1> = 'a0 => 'a1

let f0 = () => 0
let f1 = a0 => a0
let f2 = (a0, a1) => (a0, a1)

f0()->Console.log
f1(0)->Console.log

f2(0, 1)->Console.log

let rec xx = () => xx()

type logger = {log2: 'a. (string, 'a) => unit}

let log2 = (logger, message, obj) => logger.log2(message, obj)
