Js.log(Inner_define.N.add(1, 2))

open Inner_define

let f = x => (N0.f1(x), N0.f2(x, x), N0.f3(x, x, x), N1.f2(x, x))
