module F = Gpr_3865_foo
module B = Gpr_3865_bar.Make(Gpr_3865_foo)

Js.log(F.return)
Js.log(B.return)
