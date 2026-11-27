module F = Gpr_3865_foo
module B = Gpr_3865_bar.Make(Gpr_3865_foo)

Console.log(F.return)
Console.log(B.return)
