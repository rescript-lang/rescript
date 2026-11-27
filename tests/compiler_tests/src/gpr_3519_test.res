type arra<'a> = array<'a>

@send @variadic external f0: (int, int, int, array<int>) => unit = "f0"

@send @variadic external f1: (int, int, int, ~y: array<int>) => unit = "f1"
