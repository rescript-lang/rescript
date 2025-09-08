open Belt

let list_suites = {
  open Mt
  let eq = (a, b) => a == b
  list{
    ("length", _ => Eq(1, List.length(list{(0, 1, 2, 3, 4)})) /* This is tuple haha */),
    ("length2", _ => Eq(5, List.length(list{0, 1, 2, 3, 4})) /* This is tuple haha */),
    (
      "long_length",
      _ => {
        let v = 30_000
        Eq(v, List.length(List.fromArray(Array.init(v, _ => 0))))
      },
    ),
    (
      "sort",
      _ => Eq(
        list{4, 1, 2, 3}->List.sort((x: int, y) => Pervasives.compare(x, y)),
        list{1, 2, 3, 4},
      ),
    ),
    (__LOC__, _ => Eq(true, List.has(list{1, 2, 3}, 3, eq))),
    (__LOC__, _ => Eq(false, List.has(list{1, 2, 3}, 4, eq))),
    (__LOC__, _ => Eq(Some(9), List.getAssoc(list{(1, 2), (4, 9)}, 4, eq))),
  }
}

let () = Mt.from_pair_suites(__MODULE__, list_suites)
