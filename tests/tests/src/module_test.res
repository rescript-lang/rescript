let length = _ => 3

/* Test name collision */
Mt.from_pair_suites(
  __MODULE__,
  list{
    ("list_length", _ => Eq(Belt.List.length(list{1, 2}), 2)),
    ("length", _ => Eq(length(list{1, 2}), 3)),
  },
)
