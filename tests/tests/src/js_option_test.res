let simpleEq = (a: int, b) => a == b

let option_suites = {
  open Mt
  list{
    ("option_isSome_Some", _ => Eq(true, Js.Option.isSome(Some(1)))),
    ("option_isSome_None", _ => Eq(false, Js.Option.isSome(None))),
    ("option_isNone_Some", _ => Eq(false, Js.Option.isNone(Some(1)))),
    ("option_isNone_None", _ => Eq(true, Js.Option.isNone(None))),
    ("option_isSomeValue_Eq", _ => Eq(true, Js.Option.isSomeValue(simpleEq, 2, Some(2)))),
    ("option_isSomeValue_Diff", _ => Eq(false, Js.Option.isSomeValue(simpleEq, 1, Some(2)))),
    ("option_isSomeValue_DiffNone", _ => Eq(false, Js.Option.isSomeValue(simpleEq, 1, None))),
    ("option_getExn_Some", _ => Eq(2, Js.Option.getExn(Some(2)))),
    ("option_equal_Eq", _ => Eq(true, Js.Option.equal(simpleEq, Some(2), Some(2)))),
    ("option_equal_Diff", _ => Eq(false, Js.Option.equal(simpleEq, Some(1), Some(2)))),
    ("option_equal_DiffNone", _ => Eq(false, Js.Option.equal(simpleEq, Some(1), None))),
    (
      "option_andThen_SomeSome",
      _ => Eq(
        true,
        Js.Option.isSomeValue(simpleEq, 3, Js.Option.andThen(a => Some(a + 1), Some(2))),
      ),
    ),
    (
      "option_andThen_SomeNone",
      _ => Eq(false, Js.Option.isSomeValue(simpleEq, 3, Js.Option.andThen(_ => None, Some(2)))),
    ),
    (
      "option_map_Some",
      _ => Eq(true, Js.Option.isSomeValue(simpleEq, 3, Js.Option.map(a => a + 1, Some(2)))),
    ),
    ("option_map_None", _ => Eq(None, Js.Option.map(a => a + 1, None))),
    ("option_default_Some", _ => Eq(2, Js.Option.getWithDefault(3, Some(2)))),
    ("option_default_None", _ => Eq(3, Js.Option.getWithDefault(3, None))),
    (
      "option_filter_Pass",
      _ => Eq(
        true,
        Js.Option.isSomeValue(simpleEq, 2, Js.Option.filter(a => mod(a, 2) == 0, Some(2))),
      ),
    ),
    ("option_filter_Reject", _ => Eq(None, Js.Option.filter(a => mod(a, 3) == 0, Some(2)))),
    ("option_filter_None", _ => Eq(None, Js.Option.filter(a => mod(a, 3) == 0, None))),
    (
      "option_firstSome_First",
      _ => Eq(true, Js.Option.isSomeValue(simpleEq, 3, Js.Option.firstSome(Some(3), Some(2)))),
    ),
    (
      "option_firstSome_First",
      _ => Eq(true, Js.Option.isSomeValue(simpleEq, 2, Js.Option.firstSome(None, Some(2)))),
    ),
    ("option_firstSome_None", _ => Eq(None, Js.Option.firstSome(None, None))),
  }
}

let () = Mt.from_pair_suites(__MODULE__, option_suites)
