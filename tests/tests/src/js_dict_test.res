open Js_dict

let obj = (): t<'a> => Obj.magic({"foo": 43, "bar": 86})

let suites = {
  open Mt
  list{
    ("empty", _ => Eq([], keys(empty()))),
    ("get", _ => Eq(Some(43), get(obj(), "foo"))),
    ("get - property not in object", _ => Eq(None, get(obj(), "baz"))),
    ("unsafe_get", _ => Eq(43, unsafeGet(obj(), "foo"))),
    (
      "set",
      _ => {
        let o = obj()
        set(o, "foo", 36)
        Eq(Some(36), get(o, "foo"))
      },
    ),
    ("keys", _ => Eq(["foo", "bar"], keys(obj()))),
    ("entries", _ => Eq([("foo", 43), ("bar", 86)], entries(obj()))),
    ("values", _ => Eq([43, 86], values(obj()))),
    ("fromList - []", _ => Eq(empty(), fromList(list{}))),
    ("fromList", _ => Eq([("x", 23), ("y", 46)], entries(fromList(list{("x", 23), ("y", 46)})))),
    ("fromArray - []", _ => Eq(empty(), fromArray([]))),
    ("fromArray", _ => Eq([("x", 23), ("y", 46)], entries(fromArray([("x", 23), ("y", 46)])))),
    ("map", _ => Eq(Obj.magic({"foo": "43", "bar": "86"}), map(i => Js.Int.toString(i), obj()))),
  }
}
Mt.from_pair_suites(__MODULE__, suites)
