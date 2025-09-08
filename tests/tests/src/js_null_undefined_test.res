open Js_null_undefined

let suites = {
  open Mt
  list{
    ("toOption - null", _ => Eq(None, toOption(null))),
    ("toOption - undefined", _ => Eq(None, toOption(undefined))),
    ("toOption - empty", _ => Eq(None, toOption(undefined))),
    (__LOC__, _ => Eq(Some("foo"), toOption(return("foo")))),
    ("return", _ => Eq(Some("something"), toOption(return("something")))),
    ("test - null", _ => Eq(true, isNullable(null))),
    ("test - undefined", _ => Eq(true, isNullable(undefined))),
    ("test - empty", _ => Eq(true, isNullable(undefined))),
    (__LOC__, _ => Eq(true, isNullable(return()))),
    ("bind - null", _ => StrictEq(null, bind(null, v => v))),
    ("bind - undefined", _ => StrictEq(undefined, bind(undefined, v => v))),
    ("bind - empty", _ => StrictEq(undefined, bind(undefined, v => v))),
    ("bind - 'a", _ => Eq(return(4), bind(return(2), n => n * 2))),
    (
      "iter - null",
      _ => {
        let hit = ref(false)
        let _ = iter(null, _ => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - undefined",
      _ => {
        let hit = ref(false)
        let _ = iter(undefined, _ => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - empty",
      _ => {
        let hit = ref(false)
        let _ = iter(undefined, _ => hit := true)
        Eq(false, hit.contents)
      },
    ),
    (
      "iter - 'a",
      _ => {
        let hit = ref(0)
        let _ = iter(return(2), v => hit := v)
        Eq(2, hit.contents)
      },
    ),
    ("fromOption - None", _ => Eq(undefined, fromOption(None))),
    ("fromOption - Some", _ => Eq(return(2), fromOption(Some(2)))),
    ("null <> undefined", _ => Ok(null != undefined)),
    ("null <> empty", _ => Ok(null != undefined)),
    ("undefined = empty", _ => Ok(undefined == undefined)),
    (
      __LOC__,
      _ => Ok({
        let null = 3
        !Js.isNullable(Js.Nullable.return(null))
      }),
    ),
  }
}
Mt.from_pair_suites(__MODULE__, suites)
