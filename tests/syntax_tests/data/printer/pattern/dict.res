let someDict = dict{
  "one": "one",
}

let dict{"one": ?one} = someDict

let foo = () => {
  let _ = switch someDict {
  // Comment here
  | dict{
      // Comment above the pattern
      "one":
        // Comment right in the pattern
        "one",
    } =>
    Js.log("one")
  | _ => Js.log("not one")
  }

  let _ = switch someDict {
  | dict{
      // foo
      "one": "one",
      // bar
      "twoooooooooo": 2,
      // baz
      "three": 3,
      "fooooour": 4,
      "fiiiive": 5,
    } =>
    Js.log("one")
  | _ => Js.log("not one")
  }

  let _ = switch someDict {
  | dict{
      /* foo */
      /* baz */ "one": "one",
      /* bar */
      /* baz */ "twoooooooooo": 2,
      /* baz */
      /* baz */ "three": 3,
      "fooooour": 4,
      "fiiiive": 5,
    } =>
    Js.log("one")
  | _ => Js.log("not one")
  }
}
