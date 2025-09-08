open Belt
module String = Ocaml_String

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

Js.log(`你好，
世界`)

Js.log(`\x3f\u003f\b\t\n\v\f\r\0"'`)

let convert = (s: string): list<int> =>
  List.fromArray(
    Js_array2.fromMap(Js_string.castToArrayLike(s), x =>
      switch Js_string2.codePointAt(x, 0) {
      | None => assert(false)
      | Some(x) => x
      }
    ),
  )

let () = {
  eq(
    __LOC__,
    `你好，
世界`,
    `你好，\n世界`,
  )
  eq(
    __LOC__,
    convert(`汉字是世界上最美丽的character`),
    list{
      27721,
      23383,
      26159,
      19990,
      30028,
      19978,
      26368,
      32654,
      20029,
      30340,
      99,
      104,
      97,
      114,
      97,
      99,
      116,
      101,
      114,
    },
  )
  eq(__LOC__, convert(`\x3f\x3fa`), list{63, 63, 97})
  eq(__LOC__, convert(`??a`), list{63, 63, 97})
  eq(__LOC__, convert(`\u003f\x3fa`), list{63, 63, 97})
  eq(__LOC__, convert(`🚀🚀a`), list{128640, 128640, 97})
  eq(__LOC__, convert(`\uD83D\uDE80a`), list{128640, 97})
  eq(__LOC__, convert(`\uD83D\uDE80\x3f`), list{128640, 63})

  /* It is amazing Array.from(string)
   is unicode safe */
  eq(__LOC__, convert(`\uD83D\uDE80\uD83D\uDE80a`), list{128640, 128640, 97})

  eq("No inline string length", String.length(`\uD83D\uDE80\0`), 3)

  /* eq __LOC__
   (Js.String.codePointAt 0 {js|\uD83D\uDE80\0|js} ) 128640; */
  eq(
    __LOC__,
    (String.get(`\uD83D\uDE80\0`, 0) :> int),
    /* TODO: Char.code need normalization? */
    128640,
  )
  eq(__LOC__, (String.get(`🚀`, 0) :> int), 128640)

  /* "\uD83D\uDE80".charCodeAt(0) & 255
   61 */

  eq(__LOC__, convert(`\uD83D\uDE80`), list{128640})
  eq(__LOC__, convert(`\uD83D\uDE80\uD83D\uDE80`), list{128640, 128640})
  eq(__LOC__, convert(` \b\t\n\v\f\ra`), list{32, 8, 9, 10, 11, 12, 13, 97})
  /* we don't need escape string double quote {|"|}and single quote{|'|}
    however when we print it, we need escape them
    there is no need for line continuation,

 */
  eq(__LOC__, convert(` \b\t\n\v\f\r"'\\\0a`), list{32, 8, 9, 10, 11, 12, 13, 34, 39, 92, 0, 97})
}
let () = Mt.from_pair_suites(__MODULE__, suites.contents)
