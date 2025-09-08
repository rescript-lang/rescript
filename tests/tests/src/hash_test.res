open Belt

module Hashtbl = Ocaml_Hashtbl

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (f, x, y) => Mt_global.collect_eq(test_id, suites, f, x, y)

let test_strings = Array.init(32, i => Js.String2.fromCodePoint(i)->Js.String2.repeat(i))

let test_strings_hash_results = [
  0,
  904391063,
  889600889,
  929588010,
  596566298,
  365199070,
  448044845,
  311625091,
  681445541,
  634941451,
  82108334,
  17482990,
  491949228,
  696194769,
  711728152,
  594966620,
  820561748,
  958901713,
  102794744,
  378848504,
  349314368,
  114167579,
  71240932,
  110067399,
  280623927,
  323523937,
  310683234,
  178511779,
  585018975,
  544388424,
  1043872806,
  831138595,
]

let normalize = x => land(x, 0x3FFFFFFF)
let caml_hash = x => normalize(Hashtbl.hash(x))
let () = eq(__LOC__, test_strings->Array.map(caml_hash), test_strings_hash_results)

let () = eq(__LOC__, normalize(Hashtbl.hash(0)), 129913994)

let () = eq(__LOC__, normalize(Hashtbl.hash("x")), 780510073)

let () = eq(__LOC__, normalize(Hashtbl.hash("xy")), 194127723)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
