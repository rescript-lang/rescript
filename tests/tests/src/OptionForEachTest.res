let testSome = (opt: option<int>) => {
  opt->Option.forEach(x => Js.log(x))
}

let testNone = () => {
  let opt: option<string> = None
  opt->Option.forEach(x => Js.log(x))
}

let testWithSideEffect = () => {
  let counter = ref(0)
  Some(42)->Option.forEach(_ => counter := counter.contents + 1)
  None->Option.forEach(_ => counter := counter.contents + 1)
}

external directPrimitive: (option<'a>, 'a => unit) => unit = "%option_for_each"

let testDirect = (opt: option<float>) => {
  directPrimitive(opt, x => Js.log(x))
}
