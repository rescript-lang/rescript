let tst = () =>
  for i in {
    Js.log("hi")
    0
  } to {
    Js.log("hello")
    3
  } {
    ()
  }

let test2 = () => {
  let v = ref(0)
  for i in {
    v := 3
    0
  } to {
    v := 10
    1
  } {
    ()
  }
  v.contents
}
open Mt

let suites = {
  open Mt
  list{("for_order", _ => Eq(10, test2()))}
}

Mt.from_pair_suites(__MODULE__, suites)
