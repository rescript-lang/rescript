open Belt

include (
  {
    module IntMap = Map.Int

    let empty = IntMap.empty

    let m = List.reduceReverse(list{(10, 'a'), (3, 'b'), (7, 'c'), (20, 'd')}, empty, (
      acc,
      (k, v),
    ) => acc->IntMap.set(k, v))

    /* external log : 'a -> unit = "" [@@val "console.log"] */

    let assert_test = () =>
      if m->IntMap.get(10) == Some('a') {
        Console.log("hi")
      } else {
        /* log ('a', "succeed") */

        Console.log("hi")
      }
  }: {}
)
