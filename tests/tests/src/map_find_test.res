open Belt

include (
  {
    module IntMap = Map.Int

    let empty = IntMap.empty

    let m = List.reduceReverse(list{(10, 'a'), (3, 'b'), (7, 'c'), (20, 'd')}, empty, (
      acc,
      (k, v),
    ) => acc->IntMap.set(k, v))

    module SMap = Map.String

    let s = List.reduceReverse(list{("10", 'a'), ("3", 'b'), ("7", 'c'), ("20", 'd')}, SMap.empty, (
      acc,
      (k, v),
    ) => acc->SMap.set(k, v))
    @val("console.log") external log: 'a => unit = ""

    Mt.from_pair_suites(__MODULE__, ...)(list{
      ("int", _ => Eq(IntMap.get(m, 10), Some('a'))),
      ("string", _ => Eq(SMap.get(s, "10"), Some('a'))),
    })
  }: {}
)
