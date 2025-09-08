include (
  {
    module StringMap = Belt.Map.String
    @val("console.time") external time: string => unit = ""
    @val("console.timeEnd") external timeEnd: string => unit = ""

    let timing = (label, f) => {
      time(label)
      f()
      timeEnd(label)
    }

    let assertion_test = () => {
      let m = ref(StringMap.empty)
      let count = 1000000
      timing("building", ...)(_ =>
        for i in 0 to count {
          m := m.contents->StringMap.set(Js.Int.toString(i), Js.Int.toString(i))
        }
      )
      timing("querying", ...)(_ =>
        for i in 0 to count {
          m.contents->StringMap.get(Js.Int.toString(i))->ignore
        }
      )
    }
  }: {
    let assertion_test: unit => unit
  }
)
