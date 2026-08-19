module IntMap = Belt.Map.Int

let assertion_test = () => {
  let m = ref(IntMap.empty)
  let count = 1000000
  for i in 0 to count {
    m := m.contents->IntMap.set(i, i)
  }
  for i in 0 to count {
    m.contents->IntMap.get(i)->ignore
  }
}
