module Int_map = Belt.Map.Int

let assertion_test = () => {
  let m = ref(Int_map.empty)
  let count = 1000000
  for i in 0 to count {
    m := m.contents->Int_map.set(i, i)
  }
  for i in 0 to count {
    m.contents->Int_map.get(i)->ignore
  }
}
