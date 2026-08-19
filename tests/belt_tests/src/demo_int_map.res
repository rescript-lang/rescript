open Belt

let test = () => {
  let m = ref(Map.Int.empty)
  let count = 1000_000
  for i in 0 to count {
    m := m.contents->Map.Int.set(i, i)
  }
  for i in 0 to count {
    m.contents->Map.Int.get(i)->ignore
  }
}

let () = /* Console.log "start" ; */
test()
/* Console.log "finish" */
