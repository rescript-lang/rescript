let a = async () => 3
let foo = async () => {
  let _ = ()
  () => await a()
}
