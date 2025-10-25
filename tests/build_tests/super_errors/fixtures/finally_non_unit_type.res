let f = () => {
  try {
    Console.log("hello")
  } catch {
  | _ => ()
  } finally {
    42
  }
}
