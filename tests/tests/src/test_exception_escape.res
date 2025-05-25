module N: {
  let f: int
} = {
  exception A(int)
  let f = try throw(A(3)) catch {
  | _ => 3
  }
}
