module rec PA: {
  let print: array<int> => unit
} = {
  /* let () = P.print 3 */
  let print = {
    let iter = Belt.Array.forEach(_, P.print)
    a => iter(a)
  }
}
and P: {
  let print: int => unit
} = {
  let print = i => Console.log2("%d", i)
}

let () = PA.print([1, 2])
