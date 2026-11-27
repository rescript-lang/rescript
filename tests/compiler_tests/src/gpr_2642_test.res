type rec t =
  | Pident(string)
  | Pdot(t, string, int)
  | Papply(t, t)

let rec isfree = (id, x) =>
  switch x {
  | Pident(id') => id == id'
  | Pdot(p, s, pos) => isfree(id, p)
  /* if isfree id p then
    true
  else false */
  | Papply(p1, p2) => isfree(id, p1) || isfree(id, p2)
  }
