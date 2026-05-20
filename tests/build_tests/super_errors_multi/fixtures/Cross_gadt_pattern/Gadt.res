type rec t<_> =
  | Int(int): t<int>
  | Pair(t<'a>, t<'b>): t<('a, 'b)>
