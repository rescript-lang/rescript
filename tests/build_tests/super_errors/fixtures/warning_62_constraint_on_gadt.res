type rec t<'a> =
  | Int(int): t<int>
  | Bool(bool): t<bool>
constraint 'a = int
