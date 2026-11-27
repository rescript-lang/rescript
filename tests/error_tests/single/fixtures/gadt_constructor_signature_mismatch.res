module M: {
  type rec t<_> = Int(int): t<int> | Str(string): t<string>
} = {
  type rec t<_> = Int(int): t<int> | Str(string): t<int>
}
