module type S = {
  type t<_> = Indexed(int): t<int>
}
