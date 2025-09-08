module type Signature = {
  type t

  type t<'a, 'b> = node<'a, 'b>

  @attr
  type t

  @onFirst
  type t = s 
  @onSecond
  and s = string
}
