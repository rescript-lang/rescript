type rec t = [ | #X ]
type rec t = [>  ]
type rec t = [> s ]
type rec t = [< s ]
type rec t = [ | s ]
type rec t = [> | s ]
type rec t = [< | s ]
type rec t = [ | #a ]
type rec t = [> | #a ]
type rec t = [< | #a ]
type rec t = [ | #a ]
type rec t = [> | #a ]
type rec t = [< | #a ]
type rec t = [ | s | t ]
type rec t = [> | #a ]
type rec t = [> | #a ]
type rec t = [< | #A(int) ]
type rec t = [< #A(int) ]
type rec t = [< | #A & (int) ]
type rec t = [< #A & (int) ]
type rec t = [< | #A & (int) ]
type rec t = [< | #A(int) & (int) | #B ]
type rec t = [< | #A & (int) & (int) | #B ]

type rec t = [ | #a | #b | #c ]
type rec t = [
| #aaaaa
| #bbbbb
| #ccccc
| #ddddd
| #eeeee
| #fffff
| #ggggg
| #hhhhh
| #jjjjj
| #kkkkk
]
type rec t = Aaaaaa | Bbbbbb | Cccccc | Dddddd | Eeeeee | Ffffff | Gggggg | Hhhhhh | Jjjjjj | Kkkkkk

type rec t = [ | #a(int) ]
type rec t = [<
| #A(int)
| #B(int, int)
| #C(float)
| #D(int) & (string)
| #E(int, int) & (string, string, string) & (float)
| #F & (string)
]

type rec z = [< | #A(string) & (int) | #B ]

type z = [< | #A | #B > #X ]

type z = [< | #A | #B > #X #Y ]

type rec vlist<'a> = [ | #Nil | #Cons('a, vlist<'a>) ]

type t = [ @a @b @c #x(int) ]
type t = [ | @a @b @c #x(int) | @d @e foo ]
type t = [> | @a x]
type t = [> @a x]
type t = [> | @a #x]
type t = [> @a #x]
type t = [< | @a #x]
type t = [< @a #x]

type t = [<  @one #a((int, int),int) | @two int | @three #b(string) > #w #x #y  ]

// type z = @q [ @a @b @c #z]

let f = (x: [ | #b]) => x