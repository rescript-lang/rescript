@unboxed type r = {f: 'a. 'a => 'a}

let map_pair = (r, (p1, p2)) => (r.f(p1), r.f(p2))

let u = {f: x => x}

let sample = map_pair(u, (3, true))

type para<_> = C
@unboxed type rec t = H(para<_>): t

@unboxed type rec any = Any('a): any

let hi = [Any(3), Any(2), Any("x")]

let dump = (any: any) =>
  switch any {
  | Any(v) => Js.log(v)
  }

let () = {
  dump(Any(3))
  dump(Any("x"))
}

/* https://inbox.ocaml.org/caml-list/CAAxsn=HhhmAAYfSCLzWgMW0Q-duTZNQBLQYDx8yETwWTjm16tw@mail.gmail.com/t/#u */
type rec t3<'a, 'b> = [
  | #A(t3_aux<int, string>)
  | #B
]
@unboxed and t3_aux<'a, 'b> = {field: t3<'a, 'b>}

let rec v0: t3<_> = #A({field: v0})
let rec v1: t3<_> = #A({field: #B})

/*
module rec R:
   sig
     class ['a] container : 'a ->
       object
         method map : 'b. ('a -> 'b) -> 'b R.container_aux
       end
     type 'a container_aux = 
        { container: 'a container }
        [@@unboxed]
   end =
   struct
     class ['a] container (v:'a) = object
       method map : 'b. ('a -> 'b) -> 'b R.container_aux =
         fun f -> { R.container = new R.container (f v) }
     end
     type 'a container_aux = 
        { container: 'a container }
        [@@unboxed]
   end */
