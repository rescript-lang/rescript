let f = (type t, xs : list<t>) => ()
let f = (type t, xs : list<t>, type s, ys : list<s>) => ()
let f = (type t u v, xs : list<(t, u, v)>) => ()
let f = (type t u v, xs : list<(t, u, v)>, type s w z, ys : list<(s, w, z)>) => ()
let f = @attr (type t u v, xs : list<(t, u, v)>, @attr2 type s w z, ys : list<(s, w, z)>) => ()
let f = (@attr type t, @attr type s, xs: list<(t, s)>,  @attr type u, @attr type v w, ys: list<(u, v, w)>) => ()


let cancel_and_collect_callbacks:
  'a 'u 'c.
  (list<packed_callbacks>, promise<'a, 'u, 'c>) => list<packed_callbacks>
 =
  (type x, callbacks_accumulator, p: promise<_, _, c>) => ();
