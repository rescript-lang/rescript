let f = (type t, xs : list<t>) => ()
let f = @attr (type t, xs : list<t>) => ()
let f = (type t, xs : list<t>, type s, ys : list<s>) => ()
let f = @attr (type t, xs : list<t>, @attr2 type s, ys : list<s>) => ()
let f = (type t u v, xs : list<(t, u, v)>) => ()
let f = @attr (type t u v, xs : list<(t, u, v)>) => ()
let f = (type t u v, xs : list<(t, u, v)>, type s w z, ys : list<(s, w, z)>) => ()
let f = @attr (type t u v, xs : list<(t, u, v)>, @attr2 type s w z, ys : list<(s, w, z)>) => ()
let f = (@attr type t, @attr type s, xs: list<(t, s)>,  @attr type u, @attr type v w, ys: list<(u, v, w)>) => ()

let mk_formatting_gen:
  type a b c d e f. formatting_gen<a, b, c, d, e, f> => Parsetree.expression =
  fmting =>
    switch fmting {
    | Open_tag(Format(fmt', str')) => mk_constr("Open_tag", list{mk_format(fmt', str')})
    }

let cancel_and_collect_callbacks:
  'a 'u 'c.
  (list<packed_callbacks>, promise<'a, 'u, 'c>) => list<packed_callbacks>
 =
  (type x, callbacks_accumulator, p: promise<_, _, c>) => ();
