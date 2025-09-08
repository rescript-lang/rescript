type rec func<'a, 'b, 'i> = 'i => res<'a, 'b, 'i>
@unboxed and res<'a, 'b, 'i> = Val(('b, func<'a, 'b, 'i>))

let rec u = _ => Val(3, u)
