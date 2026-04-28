let l1 = List.headExn(list{1})
let l2 = List.tailExn(list{1})
let l3 = List.getExn(list{"a", "b"}, 0)
let l4 = List.toShuffled(list{1, 2})

let lazy1 = Lazy.from_fun(() => 1)
let lazy2 = Lazy.from_val(2)
let v1 = Lazy.force(lazy1)
let v2 = Lazy.force_val(lazy2)
let b1 = Lazy.is_val(lazy1)

let p = Promise.resolve(5)
let _ = Promise.done(p)
