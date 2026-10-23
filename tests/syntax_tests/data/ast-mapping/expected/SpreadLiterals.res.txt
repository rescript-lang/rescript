let a = [...xs]
let a = [1, ...xs, 2, 3, ...ys, 4]
let a = [...[1, 2], 3, ...[]]
let a = [...[...xs, 1], ...ys]
let a = @attr [1, ...(xs: array<int>)]
let a = [/* before */ ...xs, 1]

let l = list{...xs, 1}
let l = list{1, ...xs, 2, 3, ...ys}
let l = list{...list{1, 2}, ...xs}
let l = list{...list{...xs, 1}, ...xs, 2}
let l = @attr list{...xs, 1}
let l = list{1, ...xs}
let userArraySpread = Primitive_array.spread([[1], xs])
let userListSpread = Primitive_list.spread([list{1}, xs])
