// Migrations that will not compile after migration (by design)
let sortInPlaceWith1 = [3, 1, 2]->Js.Array2.sortInPlaceWith((a, b) => a - b)
let sortInPlaceWith2 = Js.Array2.sortInPlaceWith([3, 1, 2], (a, b) => a - b)
