exception A(int, bool)

let v = A(3, true)

exception B

let u = B

exception D(int)

let d = D(3)
/* Not allowed */

/* intentionally overridden ,
 so that we can not tell the differrence, only by [id] */
