let f = PathRefsUse.S.add(2)
//                    ^ref

let b = PathRefsUse.S.Box(1)
//                    ^ref

let c = PathRefsUse.Aliased.Box(2)

let g = PathRefsUse.S.add(3)
//                    ^ren plus
