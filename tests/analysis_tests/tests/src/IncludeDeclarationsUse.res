let w = IncludeDeclarations.v0 + IncludeDeclarations.v1 + IncludeDeclarations.fv
let b = IncludeDeclarations.bv
//                          ^ref
let l = IncludeDeclarations.L1
let rf = (r: IncludeDeclarations.r) => r.rf
let externalDot = IncludeDeclarations.Dot
//                                    ^ref
let externalLf = (r: IncludeDeclarations.lr) => r.lf
//                                                ^ref
let externalMade = IncludeDeclarations.made
//                                     ^ref
let upJa = IncludeDeclarationsUnpack.Ja
//                                   ^ref
let upGa = (r: IncludeDeclarationsUnpack.rj) => r.ga
//                                                ^ref
