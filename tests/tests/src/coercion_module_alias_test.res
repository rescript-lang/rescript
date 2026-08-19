let l = Console.log
module C = String

module C': module type of String = C
l(C'.fromCharCode(66))

module C''': module type of C = C' /* fails */

module C'': module type of String = C
l(C''.fromCharCode(66))

module C3 = {
  include String
}
l(C3.fromCharCode(66))

let f = x => {
  module M = {
    module L = List
  }
  M.L.length(x)
}
let g = x => {
  module L = List
  L.length(L.map(x, n => n + 1))
}

module F = (X: {}) => String
module C4 = F()
l(C4.fromCharCode(66))

module G = (X: {}) => X /* does not alias X */
module M = G()

module M' = {
  module N = {
    let x = 1
  }
  module N' = N
}
l(M'.N'.x)

module M'': {
  module N': {
    let x: int
  }
} = (M')
l(M''.N'.x)
module M2 = {
  include M'
}
module M3: {
  module N': {
    let x: int
  }
} = {
  include M'
}
l(M3.N'.x)
module M3': {
  module N': {
    let x: int
  }
} = (M2)
l(M3'.N'.x)

module M4: {
  module N': {
    let x: int
  }
} = {
  module N = {
    let x = 1
  }
  module N' = N
}
l(M4.N'.x)

module F0 = (X: {}) => {
  module N = {
    let x = 1
  }
  module N' = N
}
module G0: (X: {}) =>
{
  module N': {
    let x: int
  }
} = F0
module M5 = G0()
l(M5.N'.x)

module M6 = {
  module D = {
    let y = 3
  }
  module N = {
    let x = 1
  }
  module N' = N
}

module M1: {
  module N: {
    let x: int
  }
  module N' = N
} = M6
l(M1.N'.x)
module M7: {
  module N': {
    let x: int
  }
} = (M6: {
  module N: {
    let x: int
  }
  module N' = N
})
l(M7.N'.x)

open M6
l(N'.x)

module M8 = {
  module C = String
  module C' = C
}
module M9: {
  module C: {
    let fromCharCode: int => string
  }
  module C' = C
} = M8
l(M9.C'.fromCharCode(66))
module M10: {
  module C': {
    let fromCharCode: int => string
  }
} = (M8: {
  module C: {
    let fromCharCode: int => string
  }
  module C' = C
})
l(M10.C'.fromCharCode(66))
