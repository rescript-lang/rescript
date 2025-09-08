@@uncurried

let canUseCanvas: unit => bool = %ffi(`
  function canUseCanvas() {
    return !!document.createElement('canvas').getContext;
  }
`)

let add: (int, int) => int = %ffi(`(x,y)=>x+y`)
