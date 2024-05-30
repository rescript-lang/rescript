// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function $plus$colon(_f, _g) {
  while(true) {
    let g = _g;
    let f = _f;
    if (f.TAG === "Int") {
      let n = f._0;
      if (g.TAG === "Int") {
        return {
          TAG: "Int",
          _0: n + g._0 | 0
        };
      }
      if (n === 0) {
        return g;
      }
      
    }
    switch (g.TAG) {
      case "Int" :
          if (g._0 !== 0) {
            return {
              TAG: "Add",
              _0: f,
              _1: g
            };
          } else {
            return f;
          }
      case "Add" :
          _g = g._1;
          _f = $plus$colon(f, g._0);
          continue;
      case "Var" :
      case "Mul" :
          return {
            TAG: "Add",
            _0: f,
            _1: g
          };
      
    }
  };
}

function $star$colon(_f, _g) {
  while(true) {
    let g = _g;
    let f = _f;
    let exit = 0;
    let exit$1 = 0;
    if (f.TAG === "Int") {
      let n = f._0;
      if (g.TAG === "Int") {
        return {
          TAG: "Int",
          _0: Math.imul(n, g._0)
        };
      }
      if (n === 0) {
        return {
          TAG: "Int",
          _0: 0
        };
      }
      exit$1 = 3;
    } else {
      exit$1 = 3;
    }
    if (exit$1 === 3) {
      if (g.TAG === "Int") {
        if (g._0 === 0) {
          return {
            TAG: "Int",
            _0: 0
          };
        }
        exit = 2;
      } else {
        exit = 2;
      }
    }
    if (exit === 2 && f.TAG === "Int" && f._0 === 1) {
      return g;
    }
    switch (g.TAG) {
      case "Int" :
          if (g._0 !== 1) {
            return {
              TAG: "Mul",
              _0: f,
              _1: g
            };
          } else {
            return f;
          }
      case "Var" :
      case "Add" :
          return {
            TAG: "Mul",
            _0: f,
            _1: g
          };
      case "Mul" :
          _g = g._1;
          _f = $star$colon(f, g._0);
          continue;
      
    }
  };
}

function simplify(x) {
  switch (x.TAG) {
    case "Int" :
    case "Var" :
        return x;
    case "Add" :
        return $plus$colon(simplify(x._0), simplify(x._1));
    case "Mul" :
        return $star$colon(simplify(x._0), simplify(x._1));
    
  }
}

exports.$plus$colon = $plus$colon;
exports.$star$colon = $star$colon;
exports.simplify = simplify;
/* No side effect */
