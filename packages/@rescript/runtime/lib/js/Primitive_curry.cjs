'use strict';


function app(_f, _args) {
  while (true) {
    let args = _args;
    let f = _f;
    let init_arity = f.length;
    let arity = init_arity === 0 ? 1 : init_arity;
    let len = args.length;
    let d = arity - len | 0;
    if (d === 0) {
      return f.apply(null, args);
    }
    if (d >= 0) {
      return x => app(f, args.concat([x]));
    }
    _args = args.slice(arity, len);
    _f = f.apply(null, args.slice(0, arity));
    continue;
  };
}

function __1(o) {
  let arity = o.length;
  if (arity === 1) {
    return o;
  } else {
    return a0 => {
      let arity = o.length;
      if (arity === 1) {
        return o(a0);
      } else {
        switch (arity) {
          case 1 :
            return o(a0);
          case 2 :
            return param => o(a0, param);
          case 3 :
            return (param, param$1) => o(a0, param, param$1);
          case 4 :
            return (param, param$1, param$2) => o(a0, param, param$1, param$2);
          case 5 :
            return (param, param$1, param$2, param$3) => o(a0, param, param$1, param$2, param$3);
          case 6 :
            return (param, param$1, param$2, param$3, param$4) => o(a0, param, param$1, param$2, param$3, param$4);
          case 7 :
            return (param, param$1, param$2, param$3, param$4, param$5) => o(a0, param, param$1, param$2, param$3, param$4, param$5);
          default:
            return app(o, [a0]);
        }
      }
    };
  }
}

function __2(o) {
  let arity = o.length;
  if (arity === 2) {
    return o;
  } else {
    return (a0, a1) => {
      let arity = o.length;
      if (arity === 2) {
        return o(a0, a1);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [a1]);
          case 2 :
            return o(a0, a1);
          case 3 :
            return param => o(a0, a1, param);
          case 4 :
            return (param, param$1) => o(a0, a1, param, param$1);
          case 5 :
            return (param, param$1, param$2) => o(a0, a1, param, param$1, param$2);
          case 6 :
            return (param, param$1, param$2, param$3) => o(a0, a1, param, param$1, param$2, param$3);
          case 7 :
            return (param, param$1, param$2, param$3, param$4) => o(a0, a1, param, param$1, param$2, param$3, param$4);
          default:
            return app(o, [
              a0,
              a1
            ]);
        }
      }
    };
  }
}

function __3(o) {
  let arity = o.length;
  if (arity === 3) {
    return o;
  } else {
    return (a0, a1, a2) => {
      let arity = o.length;
      if (arity === 3) {
        return o(a0, a1, a2);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [
              a1,
              a2
            ]);
          case 2 :
            return app(o(a0, a1), [a2]);
          case 3 :
            return o(a0, a1, a2);
          case 4 :
            return param => o(a0, a1, a2, param);
          case 5 :
            return (param, param$1) => o(a0, a1, a2, param, param$1);
          case 6 :
            return (param, param$1, param$2) => o(a0, a1, a2, param, param$1, param$2);
          case 7 :
            return (param, param$1, param$2, param$3) => o(a0, a1, a2, param, param$1, param$2, param$3);
          default:
            return app(o, [
              a0,
              a1,
              a2
            ]);
        }
      }
    };
  }
}

function __4(o) {
  let arity = o.length;
  if (arity === 4) {
    return o;
  } else {
    return (a0, a1, a2, a3) => {
      let arity = o.length;
      if (arity === 4) {
        return o(a0, a1, a2, a3);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [
              a1,
              a2,
              a3
            ]);
          case 2 :
            return app(o(a0, a1), [
              a2,
              a3
            ]);
          case 3 :
            return app(o(a0, a1, a2), [a3]);
          case 4 :
            return o(a0, a1, a2, a3);
          case 5 :
            return param => o(a0, a1, a2, a3, param);
          case 6 :
            return (param, param$1) => o(a0, a1, a2, a3, param, param$1);
          case 7 :
            return (param, param$1, param$2) => o(a0, a1, a2, a3, param, param$1, param$2);
          default:
            return app(o, [
              a0,
              a1,
              a2,
              a3
            ]);
        }
      }
    };
  }
}

function __5(o) {
  let arity = o.length;
  if (arity === 5) {
    return o;
  } else {
    return (a0, a1, a2, a3, a4) => {
      let arity = o.length;
      if (arity === 5) {
        return o(a0, a1, a2, a3, a4);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [
              a1,
              a2,
              a3,
              a4
            ]);
          case 2 :
            return app(o(a0, a1), [
              a2,
              a3,
              a4
            ]);
          case 3 :
            return app(o(a0, a1, a2), [
              a3,
              a4
            ]);
          case 4 :
            return app(o(a0, a1, a2, a3), [a4]);
          case 5 :
            return o(a0, a1, a2, a3, a4);
          case 6 :
            return param => o(a0, a1, a2, a3, a4, param);
          case 7 :
            return (param, param$1) => o(a0, a1, a2, a3, a4, param, param$1);
          default:
            return app(o, [
              a0,
              a1,
              a2,
              a3,
              a4
            ]);
        }
      }
    };
  }
}

function __6(o) {
  let arity = o.length;
  if (arity === 6) {
    return o;
  } else {
    return (a0, a1, a2, a3, a4, a5) => {
      let arity = o.length;
      if (arity === 6) {
        return o(a0, a1, a2, a3, a4, a5);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [
              a1,
              a2,
              a3,
              a4,
              a5
            ]);
          case 2 :
            return app(o(a0, a1), [
              a2,
              a3,
              a4,
              a5
            ]);
          case 3 :
            return app(o(a0, a1, a2), [
              a3,
              a4,
              a5
            ]);
          case 4 :
            return app(o(a0, a1, a2, a3), [
              a4,
              a5
            ]);
          case 5 :
            return app(o(a0, a1, a2, a3, a4), [a5]);
          case 6 :
            return o(a0, a1, a2, a3, a4, a5);
          case 7 :
            return param => o(a0, a1, a2, a3, a4, a5, param);
          default:
            return app(o, [
              a0,
              a1,
              a2,
              a3,
              a4,
              a5
            ]);
        }
      }
    };
  }
}

function __7(o) {
  let arity = o.length;
  if (arity === 7) {
    return o;
  } else {
    return (a0, a1, a2, a3, a4, a5, a6) => {
      let arity = o.length;
      if (arity === 7) {
        return o(a0, a1, a2, a3, a4, a5, a6);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [
              a1,
              a2,
              a3,
              a4,
              a5,
              a6
            ]);
          case 2 :
            return app(o(a0, a1), [
              a2,
              a3,
              a4,
              a5,
              a6
            ]);
          case 3 :
            return app(o(a0, a1, a2), [
              a3,
              a4,
              a5,
              a6
            ]);
          case 4 :
            return app(o(a0, a1, a2, a3), [
              a4,
              a5,
              a6
            ]);
          case 5 :
            return app(o(a0, a1, a2, a3, a4), [
              a5,
              a6
            ]);
          case 6 :
            return app(o(a0, a1, a2, a3, a4, a5), [a6]);
          case 7 :
            return o(a0, a1, a2, a3, a4, a5, a6);
          default:
            return app(o, [
              a0,
              a1,
              a2,
              a3,
              a4,
              a5,
              a6
            ]);
        }
      }
    };
  }
}

function __8(o) {
  let arity = o.length;
  if (arity === 8) {
    return o;
  } else {
    return (a0, a1, a2, a3, a4, a5, a6, a7) => {
      let arity = o.length;
      if (arity === 8) {
        return o(a0, a1, a2, a3, a4, a5, a6, a7);
      } else {
        switch (arity) {
          case 1 :
            return app(o(a0), [
              a1,
              a2,
              a3,
              a4,
              a5,
              a6,
              a7
            ]);
          case 2 :
            return app(o(a0, a1), [
              a2,
              a3,
              a4,
              a5,
              a6,
              a7
            ]);
          case 3 :
            return app(o(a0, a1, a2), [
              a3,
              a4,
              a5,
              a6,
              a7
            ]);
          case 4 :
            return app(o(a0, a1, a2, a3), [
              a4,
              a5,
              a6,
              a7
            ]);
          case 5 :
            return app(o(a0, a1, a2, a3, a4), [
              a5,
              a6,
              a7
            ]);
          case 6 :
            return app(o(a0, a1, a2, a3, a4, a5), [
              a6,
              a7
            ]);
          case 7 :
            return app(o(a0, a1, a2, a3, a4, a5, a6), [a7]);
          default:
            return app(o, [
              a0,
              a1,
              a2,
              a3,
              a4,
              a5,
              a6,
              a7
            ]);
        }
      }
    };
  }
}

exports.__1 = __1;
exports.__2 = __2;
exports.__3 = __3;
exports.__4 = __4;
exports.__5 = __5;
exports.__6 = __6;
exports.__7 = __7;
exports.__8 = __8;
/* No side effect */
