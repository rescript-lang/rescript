// Generated by ReScript, PLEASE EDIT WITH CARE


function map(f, x) {
  if (x === 0) {
    return /* [] */0;
  }
  let r = f(x.hd);
  return {
    hd: r,
    tl: map(f, x.tl)
  };
}

export {
  map,
}
/* No side effect */
