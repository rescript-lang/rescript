


function forEach(s, f, action) {
  for (let i = s; i <= f; ++i) {
    action(i);
  }
}

function every(_s, f, p) {
  while (true) {
    let s = _s;
    if (s > f) {
      return true;
    }
    if (!p(s)) {
      return false;
    }
    _s = s + 1 | 0;
    continue;
  };
}

function everyBy(s, f, step, p) {
  if (step > 0) {
    let _s = s;
    while (true) {
      let s$1 = _s;
      if (s$1 > f) {
        return true;
      }
      if (!p(s$1)) {
        return false;
      }
      _s = s$1 + step | 0;
      continue;
    };
  } else {
    return true;
  }
}

function some(_s, f, p) {
  while (true) {
    let s = _s;
    if (s > f) {
      return false;
    }
    if (p(s)) {
      return true;
    }
    _s = s + 1 | 0;
    continue;
  };
}

function someBy(s, f, step, p) {
  if (step > 0) {
    let _s = s;
    while (true) {
      let s$1 = _s;
      if (s$1 > f) {
        return false;
      }
      if (p(s$1)) {
        return true;
      }
      _s = s$1 + step | 0;
      continue;
    };
  } else {
    return false;
  }
}

let forEachU = forEach;

let everyU = every;

let everyByU = everyBy;

let someU = some;

let someByU = someBy;

export {
  forEachU,
  forEach,
  everyU,
  every,
  everyByU,
  everyBy,
  someU,
  some,
  someByU,
  someBy,
}
/* No side effect */
