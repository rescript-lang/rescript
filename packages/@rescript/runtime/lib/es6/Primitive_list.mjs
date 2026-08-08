


function copyAux(_list, _previous) {
  while (true) {
    let previous = _previous;
    let list = _list;
    if (list === 0) {
      return previous;
    }
    let next = {
      hd: list.hd,
      tl: /* [] */0
    };
    previous.tl = next;
    _previous = next;
    _list = list.tl;
    continue;
  };
}

function concat(left, right) {
  if (left === 0) {
    return right;
  }
  let result = {
    hd: left.hd,
    tl: /* [] */0
  };
  copyAux(left.tl, result).tl = right;
  return result;
}

function spread(lists) {
  let len = lists.length;
  if (len === 1) {
    return lists[0];
  }
  if (len === 0) {
    return /* [] */0;
  }
  let length = lists.length;
  let result = lists[length - 1 | 0];
  for (let i = length - 2 | 0; i >= 0; --i) {
    result = concat(lists[i], result);
  }
  return result;
}

export {
  spread,
}
/* No side effect */
