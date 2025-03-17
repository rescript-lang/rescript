


function forEach(iterator, f) {
  let iteratorDone = false;
  while (!iteratorDone) {
    let match = iterator.next();
    f(match.value);
    iteratorDone = match.done;
  };
}

export {
  forEach,
}
/* No side effect */
