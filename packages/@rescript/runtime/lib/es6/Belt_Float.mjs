


function fromString(i) {
  let i$1 = parseFloat(i);
  if (isNaN(i$1)) {
    return;
  } else {
    return i$1;
  }
}

export {
  fromString,
}
/* No side effect */
