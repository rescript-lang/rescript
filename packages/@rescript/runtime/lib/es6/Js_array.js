


function copyWithin(to_, obj) {
  return obj.copyWithin(to_);
}

function copyWithinFrom(to_, from, obj) {
  return obj.copyWithin(to_, from);
}

function copyWithinFromRange(to_, start, end_, obj) {
  return obj.copyWithin(to_, start, end_);
}

function fillInPlace(arg1, obj) {
  return obj.fill(arg1);
}

function fillFromInPlace(arg1, from, obj) {
  return obj.fill(arg1, from);
}

function fillRangeInPlace(arg1, start, end_, obj) {
  return obj.fill(arg1, start, end_);
}

function push(arg1, obj) {
  return obj.push(arg1);
}

function pushMany(arg1, obj) {
  return obj.push(...arg1);
}

function sortInPlaceWith(arg1, obj) {
  return obj.sort(arg1);
}

function spliceInPlace(pos, remove, add, obj) {
  return obj.splice(pos, remove, ...add);
}

function removeFromInPlace(pos, obj) {
  return obj.splice(pos);
}

function removeCountInPlace(pos, count, obj) {
  return obj.splice(pos, count);
}

function unshift(arg1, obj) {
  return obj.unshift(arg1);
}

function unshiftMany(arg1, obj) {
  return obj.unshift(...arg1);
}

function concat(arg1, obj) {
  return obj.concat(arg1);
}

function concatMany(arg1, obj) {
  return obj.concat(...arg1);
}

function includes(arg1, obj) {
  return obj.includes(arg1);
}

function indexOf(arg1, obj) {
  return obj.indexOf(arg1);
}

function indexOfFrom(arg1, from, obj) {
  return obj.indexOf(arg1, from);
}

function joinWith(arg1, obj) {
  return obj.join(arg1);
}

function lastIndexOf(arg1, obj) {
  return obj.lastIndexOf(arg1);
}

function lastIndexOfFrom(arg1, from, obj) {
  return obj.lastIndexOf(arg1, from);
}

function slice(start, end_, obj) {
  return obj.slice(start, end_);
}

function sliceFrom(arg1, obj) {
  return obj.slice(arg1);
}

function every(arg1, obj) {
  return obj.every(arg1);
}

function everyi(arg1, obj) {
  return obj.every(arg1);
}

function filter(arg1, obj) {
  return obj.filter(arg1);
}

function filteri(arg1, obj) {
  return obj.filter(arg1);
}

function find(arg1, obj) {
  return obj.find(arg1);
}

function findi(arg1, obj) {
  return obj.find(arg1);
}

function findIndex(arg1, obj) {
  return obj.findIndex(arg1);
}

function findIndexi(arg1, obj) {
  return obj.findIndex(arg1);
}

function forEach(arg1, obj) {
  obj.forEach(arg1);
}

function forEachi(arg1, obj) {
  obj.forEach(arg1);
}

function map(arg1, obj) {
  return obj.map(arg1);
}

function mapi(arg1, obj) {
  return obj.map(arg1);
}

function reduce(arg1, arg2, obj) {
  return obj.reduce(arg1, arg2);
}

function reducei(arg1, arg2, obj) {
  return obj.reduce(arg1, arg2);
}

function reduceRight(arg1, arg2, obj) {
  return obj.reduceRight(arg1, arg2);
}

function reduceRighti(arg1, arg2, obj) {
  return obj.reduceRight(arg1, arg2);
}

function some(arg1, obj) {
  return obj.some(arg1);
}

function somei(arg1, obj) {
  return obj.some(arg1);
}

export {
  copyWithin,
  copyWithinFrom,
  copyWithinFromRange,
  fillInPlace,
  fillFromInPlace,
  fillRangeInPlace,
  push,
  pushMany,
  sortInPlaceWith,
  spliceInPlace,
  removeFromInPlace,
  removeCountInPlace,
  unshift,
  unshiftMany,
  concat,
  concatMany,
  includes,
  indexOf,
  indexOfFrom,
  joinWith,
  lastIndexOf,
  lastIndexOfFrom,
  slice,
  sliceFrom,
  every,
  everyi,
  filter,
  filteri,
  find,
  findi,
  findIndex,
  findIndexi,
  forEach,
  forEachi,
  map,
  mapi,
  reduce,
  reducei,
  reduceRight,
  reduceRighti,
  some,
  somei,
}
/* No side effect */
