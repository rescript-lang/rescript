'use strict';


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

exports.copyWithin = copyWithin;
exports.copyWithinFrom = copyWithinFrom;
exports.copyWithinFromRange = copyWithinFromRange;
exports.fillInPlace = fillInPlace;
exports.fillFromInPlace = fillFromInPlace;
exports.fillRangeInPlace = fillRangeInPlace;
exports.push = push;
exports.pushMany = pushMany;
exports.sortInPlaceWith = sortInPlaceWith;
exports.spliceInPlace = spliceInPlace;
exports.removeFromInPlace = removeFromInPlace;
exports.removeCountInPlace = removeCountInPlace;
exports.unshift = unshift;
exports.unshiftMany = unshiftMany;
exports.concat = concat;
exports.concatMany = concatMany;
exports.includes = includes;
exports.indexOf = indexOf;
exports.indexOfFrom = indexOfFrom;
exports.joinWith = joinWith;
exports.lastIndexOf = lastIndexOf;
exports.lastIndexOfFrom = lastIndexOfFrom;
exports.slice = slice;
exports.sliceFrom = sliceFrom;
exports.every = every;
exports.everyi = everyi;
exports.filter = filter;
exports.filteri = filteri;
exports.find = find;
exports.findi = findi;
exports.findIndex = findIndex;
exports.findIndexi = findIndexi;
exports.forEach = forEach;
exports.forEachi = forEachi;
exports.map = map;
exports.mapi = mapi;
exports.reduce = reduce;
exports.reducei = reducei;
exports.reduceRight = reduceRight;
exports.reduceRighti = reduceRighti;
exports.some = some;
exports.somei = somei;
/* No side effect */
