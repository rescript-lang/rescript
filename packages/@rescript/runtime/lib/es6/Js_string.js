

import * as Primitive_option from "./Primitive_option.js";

function charAt(arg1, obj) {
  return obj.charAt(arg1);
}

function charCodeAt(arg1, obj) {
  return obj.charCodeAt(arg1);
}

function codePointAt(arg1, obj) {
  return obj.codePointAt(arg1);
}

function concat(arg1, obj) {
  return obj.concat(arg1);
}

function concatMany(arg1, obj) {
  return obj.concat(...arg1);
}

function endsWith(arg1, obj) {
  return obj.endsWith(arg1);
}

function endsWithFrom(arg1, arg2, obj) {
  return obj.endsWith(arg1, arg2);
}

function includes(arg1, obj) {
  return obj.includes(arg1);
}

function includesFrom(arg1, arg2, obj) {
  return obj.includes(arg1, arg2);
}

function indexOf(arg1, obj) {
  return obj.indexOf(arg1);
}

function indexOfFrom(arg1, arg2, obj) {
  return obj.indexOf(arg1, arg2);
}

function lastIndexOf(arg1, obj) {
  return obj.lastIndexOf(arg1);
}

function lastIndexOfFrom(arg1, arg2, obj) {
  return obj.lastIndexOf(arg1, arg2);
}

function localeCompare(arg1, obj) {
  return obj.localeCompare(arg1);
}

function match_(arg1, obj) {
  return Primitive_option.fromNull(obj.match(arg1));
}

function normalizeByForm(arg1, obj) {
  return obj.normalize(arg1);
}

function repeat(arg1, obj) {
  return obj.repeat(arg1);
}

function replace(arg1, arg2, obj) {
  return obj.replace(arg1, arg2);
}

function replaceByRe(arg1, arg2, obj) {
  return obj.replace(arg1, arg2);
}

function unsafeReplaceBy0(arg1, arg2, obj) {
  return obj.replace(arg1, arg2);
}

function unsafeReplaceBy1(arg1, arg2, obj) {
  return obj.replace(arg1, arg2);
}

function unsafeReplaceBy2(arg1, arg2, obj) {
  return obj.replace(arg1, arg2);
}

function unsafeReplaceBy3(arg1, arg2, obj) {
  return obj.replace(arg1, arg2);
}

function search(arg1, obj) {
  return obj.search(arg1);
}

function slice(from, to_, obj) {
  return obj.slice(from, to_);
}

function sliceToEnd(from, obj) {
  return obj.slice(from);
}

function split(arg1, obj) {
  return obj.split(arg1);
}

function splitAtMost(arg1, limit, obj) {
  return obj.split(arg1, limit);
}

function splitByRe(arg1, obj) {
  return obj.split(arg1);
}

function splitByReAtMost(arg1, limit, obj) {
  return obj.split(arg1, limit);
}

function startsWith(arg1, obj) {
  return obj.startsWith(arg1);
}

function startsWithFrom(arg1, arg2, obj) {
  return obj.startsWith(arg1, arg2);
}

function substr(from, obj) {
  return obj.substr(from);
}

function substrAtMost(from, length, obj) {
  return obj.substr(from, length);
}

function substring(from, to_, obj) {
  return obj.substring(from, to_);
}

function substringToEnd(from, obj) {
  return obj.substring(from);
}

function anchor(arg1, obj) {
  return obj.anchor(arg1);
}

function link(arg1, obj) {
  return obj.link(arg1);
}

export {
  charAt,
  charCodeAt,
  codePointAt,
  concat,
  concatMany,
  endsWith,
  endsWithFrom,
  includes,
  includesFrom,
  indexOf,
  indexOfFrom,
  lastIndexOf,
  lastIndexOfFrom,
  localeCompare,
  match_,
  normalizeByForm,
  repeat,
  replace,
  replaceByRe,
  unsafeReplaceBy0,
  unsafeReplaceBy1,
  unsafeReplaceBy2,
  unsafeReplaceBy3,
  search,
  slice,
  sliceToEnd,
  split,
  splitAtMost,
  splitByRe,
  splitByReAtMost,
  startsWith,
  startsWithFrom,
  substr,
  substrAtMost,
  substring,
  substringToEnd,
  anchor,
  link,
}
/* No side effect */
