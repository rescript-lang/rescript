'use strict';


function indexOfOpt(s, search) {
  let index = s.indexOf(search);
  if (index !== -1) {
    return index;
  }
  
}

function lastIndexOfOpt(s, search) {
  let index = s.lastIndexOf(search);
  if (index !== -1) {
    return index;
  }
  
}

function searchOpt(s, re) {
  let index = s.search(re);
  if (index !== -1) {
    return index;
  }
  
}

function isEmpty(s) {
  return s.length === 0;
}

function capitalize(s) {
  if (s.length === 0) {
    return s;
  } else {
    return s[0].toUpperCase() + s.slice(1);
  }
}

exports.indexOfOpt = indexOfOpt;
exports.lastIndexOfOpt = lastIndexOfOpt;
exports.searchOpt = searchOpt;
exports.isEmpty = isEmpty;
exports.capitalize = capitalize;
/* No side effect */
