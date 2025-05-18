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

exports.indexOfOpt = indexOfOpt;
exports.lastIndexOfOpt = lastIndexOfOpt;
exports.searchOpt = searchOpt;
/* No side effect */
