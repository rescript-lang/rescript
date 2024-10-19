// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Lazy = require("rescript/lib/js/Lazy.js");

function fix() {
  return {
    TAG: "Fix",
    _0: Lazy.from_fun(fix)
  };
}

function unfixLeak(_f) {
  while (true) {
    let f = _f;
    _f = Lazy.force(f._0);
    continue;
  };
}

function unfix(p) {
  while (true) {
    let h = p.contents;
    p.contents = Lazy.force(h._0);
  };
}

exports.fix = fix;
exports.unfixLeak = unfixLeak;
exports.unfix = unfix;
/* No side effect */
