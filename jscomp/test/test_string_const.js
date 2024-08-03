// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_string = require("../../lib/js/caml_string.js");
let Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

let f = Caml_string.get("ghsogh", 3);

let hh;

try {
  hh = Caml_string.get("ghsogh", -3);
} catch (raw_e) {
  let e = Caml_js_exceptions.internalToOCamlException(raw_e);
  if (e.RE_EXN_ID === "Invalid_argument") {
    console.log(e._1);
    hh = /* 'a' */97;
  } else {
    throw new Error(e.RE_EXN_ID, {
          cause: e
        });
  }
}

exports.f = f;
exports.hh = hh;
/* hh Not a pure module */
