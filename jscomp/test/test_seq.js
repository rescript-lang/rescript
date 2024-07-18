// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_obj = require("../../lib/js/caml_obj.js");
let Pervasives = require("../../lib/js/pervasives.js");
let Caml_exceptions = require("../../lib/js/caml_exceptions.js");
let Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

let Bad = /* @__PURE__ */Caml_exceptions.create("Test_seq.Bad");

let Help = /* @__PURE__ */Caml_exceptions.create("Test_seq.Help");

let Stop = /* @__PURE__ */Caml_exceptions.create("Test_seq.Stop");

function assoc3(x, _l) {
  while(true) {
    let l = _l;
    if (l) {
      let match = l.hd;
      if (Caml_obj.equal(match[0], x)) {
        return match[1];
      }
      _l = l.tl;
      continue;
    }
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  };
}

function help_action() {
  throw new Error(Stop, {
        cause: {
          RE_EXN_ID: Stop,
          _1: {
            TAG: "Unknown",
            _0: "-help"
          }
        }
      });
}

function v(speclist) {
  assoc3("-help", speclist);
  return /* [] */0;
}

function f(g, speclist) {
  return g(assoc3("-help", speclist));
}

function add_help(speclist) {
  let add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (raw_exn){
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      add1 = {
        hd: [
          "-help",
          {
            TAG: "Unit",
            _0: help_action
          },
          " Display this list of options"
        ],
        tl: /* [] */0
      };
    } else {
      throw new Error(exn.RE_EXN_ID, {
            cause: exn
          });
    }
  }
  let add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  }
  catch (raw_exn$1){
    let exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    if (exn$1.RE_EXN_ID === "Not_found") {
      add2 = {
        hd: [
          "--help",
          {
            TAG: "Unit",
            _0: help_action
          },
          " Display this list of options"
        ],
        tl: /* [] */0
      };
    } else {
      throw new Error(exn$1.RE_EXN_ID, {
            cause: exn$1
          });
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

exports.Bad = Bad;
exports.Help = Help;
exports.Stop = Stop;
exports.assoc3 = assoc3;
exports.help_action = help_action;
exports.v = v;
exports.f = f;
exports.add_help = add_help;
/* No side effect */
