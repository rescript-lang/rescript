// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var React = require("react");

function make(Props) {
  var foo = Props.foo;
  return React.createElement(make, {
              foo: foo
            });
}

function make$1(props) {
  return Curry._1(make$1, {});
}

var mm = Curry.__1(make$1);

var Rec = {
  make: make$1,
  mm: mm
};

exports.make = make;
exports.Rec = Rec;
/* react Not a pure module */
