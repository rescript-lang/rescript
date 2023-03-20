'use strict';

var Curry = require("../../lib/js/curry.js");
var React = require("react");
var ReactDom = require("react-dom");

function fib(n) {
  if (n === 2 || n === 1) {
    return 1;
  } else {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

function sum(n) {
  var v = 0;
  for(var i = 0; i <= n; ++i){
    v = v + i | 0;
  }
  return v;
}

function map(f, param) {
  if (typeof param === "string") {
    return "Nil";
  } else {
    return /* Cons */{
            _0: Curry._1(f, param._0),
            _1: map(f, param._1)
          };
  }
}

function test_curry(x, y) {
  return x + y | 0;
}

function f(param) {
  return 32 + param | 0;
}

ReactDom.render(React.createClass({
          render: (function (param) {
              return React.DOM.div({
                          alt: "pic"
                        }, React.DOM.h1(undefined, "hello react"), React.DOM.h2(undefined, "type safe!"));
            })
        }), document.getElementById("hi"));

exports.fib = fib;
exports.sum = sum;
exports.map = map;
exports.test_curry = test_curry;
exports.f = f;
/*  Not a pure module */
