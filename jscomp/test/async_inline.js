// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Curry = require("../../lib/js/curry.js");
let React = require("react");

async function willBeInlined(param) {
  return 3;
}

let inlined = willBeInlined();

function wrapSomethingAsync(param) {
  ((async function (param) {
      let test = await Promise.resolve("Test");
      console.log(test);
    })(777));
}

function wrapSomethingAsync2(param) {
  ((async function (param) {
      let test = await Promise.resolve("Test");
      console.log(test);
    })());
}

async function doSomethingAsync(someAsyncFunction) {
  return await Curry._1(someAsyncFunction, undefined);
}

let broken = doSomethingAsync;

let M = {
  broken: broken
};

async function broken$1(someAsyncFunction) {
  return await Curry._1(someAsyncFunction, undefined);
}

let broken$2 = broken$1;

function curriedId(x) {
  return x;
}

async function curriedIdAsync(x) {
  return x;
}

function uncurriedId(x) {
  return x;
}

async function uncurriedIdAsync(x) {
  return x;
}

let tcia = curriedIdAsync(3);

let tui = 3;

let tuia = uncurriedIdAsync(3);

function nested1(param) {
  return async function (y) {
    return await y;
  };
}

async function nested2(param) {
  return async function (y) {
    return await y;
  };
}

function onSubmit(param) {
  return React.useCallback(async function (b) {
    return await b;
  });
}

let tci = 3;

exports.willBeInlined = willBeInlined;
exports.inlined = inlined;
exports.wrapSomethingAsync = wrapSomethingAsync;
exports.wrapSomethingAsync2 = wrapSomethingAsync2;
exports.M = M;
exports.broken = broken$2;
exports.curriedId = curriedId;
exports.curriedIdAsync = curriedIdAsync;
exports.uncurriedId = uncurriedId;
exports.uncurriedIdAsync = uncurriedIdAsync;
exports.tci = tci;
exports.tcia = tcia;
exports.tui = tui;
exports.tuia = tuia;
exports.nested1 = nested1;
exports.nested2 = nested2;
exports.onSubmit = onSubmit;
/* inlined Not a pure module */
