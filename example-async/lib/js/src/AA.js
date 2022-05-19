// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_exn = require("rescript/lib/js/js_exn.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

var tests = [];

function addTest(t) {
  tests.push(t);
}

function addTest1(t, x) {
  tests.push(function () {
        return t(x);
      });
}

async function foo(x, y) {
  return x + y | 0;
}

async function bar(ff) {
  var a = await ff(3, 4);
  var b = await foo(5, 6);
  return a + b | 0;
}

async function baz() {
  return await bar(foo);
}

async function testBaz() {
  var n = await baz();
  console.log("baz returned", n);
}

tests.push(testBaz);

var E = /* @__PURE__ */Caml_exceptions.create("AA.E");

async function e1() {
  throw {
        RE_EXN_ID: E,
        _1: 1000,
        Error: new Error()
      };
}

async function e2() {
  return Js_exn.raiseError("Some JS error");
}

async function e3() {
  return await e1();
}

async function e4() {
  return await e2();
}

var e5 = (function() { return Promise.reject(new Error('fail')) });

async function testTryCatch(fn) {
  try {
    return await fn();
  }
  catch (raw_n){
    var n = Caml_js_exceptions.internalToOCamlException(raw_n);
    if (n.RE_EXN_ID === E) {
      console.log("testTryCatch: E", n._1);
      return ;
    }
    if (n.RE_EXN_ID === "JsError") {
      console.log("testTryCatch: JsError");
      return ;
    }
    throw n;
  }
}

addTest1(testTryCatch, e1);

addTest1(testTryCatch, e2);

addTest1(testTryCatch, e3);

addTest1(testTryCatch, e4);

addTest1(testTryCatch, e5);

async function singlePromise(x) {
  return x + 1 | 0;
}

async function nestedPromise(x) {
  var x$1 = singlePromise(x + 1 | 0);
  [Promise.resolve(x$1)];
  return 32;
}

var explainError = ((e)=>e.toString());

async function testFetch(url) {
  var response;
  try {
    response = await fetch(url);
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (e.RE_EXN_ID === "JsError") {
      console.log("Fetch returned an error:", explainError(e._1));
      return ;
    }
    throw e;
  }
  var status = response.status;
  console.log("Fetch returned status:", status);
}

addTest1(testFetch, "https://www.google.com/sdkjdkghdsg");

addTest1(testFetch, "https://www.google.comsdkjdkghdsg");

async function runAllTests() {
  for(var i = 0 ,i_finish = tests.length; i < i_finish; ++i){
    await Caml_array.get(tests, i)();
  }
}

runAllTests();

exports.tests = tests;
exports.addTest = addTest;
exports.addTest1 = addTest1;
exports.foo = foo;
exports.bar = bar;
exports.baz = baz;
exports.testBaz = testBaz;
exports.E = E;
exports.e1 = e1;
exports.e2 = e2;
exports.e3 = e3;
exports.e4 = e4;
exports.e5 = e5;
exports.testTryCatch = testTryCatch;
exports.singlePromise = singlePromise;
exports.nestedPromise = nestedPromise;
exports.explainError = explainError;
exports.testFetch = testFetch;
exports.runAllTests = runAllTests;
/*  Not a pure module */
