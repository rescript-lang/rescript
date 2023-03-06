'use strict';


function Alias_default_value_test$C0(props) {
  var b = props.b;
  var a = props.a;
  var a$1 = a !== undefined ? a : 2;
  var b$1 = b !== undefined ? b : (a$1 << 1);
  return a$1 + b$1 | 0;
}

var C0 = {
  make: Alias_default_value_test$C0
};

function Alias_default_value_test$C1(props) {
  var bar = props.foo;
  if (bar !== undefined) {
    return bar;
  } else {
    return "";
  }
}

var C1 = {
  make: Alias_default_value_test$C1
};

function Alias_default_value_test$C2(props) {
  var a = props.a;
  var bar = props.foo;
  var bar$1 = bar !== undefined ? bar : "";
  var a$1 = a !== undefined ? a : bar$1;
  return bar$1 + a$1 + props.b;
}

var C2 = {
  make: Alias_default_value_test$C2
};

function Alias_default_value_test$C3(props) {
  var text = props.text;
  if (text !== undefined) {
    return text;
  } else {
    return "Test";
  }
}

var C3 = {
  make: Alias_default_value_test$C3
};

function Alias_default_value_test$C4(props) {
  return props.a;
}

var C4 = {
  make: Alias_default_value_test$C4
};

function Alias_default_value_test$C6(props) {
  return props.comp.xx;
}

var C6 = {
  make: Alias_default_value_test$C6
};

exports.C0 = C0;
exports.C1 = C1;
exports.C2 = C2;
exports.C3 = C3;
exports.C4 = C4;
exports.C6 = C6;
/* No side effect */
