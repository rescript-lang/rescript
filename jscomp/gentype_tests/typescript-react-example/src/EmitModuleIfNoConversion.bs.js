// Generated by ReScript, PLEASE EDIT WITH CARE


function foo(t) {
  if (t) {
    console.log("B" + t.name);
  } else {
    console.log("A");
  }
}

var X = {
  foo: foo,
  x: 42
};

var Y = {
  x: ""
};

export default {
  X ,
  Y ,
}
/* No side effect */
