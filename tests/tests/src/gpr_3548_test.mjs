// Generated by ReScript, PLEASE EDIT WITH CARE


let _map = {"Horizontal":"horizontal","Vertical":"vertical"};

let _revMap = {"horizontal":"Horizontal","vertical":"Vertical"};

function orientationToJs(param) {
  return _map[param];
}

function orientationFromJs(param) {
  return _revMap[param];
}

console.log(orientationToJs("Horizontal"));

export {
  orientationToJs,
  orientationFromJs,
}
/*  Not a pure module */