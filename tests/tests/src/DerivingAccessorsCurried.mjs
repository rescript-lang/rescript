// Generated by ReScript, PLEASE EDIT WITH CARE


function myField(param) {
  return param.myField;
}

function num(param_0) {
  return {
    TAG: "Num",
    _0: param_0
  };
}

function doubleNum(param_0, param_1) {
  return {
    TAG: "DoubleNum",
    _0: param_0,
    _1: param_1
  };
}

function compose(a, accessor) {
  return accessor(a);
}

let _composedMyField = 1;

let _composedNum = {
  TAG: "Num",
  _0: 1
};

let noParam = "NoParam";

let _myFieldAlias = myField;

let _noParamAlias = "NoParam";

let _numAlias = num;

let _doubleNumAlias = doubleNum;

export {
  myField,
  noParam,
  num,
  doubleNum,
  _myFieldAlias,
  _noParamAlias,
  _numAlias,
  _doubleNumAlias,
  compose,
  _composedMyField,
  _composedNum,
}
/* No side effect */
