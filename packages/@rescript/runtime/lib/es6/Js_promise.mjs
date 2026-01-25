


function then_(arg1, obj) {
  return obj.then(arg1);
}

function $$catch(arg1, obj) {
  return obj.catch(arg1);
}

export {
  then_,
  $$catch,
}
/* No side effect */
