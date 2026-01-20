'use strict';


function then_(arg1, obj) {
  return obj.then(arg1);
}

function $$catch(arg1, obj) {
  return obj.catch(arg1);
}

exports.then_ = then_;
exports.$$catch = $$catch;
/* No side effect */
