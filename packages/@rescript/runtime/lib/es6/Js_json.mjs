


let Kind = {};

function classify(x) {
  let ty = typeof x;
  if (ty === "string") {
    return {
      TAG: "JSONString",
      _0: x
    };
  } else if (ty === "number") {
    return {
      TAG: "JSONNumber",
      _0: x
    };
  } else if (ty === "boolean") {
    if (x === true) {
      return "JSONTrue";
    } else {
      return "JSONFalse";
    }
  } else if (x === null) {
    return "JSONNull";
  } else if (Array.isArray(x)) {
    return {
      TAG: "JSONArray",
      _0: x
    };
  } else {
    return {
      TAG: "JSONObject",
      _0: x
    };
  }
}

function test(x, v) {
  switch (v) {
    case "String" :
      return typeof x === "string";
    case "Number" :
      return typeof x === "number";
    case "Object" :
      if (x !== null && typeof x === "object") {
        return !Array.isArray(x);
      } else {
        return false;
      }
    case "Array" :
      return Array.isArray(x);
    case "Boolean" :
      return typeof x === "boolean";
    case "Null" :
      return x === null;
  }
}

function decodeString(json) {
  if (typeof json === "string") {
    return json;
  }
}

function decodeNumber(json) {
  if (typeof json === "number") {
    return json;
  }
}

function decodeObject(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return json;
  }
}

function decodeArray(json) {
  if (Array.isArray(json)) {
    return json;
  }
}

function decodeBoolean(json) {
  if (typeof json === "boolean") {
    return json;
  }
}

function decodeNull(json) {
  if (json === null) {
    return null;
  }
}

let patch = (function (json) {
  var x = [json];
  var q = [{ kind: 0, i: 0, parent: x }];
  while (q.length !== 0) {
    // begin pop the stack
    var cur = q[q.length - 1];
    if (cur.kind === 0) {
      cur.val = cur.parent[cur.i]; // patch the undefined value for array
      if (++cur.i === cur.parent.length) {
        q.pop();
      }
    } else {
      q.pop();
    }
    // finish
    var task = cur.val;
    if (typeof task === "object") {
      if (Array.isArray(task) && task.length !== 0) {
        q.push({ kind: 0, i: 0, parent: task, val: undefined });
      } else {
        for (var k in task) {
          if (k === "RE_PRIVATE_NONE") {
            if (cur.kind === 0) {
              cur.parent[cur.i - 1] = undefined;
            } else {
              cur.parent[cur.i] = undefined;
            }
            continue;
          }
          q.push({ kind: 1, i: k, parent: task, val: task[k] });
        }
      }
    }
  }
  return x[0];
});

function serializeExn(x) {
  return (function(obj){
  var output= JSON.stringify(obj,function(_,value){
      if(value===undefined){
          return {RE_PRIVATE_NONE : true}
      }
    return value
  });
  
 if(output === undefined){
   // JSON.stringify will throw TypeError when it detects cylic objects
   throw new TypeError("output is undefined")
 }
 return output 
 })(x);
}

function deserializeUnsafe(s) {
  return patch(JSON.parse(s));
}

export {
  Kind,
  classify,
  test,
  decodeString,
  decodeNumber,
  decodeObject,
  decodeArray,
  decodeBoolean,
  decodeNull,
  deserializeUnsafe,
  serializeExn,
}
/* No side effect */
