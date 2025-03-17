'use strict';


function push_back(q, v) {
  let cell = {
    content: v,
    next: undefined
  };
  let last = q.last;
  if (last !== undefined) {
    q.length = q.length + 1 | 0;
    last.next = cell;
    q.last = cell;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
  }
}

function unsafe_pop(q) {
  let cell = q.first;
  if (cell !== undefined) {
    let next = cell.next;
    if (next === undefined) {
      q.length = 0;
      q.first = undefined;
      q.last = undefined;
    } else {
      q.length = q.length - 1 | 0;
      q.first = next;
    }
    return cell.content;
  }
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "Primitive_hash.res",
      70,
      12
    ],
    Error: new Error()
  };
}

function rotl32(x, n) {
  return (x << n) | (x >>> (32 - n | 0)) | 0;
}

function hash_mix_int(h, d) {
  let d$1 = d;
  d$1 = Math.imul(d$1, -862048943);
  d$1 = rotl32(d$1, 15);
  d$1 = Math.imul(d$1, 461845907);
  let h$1 = h ^ d$1;
  h$1 = rotl32(h$1, 13);
  return (h$1 + (h$1 << 2) | 0) - 430675100 | 0;
}

function hash_final_mix(h) {
  let h$1 = h ^ (h >>> 16);
  h$1 = Math.imul(h$1, -2048144789);
  h$1 = h$1 ^ (h$1 >>> 13);
  h$1 = Math.imul(h$1, -1028477387);
  return h$1 ^ (h$1 >>> 16);
}

function hash_mix_string(h, s) {
  let len = s.length;
  let block = (len / 4 | 0) - 1 | 0;
  let hash = h;
  for (let i = 0; i <= block; ++i) {
    let j = (i << 2);
    let w = s.charCodeAt(j) | (s.charCodeAt(j + 1 | 0) << 8) | (s.charCodeAt(j + 2 | 0) << 16) | (s.charCodeAt(j + 3 | 0) << 24);
    hash = hash_mix_int(hash, w);
  }
  let modulo = len & 3;
  if (modulo !== 0) {
    let w$1 = modulo === 3 ? (s.charCodeAt(len - 1 | 0) << 16) | (s.charCodeAt(len - 2 | 0) << 8) | s.charCodeAt(len - 3 | 0) : (
        modulo === 2 ? (s.charCodeAt(len - 1 | 0) << 8) | s.charCodeAt(len - 2 | 0) : s.charCodeAt(len - 1 | 0)
      );
    hash = hash_mix_int(hash, w$1);
  }
  hash = hash ^ len;
  return hash;
}

function hash(count, _limit, seed, obj) {
  let s = seed;
  if (typeof obj === "number") {
    let u = obj | 0;
    s = hash_mix_int(s, (u + u | 0) + 1 | 0);
    return hash_final_mix(s);
  }
  if (typeof obj === "string") {
    s = hash_mix_string(s, obj);
    return hash_final_mix(s);
  }
  let queue = {
    length: 0,
    first: undefined,
    last: undefined
  };
  let num = count;
  push_back(queue, obj);
  num = num - 1 | 0;
  while (queue.length !== 0 && num > 0) {
    let obj$1 = unsafe_pop(queue);
    if (typeof obj$1 === "number") {
      let u$1 = obj$1 | 0;
      s = hash_mix_int(s, (u$1 + u$1 | 0) + 1 | 0);
      num = num - 1 | 0;
    } else if (typeof obj$1 === "string") {
      s = hash_mix_string(s, obj$1);
      num = num - 1 | 0;
    } else if (typeof obj$1 !== "boolean" && typeof obj$1 !== "undefined" && typeof obj$1 !== "symbol" && typeof obj$1 !== "function") {
      let size = obj$1.length | 0;
      if (size !== 0) {
        let obj_tag = obj$1.TAG;
        let tag = (size << 10) | obj_tag;
        s = hash_mix_int(s, tag);
        let v = size - 1 | 0;
        let block = v < num ? v : num;
        for (let i = 0; i <= block; ++i) {
          push_back(queue, obj$1[i]);
        }
      } else {
        let size$1 = (function(obj,cb){
            var size = 0  
            for(var k in obj){
              cb(obj[k])
              ++ size
            }
            return size
          })(obj$1, v => push_back(queue, v));
        s = hash_mix_int(s, (size$1 << 10) | 0);
      }
    }
    
  };
  return hash_final_mix(s);
}

exports.hash_mix_int = hash_mix_int;
exports.hash_mix_string = hash_mix_string;
exports.hash_final_mix = hash_final_mix;
exports.hash = hash;
/* No side effect */
