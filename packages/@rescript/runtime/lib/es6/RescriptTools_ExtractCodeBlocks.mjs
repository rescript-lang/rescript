

import * as Stdlib_Array from "./Stdlib_Array.mjs";

function decodeFromJson(json) {
  if (Array.isArray(json)) {
    let codeblocks = Stdlib_Array.filterMap(json, c => {
      if (typeof c !== "object" || c === null || Array.isArray(c)) {
        return;
      }
      let id = c.id;
      if (typeof id !== "string") {
        return;
      }
      let name = c.name;
      if (typeof name !== "string") {
        return;
      }
      let code = c.code;
      if (typeof code === "string") {
        return {
          id: id,
          name: name,
          code: code
        };
      }
    });
    return {
      TAG: "Ok",
      _0: codeblocks
    };
  }
  switch (typeof json) {
    case "string" :
      return {
        TAG: "Error",
        _0: json
      };
    default:
      return {
        TAG: "Error",
        _0: "Failed to decode codeblocks"
      };
  }
}

export {
  decodeFromJson,
}
/* No side effect */
