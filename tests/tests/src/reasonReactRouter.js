// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let React = require("react");
let Js_string = require("rescript/lib/js/Js_string.js");
let Primitive_option = require("rescript/lib/js/Primitive_option.js");

function safeMakeEvent(eventName) {
  if (typeof Event === "function") {
    return new Event(eventName);
  }
  let event = document.createEvent("Event");
  event.initEvent(eventName, true, true);
  return event;
}

function path() {
  let window = globalThis.window;
  if (window === undefined) {
    return /* [] */0;
  }
  let raw = Primitive_option.valFromOption(window).location.pathname;
  switch (raw) {
    case "" :
    case "/" :
      return /* [] */0;
    default:
      let raw$1 = Js_string.sliceToEnd(1, raw);
      let match = raw$1[raw$1.length - 1 | 0];
      let raw$2 = match === "/" ? Js_string.slice(0, -1, raw$1) : raw$1;
      let a = Js_string.split("/", raw$2);
      let _i = a.length - 1 | 0;
      let _res = /* [] */0;
      while (true) {
        let res = _res;
        let i = _i;
        if (i < 0) {
          return res;
        }
        _res = {
          hd: a[i],
          tl: res
        };
        _i = i - 1 | 0;
        continue;
      };
  }
}

function hash() {
  let window = globalThis.window;
  if (window === undefined) {
    return "";
  }
  let raw = Primitive_option.valFromOption(window).location.hash;
  switch (raw) {
    case "" :
    case "#" :
      return "";
    default:
      return Js_string.sliceToEnd(1, raw);
  }
}

function search() {
  let window = globalThis.window;
  if (window === undefined) {
    return "";
  }
  let raw = Primitive_option.valFromOption(window).location.search;
  switch (raw) {
    case "" :
    case "?" :
      return "";
    default:
      return Js_string.sliceToEnd(1, raw);
  }
}

function push(path) {
  let match = globalThis.history;
  let match$1 = globalThis.window;
  if (match !== undefined && match$1 !== undefined) {
    Primitive_option.valFromOption(match).pushState(null, "", path);
    Primitive_option.valFromOption(match$1).dispatchEvent(safeMakeEvent("popstate"));
    return;
  }
  
}

function replace(path) {
  let match = globalThis.history;
  let match$1 = globalThis.window;
  if (match !== undefined && match$1 !== undefined) {
    Primitive_option.valFromOption(match).replaceState(null, "", path);
    Primitive_option.valFromOption(match$1).dispatchEvent(safeMakeEvent("popstate"));
    return;
  }
  
}

function urlNotEqual(a, b) {
  if (a.hash !== b.hash || a.search !== b.search) {
    return true;
  } else {
    let _aList = a.path;
    let _bList = b.path;
    while (true) {
      let bList = _bList;
      let aList = _aList;
      if (!aList) {
        if (bList) {
          return true;
        } else {
          return false;
        }
      }
      if (!bList) {
        return true;
      }
      if (aList.hd !== bList.hd) {
        return true;
      }
      _bList = bList.tl;
      _aList = aList.tl;
      continue;
    };
  }
}

function url() {
  return {
    path: path(),
    hash: hash(),
    search: search()
  };
}

function watchUrl(callback) {
  let window = globalThis.window;
  if (window === undefined) {
    return () => {};
  }
  let watcherID = () => callback(url());
  Primitive_option.valFromOption(window).addEventListener("popstate", watcherID);
  return watcherID;
}

function unwatchUrl(watcherID) {
  let window = globalThis.window;
  if (window !== undefined) {
    Primitive_option.valFromOption(window).removeEventListener("popstate", watcherID);
    return;
  }
  
}

function useUrl(serverUrl, param) {
  let match = React.useState(() => {
    if (serverUrl !== undefined) {
      return serverUrl;
    } else {
      return url();
    }
  });
  let setUrl = match[1];
  let url$1 = match[0];
  React.useEffect(() => {
    let watcherId = watchUrl(url => setUrl(param => url));
    let newUrl = url();
    if (urlNotEqual(newUrl, url$1)) {
      setUrl(param => newUrl);
    }
    return () => unwatchUrl(watcherId);
  }, []);
  return url$1;
}

let dangerouslyGetInitialUrl = url;

exports.push = push;
exports.replace = replace;
exports.watchUrl = watchUrl;
exports.unwatchUrl = unwatchUrl;
exports.dangerouslyGetInitialUrl = dangerouslyGetInitialUrl;
exports.useUrl = useUrl;
/* react Not a pure module */
