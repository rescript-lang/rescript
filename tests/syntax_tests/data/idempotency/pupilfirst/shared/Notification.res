type jsNotify

@module("./notifier")
external jsNotify: (string, string, string) => unit = "default"

/* [@send]
 external notify: (jsNotify, string, string, string) => unit = "notify"; */

let success = (title, text) => jsNotify(title, text, "success")
let error = (title, text) => jsNotify(title, text, "error")
let notice = (title, text) => jsNotify(title, text, "notice")
let warn = notice
