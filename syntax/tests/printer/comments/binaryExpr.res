/* with comments */
compilation // after
/* below compilation */
->Plugin.buildAssets // trailing

compilation /* same line as compilation */ ->Plugin.buildAssets // trailing

/* with comments */
compilation
/* first */
->Plugin.buildAssets /* first trail */
/* second */
->Js.Json.stringify /* second trail */
/* last */
->Node.Fs.writeFileAsUtf8Sync(path); /* last trail */

if (
  // above1
    a == b && // trailing1
  // above2
    c == d && // trailing2
  // above3
    e == f && // trailing3
    x
) {
    console.log("foo bar")
}


let truth =
  a == b && c == d /* cmt */ && e == f

let truth =
  /* c0 */ a /* c1 */ == /* c2 */ b /* c3 */ &&
 /* c4 */ c /* c5 */ == /* c6 */ d /* c7 */ &&
  /* c8 */ e /* c9 */ ==  /* c10 */ f  /* c11 */

promise
// TODO: This comment needs to be here
->Js.Promise.then(payload => {
  Js.log2("this payload was received", payload);
  Js.Promise.resolve(""->Obj.magic);
})


promise
/* TODO: This comment needs to be here */
->Js.Promise.then(payload => {
  Js.log2("this payload was received", payload);
  Js.Promise.resolve(""->Obj.magic);
})

promise
// comment
->Js.Promise.then(payload => {
  Js.Promise.resolve(""->Obj.magic);
})
// comment 2
->Js.Promise.catch(err => {
  Js.log2("this error was caught", err);
  Js.Promise.resolve(""->Obj.magic);
})


promise
/* comment */
->Js.Promise.then(payload => {
  Js.Promise.resolve(""->Obj.magic);
})
/* comment 2 */

// comment 3

/* comment 4 */
->Js.Promise.catch(err => {
  Js.log2("this error was caught", err);
  Js.Promise.resolve(""->Obj.magic);
})

promise
// TODO: This comment needs to be here
|> Js.Promise.then(payload => {
  Js.log2("this payload was received", payload);
  Js.Promise.resolve(""->Obj.magic);
})

promise
// comment
|> Js.Promise.then(payload => {
  Js.Promise.resolve(""->Obj.magic);
})
// comment 2
|> Js.Promise.catch(err => {
  Js.log2("this error was caught", err);
  Js.Promise.resolve(""->Obj.magic);
})
