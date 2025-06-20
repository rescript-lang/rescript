/** we should also allow js function call from an external js module 

*/
@val("console.log")
external log: 'a => unit = "?ignore"

@scope("console") external log2: 'a => unit = "log"

external log3: 'a => unit = "log"
let v = u => {
  log3(u)
  log2(u)
  log(u)
  u
}
