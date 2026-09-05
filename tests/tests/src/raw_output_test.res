@@warning("-22")
let mk = fn => fn()

/* Here it became subtle, 
  the optimizer knew its arity is zero, and it has one arguments

  in normal case, if its arity is 3, and it has four arguments, the 
  optimizer will try to supply it with some arguments, in that case
  it will do a wrong job. So we disabled such application 
  when arity is zero
*/
mk(%raw(`(_)=> console.log('should works')`))

Console.log((() => 1)())

// GitHub issue #6236: escapes needed for the surrounding ReScript template
// must not survive in the emitted raw JavaScript.
%%raw(`
var issue6236 = \`\${"hello"}\`;
`)
