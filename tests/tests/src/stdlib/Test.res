// Test "framework"

@scope("process") @val external exit: int => unit = "exit"
@scope("Error") @val external captureStackTrace: {..} => unit = "captureStackTrace"
@module("@babel/code-frame") @val
external codeFrameColumns: (string, {..}, {..}) => string = "codeFrameColumns"
@module("fs") @val external readFileSync: (string, {..}) => string = "readFileSync"
@module("path") @val external join: (string, string) => string = "join"

let dirname = %raw("new URL('.', import.meta.url).pathname")

@val @module("util") external inspect: _ => string = "inspect"
let print = value =>
  switch Type.typeof(value) {
  | #string => JSON.stringifyAny(value)->Option.getExn // uses " instead of '
  | #object | #bigint => inspect(value)
  | _ => String.make(value)
  }

let run = (loc, left, comparator, right) => {
  if !comparator(left, right) {
    let ((file, line, _, _), _) = loc
    let fileContent = readFileSync(join(dirname, file), {"encoding": "utf-8"})
    let left = print(left)
    let right = print(right)
    let codeFrame = codeFrameColumns(
      fileContent,
      {"start": {"line": line}},
      {"highlightCode": true},
    )
    let errorMessage = `
  \u001b[31mTest Failure!
  \u001b[36m${file}\u001b[0m:\u001b[2m${string_of_int(line)}
${codeFrame}
  \u001b[39mLeft: \u001b[31m${left}
  \u001b[39mRight: \u001b[31m${right}\u001b[0m
`
    Console.log(errorMessage)
    // API: https://nodejs.org/api/errors.html#errors_error_capturestacktrace_targetobject_constructoropt
    let obj = Object.make()
    captureStackTrace(obj)
    // clean up stack trace! Stack format: https://nodejs.org/api/errors.html#errors_error_stack
    obj["stack"]
    ->String.replaceRegExp(/\n    /g, "\n  ") // indent 2 spaces instead of 4, to align with code frame
    ->String.replaceRegExp(/^Error\n/, "") // first line is just the word "Error"
    ->String.replaceRegExp(/^.+\n/, "") // second line (now first) is this Test module's own stack frame
    ->String.replaceRegExp(/\n  at .+\(node:internal.+\n?/g, "") // remove internal lines like "  at ModuleJob.run (node:internal/modules/esm/module_job:193:25)"
    ->Console.log
  }
}

module TypeofParensOnFun = {
  @val external require: string => {..} = "require"

  Console.log(Type.typeof(require))
}
