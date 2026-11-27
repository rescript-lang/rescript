let testFnWithDirective = @directive("'use server'") (name: string) => "Hello " ++ name

let x = testFnWithDirective("test")
