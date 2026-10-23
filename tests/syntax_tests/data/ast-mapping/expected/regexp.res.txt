let plain = /abc/
let escaped = /a\/b\\c\d/ig
let characterClass = /[/\]]+/u
let unicode = /世界+/g
let dot = /./s
let flags = /a/dgimsuy
@foo
let attributed = /a/g
let comments = [/* before */ /a/g /* after */, /b/]
let use = f(/a/i, /b/m)
let expressionAttributes = @foo /a/g
let choice = flag ? /a/i : /b/m
let indexed = /a/g["lastIndex"]
