/***
Functions for interacting with JavaScript console.

See: [`console`](https://developer.mozilla.org/en-US/docs/Web/API/Console).
*/

/**
`assert_(assertion, value)` print a message to console if `assertion` evaluates `false`. Does nothing if it's `true`.

See [`console.assert`](https://developer.mozilla.org/en-US/docs/Web/API/console/assert)
on MDN.

## Examples

```rescript
Console.assert_(false, "Hello World!")
Console.assert_(42 == 42, "The answer")
```
*/
@val
external assert_: (bool, 'a) => unit = "console.assert"

/**
`assert2(v1, v2)`. Like `assert_`, but with two arguments.

## Examples

```rescript
Console.assert2(false, "Hello", "World")
Console.assert2(42 == 42, [1, 2, 3], '4')
```
*/
@val
external assert2: (bool, 'a, 'b) => unit = "console.assert"

/**
`assert3(v1, v2, v3)`. Like `assert_`, but with three arguments.

## Examples

```rescript
Console.assert3(false, "Hello", "World", "ReScript")
Console.assert3(42 == 42, "One", 2, #3)
```
*/
@val
external assert3: (bool, 'a, 'b, 'c) => unit = "console.assert"

/**
`assert4(v1, v2, v3, v4)`. Like `assert_`, but with four arguments.

## Examples

```rescript
let value = 42
Console.assert4(false, "Hello", "World", "ReScript", "!!!")
Console.assert4(value == 42, [1, 2], (3, 4), [#5, #6], #"polyvar")
```
*/
@val
external assert4: (bool, 'a, 'b, 'c, 'd) => unit = "console.assert"

/**
`assert5(v1, v2, v3, v4, v5)`. Like `assert_`, but with five arguments.

## Examples

```rescript
let value = 42
Console.assert5(false, "Hello", "World", "JS", '!', '!')
Console.assert5(value == 42, [1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"})
```
*/
@val
external assert5: (bool, 'a, 'b, 'c, 'd, 'e) => unit = "console.assert"

/**
`assert6(v1, v2)`. Like `assert_`, but with six arguments.

## Examples

```rescript
let value = 42
Console.assert6(false, "Hello", "World", "JS", '!', '!', '?')
Console.assert6(value == 42, [1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"}, 42)
```
*/
@val
external assert6: (bool, 'a, 'b, 'c, 'd, 'e, 'f) => unit = "console.assert"

/**
`assertMany(assertion, arr)`. Like `assert_`, but variadic.

## Examples

```rescript
let value = 42
Console.assertMany(false, ["Hello", "World"])
Console.assertMany(value == 42, [1, 2, 3])
```
*/
@val
@variadic
external assertMany: (bool, array<_>) => unit = "console.assert"

/**
`clear()` clears the console, if allowed.

See [`console.clear`](https://developer.mozilla.org/en-US/docs/Web/API/console/clear)
on MDN.

## Examples

```rescript
Console.clear()
```
*/
@val
external clear: unit => unit = "console.clear"

/**
`count(label)` prints to the console the number of times it's been called with the given label.

See [`console.count`](https://developer.mozilla.org/en-US/docs/Web/API/console/count)
on MDN.

## Examples

```rescript
Console.count("rescript")
```
*/
@val
external count: string => unit = "console.count"

/**
`countReset(label)` resets the count for the given label to 0.

See [`console.countReset`](https://developer.mozilla.org/en-US/docs/Web/API/console/countReset)
on MDN.

## Examples

```rescript
Console.countReset("rescript")
```
*/
@val
external countReset: string => unit = "console.countReset"

/**
`debug(value)` print a debug message to console.

See [`console.debug`](https://developer.mozilla.org/en-US/docs/Web/API/console/debug)
on MDN.

## Examples

```rescript
Console.debug("Hello")
let obj = {"name": "ReScript", "version": 10}
Console.debug(obj)
```
*/
@val
external debug: 'a => unit = "console.debug"

/**
`debug2(v1, v2)`. Like `debug`, but with two arguments.

## Examples

```rescript
Console.debug2("Hello", "World")
Console.debug2([1, 2, 3], '4')
```
*/
@val
external debug2: ('a, 'b) => unit = "console.debug"

/**
`debug3(v1, v2, v3)`. Like `debug`, but with three arguments.

## Examples

```rescript
Console.debug3("Hello", "World", "ReScript")
Console.debug3("One", 2, #3)
```
*/
@val
external debug3: ('a, 'b, 'c) => unit = "console.debug"

/**
`debug4(v1, v2, v3, v4)`. Like `debug`, but with four arguments.

## Examples

```rescript
Console.debug4("Hello", "World", "ReScript", "!!!")
Console.debug4([1, 2], (3, 4), [#5, #6], #"polyvar")
```
*/
@val
external debug4: ('a, 'b, 'c, 'd) => unit = "console.debug"

/**
`debug5(v1, v2, v3, v4, v5)`. Like `debug`, but with five arguments.

## Examples

```rescript
Console.debug5("Hello", "World", "JS", '!', '!')
Console.debug5([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"})
```
*/
@val
external debug5: ('a, 'b, 'c, 'd, 'e) => unit = "console.debug"

/**
`debug6(v1, v2, v3, v4, v5, v6)`. Like `debug`, but with six arguments.

## Examples

```rescript
Console.debug6("Hello", "World", "JS", '!', '!', '?')
Console.debug6([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"}, 42)
```
*/
@val
external debug6: ('a, 'b, 'c, 'd, 'e, 'f) => unit = "console.debug"

/**
`debugMany(arr)`. Like `debug`, but variadic.

## Examples

```rescript
Console.debugMany(["Hello", "World"])
Console.debugMany([1, 2, 3])
```
*/
@val
@variadic
external debugMany: array<_> => unit = "console.debug"

/**
`dir(object)` displays an interactive view of the object in the console.

See [`console.dir`](https://developer.mozilla.org/en-US/docs/Web/API/console/dir)
on MDN.

## Examples

```rescript
Console.dir({"language": "rescript", "version": "10.1.2"})
```
*/
@val
external dir: 'a => unit = "console.dir"

/**
`dirxml(object)` displays an interactive tree view of an XML/HTML element in the console.

See [`console.dirxml`](https://developer.mozilla.org/en-US/docs/Web/API/console/dirxml)
on MDN.
*/
@val
external dirxml: 'a => unit = "console.dirxml"

/**
`error(value)` prints an error message to console.

See [`console.error`](https://developer.mozilla.org/en-US/docs/Web/API/console/error)
on MDN.

## Examples

```rescript
Console.error("error message")
Console.error(("error", "invalid value"))
```
*/
@val
external error: 'a => unit = "console.error"

/**
`error(v1, v2)`. Like `error`, but two arguments.

## Examples

```rescript
Console.error2("Error", "here")
Console.error2(("log", "error"), "message")
```
*/
@val
external error2: ('a, 'b) => unit = "console.error"

/**
`error3(v1, v2, v3)`. Like `error`, but three arguments.

## Examples

```rescript
Console.error3("Hello", "World", "!!!")
Console.error3(#first, #second, #third)
```
*/
@val
external error3: ('a, 'b, 'c) => unit = "console.error"

/**
`error4(v1, v2, v3, v4)`. Like `error`, but with four arguments.

## Examples

```rescript
Console.error4("Hello", "World", "ReScript", '!')
Console.error4(#first, #second, #third, ("fourth"))
```
*/
@val
external error4: ('a, 'b, 'c, 'd) => unit = "console.error"

/**
`error5(v1, v2, v3, v4, v5)`. Like `error`, but with five arguments.

## Examples

```rescript
Console.error5('e', 'r', 'r', 'o', 'r')
Console.error5(1, #second, #third, ("fourth"), 'c')
```
*/
@val
external error5: ('a, 'b, 'c, 'd, 'e) => unit = "console.error"

/**
`error6(v1, v2, v3, v4, v5, v6)`. Like `error`, but with six arguments.

## Examples

```rescript
Console.error6("Hello", "World", "from", "JS", "!!!", '!')
Console.error6([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"}, 42)
```
*/
@val
external error6: ('a, 'b, 'c, 'd, 'e, 'f) => unit = "console.error"

/**
`group(label)` creates a new "group" level with the given label.

See [`console.group`](https://developer.mozilla.org/en-US/docs/Web/API/console/group)
on MDN.

## Example

```rescript
Console.group("first group")
Console.group("second group")
Console.log("a message on the second level")
Console.groupEnd()
Console.log("a message message on the first level")
Console.groupEnd()
```
*/
@val
external group: string => unit = "console.group"

/**
`groupCollapsed(label)`. Like `group` but collapses the group initially.

See [`console.groupCollapsed`](https://developer.mozilla.org/en-US/docs/Web/API/console/groupCollapsed)
on MDN.
*/
@val
external groupCollapsed: string => unit = "console.groupCollapsed"

/**
`groupEnd()` ends the current group.

See [`console.groupEnd`](https://developer.mozilla.org/en-US/docs/Web/API/console/groupEnd)
on MDN.
*/
@val
external groupEnd: unit => unit = "console.groupEnd"

/**
`errorMany(arr)`. Like `error`, but variadic.

## Examples

```rescript
Console.errorMany(["Hello", "World"])
Console.errorMany([1, 2, 3])
```
*/
@val
@variadic
external errorMany: array<_> => unit = "console.error"

/**
`info(value)` print an informational message to console.

See [`console.info`](https://developer.mozilla.org/en-US/docs/Web/API/console/info)
on MDN.

## Examples

```rescript
Console.info("Information")
Console.info(("Hello", "JS"))
```
*/
@val
external info: 'a => unit = "console.info"

/**
`info2(v1, v2)`. Like `info`, but with two arguments.

## Examples

```rescript
Console.info2("Info", "failed to download")
Console.info2(#info, {"name": "ReScript"})
```
*/
@val
external info2: ('a, 'b) => unit = "console.info"

/**
`info3(v1, v2, v3)`. Like `info`, but with three arguments.

## Examples

```rescript
Console.info3("Hello", "World", "ReScript")
Console.info3([1, 2, 3], #4, #5)
```
*/
@val
external info3: ('a, 'b, 'c) => unit = "console.info"

/**
`info4(v1, v2, v3, v4)`. Like `info`, but with four arguments.

## Examples

```rescript
Console.info4("Hello", "World", "ReScript", '!')
Console.info4([1, 2, 3], #4, #5, #lastinfo)
```
*/
@val
external info4: ('a, 'b, 'c, 'd) => unit = "console.info"

/**
`info5(v1, v2, v3, v4, v5)`. Like `info`, but with five arguments.

## Examples

```rescript
Console.info5("Hello", "World", "from", "JS", "!!!")
Console.info5([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"})
```
*/
@val
external info5: ('a, 'b, 'c, 'd, 'e) => unit = "console.info"

/**
`info6(v1, v2, v3, v4, v5, v6)`. Like `info`, but with six arguments.

## Examples

```rescript
Console.info6("Hello", "World", "from", "JS", "!!!", '!')
Console.info6([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"}, 42)
```
*/
@val
external info6: ('a, 'b, 'c, 'd, 'e, 'f) => unit = "console.info"

/**
`infoMany(arr)`. Like `info`, but variadic.

## Examples

```rescript
Console.infoMany(["Hello", "World"])
Console.infoMany([1, 2, 3])
```
*/
@val
@variadic
external infoMany: array<_> => unit = "console.info"

/**
`log(value)` print a message to console.

See [`console.log`](https://developer.mozilla.org/en-US/docs/Web/API/console/log)
on MDN.

## Examples

```rescript
Console.log("Hello")
let obj = {"name": "ReScript", "version": 10}
Console.log(obj)
```
*/
@val
external log: 'a => unit = "console.log"

/**
`log2(v1, v2)`. Like `log`, but with two arguments.

## Examples

```rescript
Console.log2("Hello", "World")
Console.log2([1, 2, 3], '4')
```
*/
@val
external log2: ('a, 'b) => unit = "console.log"

/**
`log3(v1, v2, v3)`. Like `log`, but with three arguments.

## Examples

```rescript
Console.log3("Hello", "World", "ReScript")
Console.log3("One", 2, #3)
```
*/
@val
external log3: ('a, 'b, 'c) => unit = "console.log"

/**
`log4(v1, v2, v3, v4)`. Like `log`, but with four arguments.

## Examples

```rescript
Console.log4("Hello", "World", "ReScript", "!!!")
Console.log4([1, 2], (3, 4), [#5, #6], #"polyvar")
```
*/
@val
external log4: ('a, 'b, 'c, 'd) => unit = "console.log"

/**
`log5(v1, v2, v3, v4, v5)`. Like `log`, but with five arguments.

## Examples

```rescript
Console.log5("Hello", "World", "JS", '!', '!')
Console.log5([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"})
```
*/
@val
external log5: ('a, 'b, 'c, 'd, 'e) => unit = "console.log"

/**
`log6(v1, v2, v3, v4, v5, v6)`. Like `log`, but with six arguments.

## Examples

```rescript
Console.log6("Hello", "World", "JS", '!', '!', '?')
Console.log6([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"}, 42)
```
*/
@val
external log6: ('a, 'b, 'c, 'd, 'e, 'f) => unit = "console.log"

/**
`logMany(arr)`. Like `log`, but variadic.

## Examples

```rescript
Console.logMany(["Hello", "World"])
Console.logMany([1, 2, 3])
```
*/
@val
@variadic
external logMany: array<_> => unit = "console.log"

/**
`table(object)` displays an tabular view of the object in the console.

See [`console.table`](https://developer.mozilla.org/en-US/docs/Web/API/console/table)
on MDN.

## Examples

```rescript
Console.table({"language": "rescript", "version": "10.1.2"})
```
*/
@val
external table: 'a => unit = "console.table"

/**
`time(label)` creates a timer to measure how long an operation takes. `label`
must be a unique name. Call `console.timeEnd` with the same `label` to print
output time.

See [`console.time`](https://developer.mozilla.org/en-US/docs/Web/API/console/time)
on MDN.

## Examples

```rescript
Console.time("for_time")
for x in 3 downto 1 {
  Console.log(x)
  Console.timeLog("for_time")
}
Console.timeEnd("for_time")
```
*/
@val
external time: string => unit = "console.time"

/**
`timeEnd(label)` stops a timer created by `time`.

See [`console.timeEnd`](https://developer.mozilla.org/en-US/docs/Web/API/console/timeEnd)
on MDN.

## Examples

```rescript
Console.time("for_time")
for x in 3 downto 1 {
  Console.log(x)
  Console.timeLog("for_time")
}
Console.timeEnd("for_time")
```
*/
@val
external timeEnd: string => unit = "console.timeEnd"

/**
`timeLog(label)` prints the current elapsed time of the given timer to the console.

See [`console.timeLog`](https://developer.mozilla.org/en-US/docs/Web/API/console/timeLog)
on MDN.

## Examples

```rescript
Console.time("for_time")
for x in 3 downto 1 {
  Console.log(x)
  Console.timeLog("for_time")
}
Console.timeEnd("for_time")
```
*/
@val
external timeLog: string => unit = "console.timeLog"

/**
`trace()` print a stack trace to console.

See [`console.trace`](https://developer.mozilla.org/en-US/docs/Web/API/console/trace)
on MDN.

## Examples

```rescript
let main = () => {
  Console.trace()
}
main()
// In the console, the following trace will be displayed:
// main
// <anonymous>
```
*/
@val
external trace: unit => unit = "console.trace"

/**
`warn(value)` print a warning message to console.

See [`console.warn`](https://developer.mozilla.org/en-US/docs/Web/API/console/warn)
on MDN.

## Examples

```rescript
Console.warn("Warning")
Console.warn(("Warning", "invalid number"))
```
*/
@val
external warn: 'a => unit = "console.warn"

/**
`warn2(v1, v2)`. Like `warn`, but two arguments.

## Examples

```rescript
Console.warn2("Hello", "World")
Console.warn2([1, 2, 3], 4)
```
*/
@val
external warn2: ('a, 'b) => unit = "console.warn"

/**
`warn3(v1, v2, v3)`. Like `warn`, but three arguments.

## Examples

```rescript
Console.warn3("Hello", "World", "ReScript")
Console.warn3([1, 2, 3], #4, #5)
```
*/
@val
external warn3: ('a, 'b, 'c) => unit = "console.warn"

/**
`warn4(v1, v2, v3, v4)`. Like `warn`, but with four arguments.

## Examples

```rescript
Console.warn4("Hello", "World", "ReScript", "!!!")
Console.warn4(#first, #second, #third, ("fourth"))
```
*/
@val
external warn4: ('a, 'b, 'c, 'd) => unit = "console.warn"

/**
`warn5(v1, v2, v3, v4, v5)`. Like `warn`, but with five arguments.

## Examples

```rescript
Console.warn5("Hello", "World", "from", "JS", "!!!")
Console.warn5([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"})
```
*/
@val
external warn5: ('a, 'b, 'c, 'd, 'e) => unit = "console.warn"

/**
`warn6(v1, v2, v3, v4, v5, v6)`. Like `warn`, but with six arguments.

## Examples

```rescript
Console.warn6("Hello", "World", "from", "JS", "!!!", '!')
Console.warn6([1, 2], (3, 4), [#5, #6], #"polyvar", {"name": "ReScript"}, 42)
```
*/
@val
external warn6: ('a, 'b, 'c, 'd, 'e, 'f) => unit = "console.warn"

/**
`warnMany(arr)`. Like `warn`, but variadic.

## Examples

```rescript
Console.warnMany(["Hello", "World"])
Console.warnMany([1, 2, 3])
```
*/
@val
@variadic
external warnMany: array<_> => unit = "console.warn"
