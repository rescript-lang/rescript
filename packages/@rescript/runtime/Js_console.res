@deprecated({
  reason: "Use `Console.log` instead.",
  migrate: Console.log(),
})
@val
@scope("console")
external log: 'a => unit = "log"

@deprecated({
  reason: "Use `Console.log2` instead.",
  migrate: Console.log2(),
})
@val
@scope("console")
external log2: ('a, 'b) => unit = "log"

@deprecated({
  reason: "Use `Console.log3` instead.",
  migrate: Console.log3(),
})
@val
@scope("console")
external log3: ('a, 'b, 'c) => unit = "log"

@deprecated({
  reason: "Use `Console.log4` instead.",
  migrate: Console.log4(),
})
@val
@scope("console")
external log4: ('a, 'b, 'c, 'd) => unit = "log"

@deprecated({
  reason: "Use `Console.logMany` instead.",
  migrate: Console.logMany(),
})
@val
@scope("console")
@variadic
external logMany: array<'a> => unit = "log"

@deprecated({
  reason: "Use `Console.info` instead.",
  migrate: Console.info(),
})
@val
@scope("console")
external info: 'a => unit = "info"

@deprecated({
  reason: "Use `Console.info2` instead.",
  migrate: Console.info2(),
})
@val
@scope("console")
external info2: ('a, 'b) => unit = "info"

@deprecated({
  reason: "Use `Console.info3` instead.",
  migrate: Console.info3(),
})
@val
@scope("console")
external info3: ('a, 'b, 'c) => unit = "info"

@deprecated({
  reason: "Use `Console.info4` instead.",
  migrate: Console.info4(),
})
@val
@scope("console")
external info4: ('a, 'b, 'c, 'd) => unit = "info"

@deprecated({
  reason: "Use `Console.infoMany` instead.",
  migrate: Console.infoMany(),
})
@val
@scope("console")
@variadic
external infoMany: array<'a> => unit = "info"

@deprecated({
  reason: "Use `Console.warn` instead.",
  migrate: Console.warn(),
})
@val
@scope("console")
external warn: 'a => unit = "warn"

@deprecated({
  reason: "Use `Console.warn2` instead.",
  migrate: Console.warn2(),
})
@val
@scope("console")
external warn2: ('a, 'b) => unit = "warn"

@deprecated({
  reason: "Use `Console.warn3` instead.",
  migrate: Console.warn3(),
})
@val
@scope("console")
external warn3: ('a, 'b, 'c) => unit = "warn"

@deprecated({
  reason: "Use `Console.warn4` instead.",
  migrate: Console.warn4(),
})
@val
@scope("console")
external warn4: ('a, 'b, 'c, 'd) => unit = "warn"

@deprecated({
  reason: "Use `Console.warnMany` instead.",
  migrate: Console.warnMany(),
})
@val
@scope("console")
@variadic
external warnMany: array<'a> => unit = "warn"

@deprecated({
  reason: "Use `Console.error` instead.",
  migrate: Console.error(),
})
@val
@scope("console")
external error: 'a => unit = "error"

@deprecated({
  reason: "Use `Console.error2` instead.",
  migrate: Console.error2(),
})
@val
@scope("console")
external error2: ('a, 'b) => unit = "error"

@deprecated({
  reason: "Use `Console.error3` instead.",
  migrate: Console.error3(),
})
@val
@scope("console")
external error3: ('a, 'b, 'c) => unit = "error"

@deprecated({
  reason: "Use `Console.error4` instead.",
  migrate: Console.error4(),
})
@val
@scope("console")
external error4: ('a, 'b, 'c, 'd) => unit = "error"

@deprecated({
  reason: "Use `Console.errorMany` instead.",
  migrate: Console.errorMany(),
})
@val
@scope("console")
@variadic
external errorMany: array<'a> => unit = "error"

@deprecated({
  reason: "Use `Console.trace` instead.",
  migrate: Console.trace(),
})
@val
@scope("console")
external trace: unit => unit = "trace"

@deprecated({
  reason: "Use `Console.time` instead.",
  migrate: Console.time(),
})
@val
@scope("console")
external timeStart: string => unit = "time"

@deprecated({
  reason: "Use `Console.timeEnd` instead.",
  migrate: Console.timeEnd(),
})
@val
@scope("console")
external timeEnd: string => unit = "timeEnd"

@deprecated({
  reason: "Use `Console.table` instead.",
  migrate: Console.table(),
})
@val
@scope("console")
external table: 'a => unit = "table"
