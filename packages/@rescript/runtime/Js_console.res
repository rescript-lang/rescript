@val @scope("console") external log: 'a => unit = "log"
@val @scope("console") external log2: ('a, 'b) => unit = "log"
@val @scope("console") external log3: ('a, 'b, 'c) => unit = "log"
@val @scope("console") external log4: ('a, 'b, 'c, 'd) => unit = "log"
@val @scope("console") @variadic external logMany: array<'a> => unit = "log"

@val @scope("console") external info: 'a => unit = "info"
@val @scope("console") external info2: ('a, 'b) => unit = "info"
@val @scope("console") external info3: ('a, 'b, 'c) => unit = "info"
@val @scope("console") external info4: ('a, 'b, 'c, 'd) => unit = "info"
@val @scope("console") @variadic external infoMany: array<'a> => unit = "info"

@val @scope("console") external warn: 'a => unit = "warn"
@val @scope("console") external warn2: ('a, 'b) => unit = "warn"
@val @scope("console") external warn3: ('a, 'b, 'c) => unit = "warn"
@val @scope("console") external warn4: ('a, 'b, 'c, 'd) => unit = "warn"
@val @scope("console") @variadic external warnMany: array<'a> => unit = "warn"

@val @scope("console") external error: 'a => unit = "error"
@val @scope("console") external error2: ('a, 'b) => unit = "error"
@val @scope("console") external error3: ('a, 'b, 'c) => unit = "error"
@val @scope("console") external error4: ('a, 'b, 'c, 'd) => unit = "error"
@val @scope("console") @variadic external errorMany: array<'a> => unit = "error"

@val @scope("console") external trace: unit => unit = "trace"

@val @scope("console") external timeStart: string => unit = "time"

@val @scope("console") external timeEnd: string => unit = "timeEnd"
