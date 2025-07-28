// actionFilter=InsertMissingArguments
let x = (~a, ~b) => a + b
let y = x(~a=2, ~b=%todo) + 2

/* === AVAILABLE ACTIONS:
- PartiallyApplyFunction - Partially apply function
- InsertMissingArguments(~b) - Insert missing arguments
*/
