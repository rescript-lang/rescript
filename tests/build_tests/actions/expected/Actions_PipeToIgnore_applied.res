// actionFilter=PipeToIgnore
switch 1 {
| _ => "one"
}->ignore

/* === AVAILABLE ACTIONS:
- AssignToUnderscore - Assign to let _ =
- PipeToIgnore - Pipe to ignore()
*/
