external getLogger:
  unit => {
		"log": 'a => unit,
		"log2": 'a. int => int,
		"log3": 'a 'b. ('a, 'b) => int
	} =
"./src/logger.mock.js"

// polytype in label_declaration doesn't have attributes
type reducer<'state> = {
  state: @attr 'state,
  send: 'action. @attr ('state, 'action) => 'action
}
