external getLogger:
  unit => {
		"log": 'a => unit,
		"log2": 'a. int => int,
		"log3": 'a 'b. ('a, 'b) => int
	} =
"./src/logger.mock.js"
