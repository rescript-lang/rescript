let simple = {"a": 1, "b": true}

let nested = {"x": {"y": {"z": 3}}, "s": "hello"}

let read = nested["x"]["y"]

let write = (o: {.."a": int}) => o["a"] = 2

let inAcall = Console.log({"tag": "point", "value": 1})
