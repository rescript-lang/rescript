let formatDate = (~fmt=?, s) => s

let deadCaller = () => formatDate(~fmt="ISO", "2024-01-01")

let liveCaller = () => formatDate("2024-01-01")

let _ = liveCaller()

