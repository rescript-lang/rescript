switch %external(__DEV__) {
| Some(_) => Console.log("dev mode")
| None => Console.log("production mode")
}
