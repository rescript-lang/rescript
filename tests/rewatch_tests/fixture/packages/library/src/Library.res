/***A library for greetings and users.*/
let greeting = "hello from library"

type user = {name: string}
let admin: user = {name: "admin"}
let greet = (name: string) => "hello " ++ name
