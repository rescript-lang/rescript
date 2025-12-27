// Using stdlib types directly
let myList: List.t<int> = list{1, 2, 3}
let myOption: Option.t<string> = Some("hello")
let myResult: Result.t<int, string> = Ok(42)
let myDict: Dict.t<float> = dict{"pi": 3.14}
let myNull: Null.t<int> = Null.make(42)
let myNullable: Nullable.t<string> = Nullable.make("hello")

// Using Js module types (legacy)
let jsNull: Js.Null.t<int> = Js.Null.return(42)
let jsNullable: Js.Nullable.t<string> = Js.Nullable.return("hello")
