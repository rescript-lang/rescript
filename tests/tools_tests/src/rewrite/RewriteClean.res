let flag = true
let useValue = (~value=?, ()) => value

let direct = switch flag {
| true => 1
| false => 2
}

let withGuard = switch () {
| _ if flag => 1
| _ => 2
}

let value = useValue(~value=1, ())
