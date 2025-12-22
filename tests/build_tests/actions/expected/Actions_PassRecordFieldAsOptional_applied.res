type record = {a: int, test?: bool}
let test = Some(true)

let x = {a: 10, ?test}

/* === AVAILABLE ACTIONS:
- ChangeRecordFieldOptional(true) - Pass field as optional
*/
