await wait(2)

let maybeSomeValue = switch await fetchData(url) {    
| data => Some(data)
| exception JsExn(_) => None
}

let x = await 1 + 2

let x = await wait(1) + await wait(2)

let () = {
  let response = await fetch("/users.json"); // get users list
  let users = await response.json(); // parse JSON
  let comments = (await (await fetch("comment.json")).json())[0];
  Js.log2(users, comments)
}

let () = {
    await delay(10)
}

let () = {
    await delay(10)
    await delay(20)
}

let forEach = await @a @b Js.Import(Belt.List.forEach)

module M = await @a @b Belt.List

let f = () => {
  module M = await @a @b Belt.List
  M.forEach
}

let () = {
  module M = await @a @b Belt.List
  M.forEach
}

module type BeltList = module type of Belt.List

let f = () => {
  module M = await @a @b (Belt.List: BeltList)
  M.forEach
}

let () = {
  module M = await @a @b (Belt.List: BeltList)
  M.forEach
}
