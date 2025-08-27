let ok = Ok(1)

let x = {
  let? Ok(_x) = ok
  Ok()
}
