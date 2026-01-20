let x = {
  module M = ME
  Me.x
}

let x = {
  module M = await ME
  M.x
}

let x = {
  module M = await (ME: MT)
  M.x
}
