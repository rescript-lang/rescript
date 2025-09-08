module Device = unpack({
  let deviceName = parseCmdline()
  try Hashtbl.find(devices, deviceName) catch {
  | Not_found => exit(2)
  }
})

module Device = unpack({
  let deviceName = parseCmdline()
  try Hashtbl.find(devices, deviceName) catch {
  | Not_found => exit(2)
  }
} : Device)

let draw_using_device = (device_name, picture) => {
  module Device = unpack(Hashtbl.find(devices, device_name, test1, test2, test3, test4, test5, test6): DEVICEEEEEEEEEEEEEEEEEEEE)
  Device.draw(picture)
}

module New_three = unpack(three: X_int)

let to_int = (m) => {
  module M = unpack(m : X_int)
  M.x
}
