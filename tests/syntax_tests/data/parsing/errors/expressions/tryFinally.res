let a = try dangerousCall() finally cleanup()

let b = try {
  dangerousCall()
} catch {
| _ => ()
} finally
