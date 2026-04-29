type t =
  | @as("one") One({thing: string})
  | @catch(string) Other({kind: string, thing: int})
