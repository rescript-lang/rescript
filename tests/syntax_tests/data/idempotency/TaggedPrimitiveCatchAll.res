@tag("kind")
type response =
  | @as(202) Ok202({code: int})
  | @as(200) Ok200({code: int})
  | @catch(int) Other({kind: int, body: string})

let decode = (x: response) =>
  switch x {
  | Ok202(r) => r.code + 1
  | Other(r) => r.kind + 2
  | Ok200(r) => r.code + 3
  }

@tag("kind")
type noExposedDiscriminantField =
  | @as("ok") Ok({value: int})
  | @catch(string) Other({value: int})
