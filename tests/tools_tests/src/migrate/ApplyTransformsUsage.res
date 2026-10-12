let task = ApplyTransforms.make((~resolve, ~reject as _) => resolve(1))

let total = ApplyTransforms.add(~x=1, ~y=2, ())
