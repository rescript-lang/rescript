@@warning("-39")
// https://github.com/rescript-lang/rescript-compiler/issues/4511
/*
[@config {
  flags : [|"-dsource"|]
}];
*/
module Rec = {
  @react.component
  let rec make = (~b) => {
    mm({b: b})
  }
  and mm = x => make({b: !x.b})
}
