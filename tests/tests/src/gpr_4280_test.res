open Mocha
open Test_utils

let u = ref(0)
let div = (~children, ()) =>
  for i in 0 to 1 {
    u := 300
    Js.log("nonline")
  }

let string = (s: string) =>
  for i in 0 to 1 {
    u := 200
    Js.log("no")
  }

let fn = (authState, route) =>
  switch (authState, route) {
  | (#Unauthenticated, #Onboarding(onboardingRoute))
  | (#Unverified(_), #Onboarding(onboardingRoute)) =>
    Js.Console.log(onboardingRoute)
    div(~children=list{string("Onboarding")}, ())
    0
  | (#Unauthenticated, #SignIn)
  | (#Unauthenticated, #SignUp)
  | (#Unauthenticated, #Invite)
  | (#Unauthenticated, #PasswordReset) =>
    div(~children=list{string("LoggedOut")}, ())
    1

  | (#Unverified(user), _) =>
    Js.Console.log(user)
    div(~children=list{string("VerifyEmail")}, ())
    2
  | (#Unauthenticated, _) =>
    div(~children=list{string("Redirect")}, ())
    3
  }

describe(__MODULE__, () => {
  test("fn with Unauthenticated and Invite", () => eq(__LOC__, fn(#Unauthenticated, #Invite), 1))
  test("fn with Unauthenticated and Onboarding", () =>
    eq(__LOC__, fn(#Unauthenticated, #Onboarding(0)), 0)
  )
  test("fn with Unverified and Invite", () => eq(__LOC__, fn(#Unverified(0), #Invite), 2))
  test("fn with Unauthenticated and unknown route", () => eq(__LOC__, fn(#Unauthenticated, #xx), 3))
})
