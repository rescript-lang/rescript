'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

var u = {
  contents: 0
};

function div(children, param) {
  for(var i = 0; i <= 1; ++i){
    u.contents = 300;
    console.log("nonline");
  }
}

function string(s) {
  for(var i = 0; i <= 1; ++i){
    u.contents = 200;
    console.log("no");
  }
}

function fn(authState, route) {
  var onboardingRoute;
  if (typeof authState === "object") {
    var exit = 0;
    if (typeof route === "object" && route.NAME === "Onboarding") {
      onboardingRoute = route.VAL;
    } else {
      exit = 2;
    }
    if (exit === 2) {
      console.log(authState.VAL);
      div({
            hd: string("VerifyEmail"),
            tl: /* [] */0
          }, undefined);
      return 2;
    }
    
  } else {
    var exit$1 = 0;
    if (typeof route === "object") {
      if (route.NAME === "Onboarding") {
        onboardingRoute = route.VAL;
      } else {
        exit$1 = 2;
      }
    } else {
      if (route === "SignUp" || route === "SignIn" || route === "Invite" || route === "PasswordReset") {
        div({
              hd: string("LoggedOut"),
              tl: /* [] */0
            }, undefined);
        return 1;
      }
      exit$1 = 2;
    }
    if (exit$1 === 2) {
      div({
            hd: string("Redirect"),
            tl: /* [] */0
          }, undefined);
      return 3;
    }
    
  }
  console.log(onboardingRoute);
  div({
        hd: string("Onboarding"),
        tl: /* [] */0
      }, undefined);
  return 0;
}

eq("File \"gpr_4280_test.res\", line 42, characters 3-10", fn("Unauthenticated", "Invite"), 1);

eq("File \"gpr_4280_test.res\", line 43, characters 3-10", fn("Unauthenticated", {
          NAME: "Onboarding",
          VAL: 0
        }), 0);

eq("File \"gpr_4280_test.res\", line 44, characters 3-10", fn({
          NAME: "Unverified",
          VAL: 0
        }, "Invite"), 2);

eq("File \"gpr_4280_test.res\", line 45, characters 3-10", fn("Unauthenticated", "xx"), 3);

Mt.from_pair_suites("gpr_4280_test.res", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.div = div;
exports.string = string;
exports.fn = fn;
/*  Not a pure module */
