// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Js_option = require("rescript/lib/js/js_option.js");

function simpleEq(a, b) {
  return a === b;
}

let option_suites_0 = [
  "option_isSome_Some",
  param => ({
    TAG: "Eq",
    _0: true,
    _1: Js_option.isSome(1)
  })
];

let option_suites_1 = {
  hd: [
    "option_isSome_None",
    param => ({
      TAG: "Eq",
      _0: false,
      _1: Js_option.isSome(undefined)
    })
  ],
  tl: {
    hd: [
      "option_isNone_Some",
      param => ({
        TAG: "Eq",
        _0: false,
        _1: Js_option.isNone(1)
      })
    ],
    tl: {
      hd: [
        "option_isNone_None",
        param => ({
          TAG: "Eq",
          _0: true,
          _1: Js_option.isNone(undefined)
        })
      ],
      tl: {
        hd: [
          "option_isSomeValue_Eq",
          param => ({
            TAG: "Eq",
            _0: true,
            _1: Js_option.isSomeValue(simpleEq, 2, 2)
          })
        ],
        tl: {
          hd: [
            "option_isSomeValue_Diff",
            param => ({
              TAG: "Eq",
              _0: false,
              _1: Js_option.isSomeValue(simpleEq, 1, 2)
            })
          ],
          tl: {
            hd: [
              "option_isSomeValue_DiffNone",
              param => ({
                TAG: "Eq",
                _0: false,
                _1: Js_option.isSomeValue(simpleEq, 1, undefined)
              })
            ],
            tl: {
              hd: [
                "option_getExn_Some",
                param => ({
                  TAG: "Eq",
                  _0: 2,
                  _1: Js_option.getExn(2)
                })
              ],
              tl: {
                hd: [
                  "option_equal_Eq",
                  param => ({
                    TAG: "Eq",
                    _0: true,
                    _1: Js_option.equal(simpleEq, 2, 2)
                  })
                ],
                tl: {
                  hd: [
                    "option_equal_Diff",
                    param => ({
                      TAG: "Eq",
                      _0: false,
                      _1: Js_option.equal(simpleEq, 1, 2)
                    })
                  ],
                  tl: {
                    hd: [
                      "option_equal_DiffNone",
                      param => ({
                        TAG: "Eq",
                        _0: false,
                        _1: Js_option.equal(simpleEq, 1, undefined)
                      })
                    ],
                    tl: {
                      hd: [
                        "option_andThen_SomeSome",
                        param => ({
                          TAG: "Eq",
                          _0: true,
                          _1: Js_option.isSomeValue(simpleEq, 3, Js_option.andThen(a => a + 1 | 0, 2))
                        })
                      ],
                      tl: {
                        hd: [
                          "option_andThen_SomeNone",
                          param => ({
                            TAG: "Eq",
                            _0: false,
                            _1: Js_option.isSomeValue(simpleEq, 3, Js_option.andThen(param => {}, 2))
                          })
                        ],
                        tl: {
                          hd: [
                            "option_map_Some",
                            param => ({
                              TAG: "Eq",
                              _0: true,
                              _1: Js_option.isSomeValue(simpleEq, 3, Js_option.map(a => a + 1 | 0, 2))
                            })
                          ],
                          tl: {
                            hd: [
                              "option_map_None",
                              param => ({
                                TAG: "Eq",
                                _0: undefined,
                                _1: Js_option.map(a => a + 1 | 0, undefined)
                              })
                            ],
                            tl: {
                              hd: [
                                "option_default_Some",
                                param => ({
                                  TAG: "Eq",
                                  _0: 2,
                                  _1: Js_option.getWithDefault(3, 2)
                                })
                              ],
                              tl: {
                                hd: [
                                  "option_default_None",
                                  param => ({
                                    TAG: "Eq",
                                    _0: 3,
                                    _1: Js_option.getWithDefault(3, undefined)
                                  })
                                ],
                                tl: {
                                  hd: [
                                    "option_filter_Pass",
                                    param => ({
                                      TAG: "Eq",
                                      _0: true,
                                      _1: Js_option.isSomeValue(simpleEq, 2, Js_option.filter(a => a % 2 === 0, 2))
                                    })
                                  ],
                                  tl: {
                                    hd: [
                                      "option_filter_Reject",
                                      param => ({
                                        TAG: "Eq",
                                        _0: undefined,
                                        _1: Js_option.filter(a => a % 3 === 0, 2)
                                      })
                                    ],
                                    tl: {
                                      hd: [
                                        "option_filter_None",
                                        param => ({
                                          TAG: "Eq",
                                          _0: undefined,
                                          _1: Js_option.filter(a => a % 3 === 0, undefined)
                                        })
                                      ],
                                      tl: {
                                        hd: [
                                          "option_firstSome_First",
                                          param => ({
                                            TAG: "Eq",
                                            _0: true,
                                            _1: Js_option.isSomeValue(simpleEq, 3, Js_option.firstSome(3, 2))
                                          })
                                        ],
                                        tl: {
                                          hd: [
                                            "option_firstSome_First",
                                            param => ({
                                              TAG: "Eq",
                                              _0: true,
                                              _1: Js_option.isSomeValue(simpleEq, 2, Js_option.firstSome(undefined, 2))
                                            })
                                          ],
                                          tl: {
                                            hd: [
                                              "option_firstSome_None",
                                              param => ({
                                                TAG: "Eq",
                                                _0: undefined,
                                                _1: Js_option.firstSome(undefined, undefined)
                                              })
                                            ],
                                            tl: /* [] */0
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

let option_suites = {
  hd: option_suites_0,
  tl: option_suites_1
};

Mt.from_pair_suites("Js_option_test", option_suites);

exports.simpleEq = simpleEq;
exports.option_suites = option_suites;
/*  Not a pure module */
