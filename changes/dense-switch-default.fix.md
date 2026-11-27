- Fix a dense `switch` on ints or chars losing its default arm when its highest
  case is unreachable, so that it returned `undefined` for values above the
  cases.
