let isPositiveStringInteger = str => {
  // TODO: this should possibly use something like - https://stackoverflow.com/a/48569311/3103033 ?

  // NOTE: This allows leading 0s (which seams to not be a problem for web3 or truffle)
  //       This doesn't check if the number is smaller than 2^256 which is the largest integer ethereum can handle
  let f = /^([0-9]\d*)$/

  Js.Re.test_(f, str)
}

let elipsify: (string, int) => string = (inputString, maxLength) =>
  if inputString->String.length > maxLength {
    String.sub(inputString, 0, maxLength - 3) ++ "..."
  } else {
    inputString
  }

let elipsifyMiddle: (string, int, int) => string = (inputString, maxLength, trailingCharacters) => {
  let stringLength = inputString->String.length

  if stringLength > maxLength && trailingCharacters + maxLength < stringLength {
    String.sub(inputString, 0, maxLength - 3) ++
    ("..." ++
    String.sub(inputString, Js.Math.abs_int(stringLength - trailingCharacters), trailingCharacters))
  } else {
    inputString
  }
}

let bnToMoment = bn =>
  bn
  ->BN.toString
  ->int_of_string /* trusting that gql will be reliable here */
  ->MomentRe.momentWithUnix
