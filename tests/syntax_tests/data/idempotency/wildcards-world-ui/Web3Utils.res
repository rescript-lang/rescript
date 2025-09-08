open Globals

@module("web3-utils")
external fromWei: (string, string) => string = "fromWei"

@module("web3-utils")
external fromWeiBN: (BN.t, string) => string = "fromWei"

@module("web3-utils") external isBN: string => bool = "isBN"

@module("web3-utils") external toWei: (string, string) => string = "toWei"

let fromWeiToEth = value => fromWei(value, "ether")
let fromWeiBNToEth = value => fromWeiBN(value, "ether")
let fromWeiBNToEthPrecision = (value, ~digits) =>
  value
  ->fromWeiBNToEth
  ->Float.fromString
  ->Belt.Option.mapWithDefault(0., a => a)
  ->toFixedWithPrecisionNoTrailingZeros(~digits)

let toWeiFromEth = value => toWei(value, "ether")
