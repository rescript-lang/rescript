type provider

type t

type abi

type ethAddress = string
type ethersBigNumber = {toString: (. unit) => string}

type rawProvider

type rec web3Library = {
  getBalance: (. ethAddress) => Promise.promise<option<ethersBigNumber>>,
  getSigner: (. ethAddress) => web3Library,
  provider: rawProvider,
}

type rpcDefinition = {
  method: string,
  params: array<string>,
  from: ethAddress,
}

type error
type result
type rpcResult = {result: result}

@send
external sendAsync: (
  rawProvider,
  rpcDefinition,
  (. Js.Nullable.t<error>, rpcResult) => unit,
) => unit = "sendAsync"

@new @module("web3") external new_: rawProvider => t = "default"

@send @scope(("eth", "personal"))
external personalSign: (t, string, ethAddress) => Js.Promise.t<string> = "sign"

module Contract = {
  type sendParams = {from: ethAddress}
  type txSendResult
  type contractMethod = {
    encodeABI: unit => string,
    send: sendParams => Js.Promise.t<txSendResult>,
  }

  type contract

  type dai

  // [@send] [@new] [@scope "eth"]
  // external getContract: (t, abi, ethAddress) => contract = "Contract";

  // Temporary code until I work out how to make the above binding work...
  let getContract: (. t, abi, ethAddress) => contract = %raw(`
      (web3, abi, contractAddress) => new web3.eth.Contract(abi, contractAddress)
    `)

  module MaticSteward = {
    type steward

    @send @scope("methods")
    external depositWithPermit: (
      steward,
      BN.t,
      BN.t,
      bool,
      int,
      string,
      string,
      ethAddress,
      BN.t,
    ) => contractMethod = "depositWithPermit"

    @send @scope("methods")
    external buyWithPermit: (
      steward,
      //   nonce,
      BN.t,
      //   expiry,
      BN.t,
      //   allowed,
      bool,
      //   v,
      int,
      //   r,
      string,
      //   s,
      string,
      //   tokenId,
      string,
      //   _newPrice,
      string,
      //   previousPrice,
      string,
      //   serviceProviderPercentage,
      string,
      //   depositAmount,
      string,
    ) => contractMethod = "buyWithPermit"

    @send @scope("methods")
    external buyAuctionWithPermit: (
      steward,
      //   nonce,
      BN.t,
      //   expiry,
      BN.t,
      //   allowed,
      bool,
      //   v,
      int,
      //   r,
      string,
      //   s,
      string,
      //   tokenId,
      string,
      //   _newPrice,
      string,
      //   serviceProviderPercentage,
      string,
      //   depositAmount,
      string,
    ) => contractMethod = "buyAuctionWithPermit"

    @send @scope("methods")
    external withdrawDeposit: (steward, ethAddress) => contractMethod = "withdrawDeposit"

    @module("./abis/abis.json")
    external stewardMaticAbi: abi = "stewardMatic"

    let getStewardContract = (web3, address): steward =>
      getContract(. web3, stewardMaticAbi, address)->Obj.magic
  }
}
