open Globals
open Ethers

type txHash = string
type tx = {
  hash: txHash,
  wait: (. unit) => Promise.Js.t<txResult, txError>,
}
type parsedUnits
type txOptions = {
  gasLimit: string,
  value: parsedUnits,
}
type tokenIdString = string
type estimateBuy = {
  buy: // (. string, parsedUnits, txOptions) =>
  (. string, parsedUnits, parsedUnits, txOptions) => Promise.Js.t<string, string>,
}
type stewardContract = {
  estimate: estimateBuy,
  buy: (. tokenIdString, parsedUnits, parsedUnits, string, txOptions) => Promise.Js.t<tx, txError>,
  buyAuction: (. tokenIdString, parsedUnits, string, txOptions) => Promise.Js.t<tx, txError>,
  depositWei: (. txOptions) => Promise.Js.t<tx, txError>,
  withdrawDeposit: (. parsedUnits, txOptions) => Promise.Js.t<tx, txError>,
  _collectPatronagePatron: (. string, txOptions) => Promise.Js.t<tx, txError>,
  changePrice: (. tokenIdString, parsedUnits, txOptions) => Promise.Js.t<tx, txError>,
}

type ethersBnFormat
@send external ethersBnToString: ethersBnFormat => string = "toString"

type loyaltyTokenContract = {
  // approve(address to, uint256 tokenId)
  allowance: (. Web3.ethAddress, Web3.ethAddress) => Js.Promise.t<ethersBnFormat>,
  balanceOf: (. Web3.ethAddress) => Js.Promise.t<ethersBnFormat>,
  approve: (. Web3.ethAddress, string, txOptions) => Promise.Js.t<tx, txError>,
}

@new @module("ethers")
external getContract: (Web3.ethAddress, Web3.abi, Web3.web3Library) => stewardContract = "Contract"

@new @module("ethers")
external getLoyaltyTokenContract: (
  Web3.ethAddress,
  Web3.abi,
  Web3.web3Library,
) => loyaltyTokenContract = "Contract"

@module("./abi/loyaltyToken.json")
external loyaltyTokenAbi: Web3.abi = "loyaltyToken"

@module("ethers") @scope("utils")
external parseUnits: (. string, int) => parsedUnits = "parseUnits"

let getExchangeContract = (stewardAddress, stewardAbi, library, account) =>
  getContract(stewardAddress, stewardAbi, ContractUtil.getProviderOrSigner(library, account))
let getLoyaltyTokenContract = (stewardAddress, library, account) =>
  getLoyaltyTokenContract(
    stewardAddress,
    loyaltyTokenAbi,
    ContractUtil.getProviderOrSigner(library, account),
  )

let stewardAddressMainnet = "0x6D47CF86F6A490c6410fC082Fd1Ad29CF61492d0"
let stewardAddressGoerli = "0x0C00CFE8EbB34fE7C31d4915a43Cde211e9F0F3B"
let stewardAddressRinkeby = "0x229Cb219F056A9097b2744594Bc37597380854E8"

let loyaltyTokenAddressMainnet = "0x773c75c2277eD3e402BDEfd28Ec3b51A3AfbD8a4"
let loyaltyTokenAddressGoerli = "0xd7d8c42ab5b83aa3d4114e5297989dc27bdfb715"
// let loyaltyTokenAddressRinkbey = "0xd7d8c42ab5b83aa3d4114e5297989dc27bdfb715";

let stewardAddressMaticMain = "0x6D47CF86F6A490c6410fC082Fd1Ad29CF61492d0"
let stewardAddressMumbai = "0x0C00CFE8EbB34fE7C31d4915a43Cde211e9F0F3B"

let loyaltyTokenAddressMaticMain = "0x773c75c2277eD3e402BDEfd28Ec3b51A3AfbD8a4"
let loyaltyTokenAddressMumbai = "0xd7d8c42ab5b83aa3d4114e5297989dc27bdfb715"
// let loyaltyTokenAddressRinkbey = "0xd7d8c42ab5b83aa3d4114e5297989dc27bdfb715";

let getDaiContractAddress = (chain: Client.context, chainId) =>
  switch chain {
  | Neither
  | MaticQuery =>
    switch chainId {
    | 137 => "0x8f3Cf7ad23Cd3CaDbD9735AFf958023239c6A063"
    | 80001 => "0xeb37A6dF956F1997085498aDd98b25a2f633d83F"
    | 5
    | _ => "0xba97BeC8d359D73c81D094421803D968A9FBf676"
    }
  | MainnetQuery => "NEVER"
  }

let getStewardAddress = (chain: Client.context, chainId) =>
  switch chain {
  | Neither
  | MaticQuery =>
    switch chainId {
    | 137 => "0x69895ba53B4CB7afaea2Ab519409F3d3C613a562"
    | 80001 => "0xE44056eff470b1e505c3776601685c97A6966887"
    | 5
    | _ => "0xF26F8B2c178a0DeBB176c6b18e3F6d243fEEf828"
    }
  | MainnetQuery => "TODO"
  }

let getMaticNetworkName = chainId =>
  switch chainId {
  | 137 => "matic"
  | 80001 => "mumbai"
  | 5
  | _ => "goerli"
  }

let getChildChainId = parentChainId =>
  switch parentChainId {
  | 1 => 137
  | 5 => 80001
  | 4
  | _ => 5
  }

let useStewardAbi = () =>
  switch RootProvider.useStewardAbi() {
  | Some(abi) => abi
  | None => %raw(`require("./abi/steward.json").stewardAbi`)
  }

let defaultStewardAddressFromChainId = x =>
  switch x {
  | 1 => Some(stewardAddressMainnet)
  | 4 => Some(stewardAddressRinkeby)
  | 5 => Some(stewardAddressGoerli)
  | _ => None
  }
let useStewardAddress = () => {
  let externallySetAddress = RootProvider.useStewardContractAddress()
  chainId =>
    externallySetAddress->Belt.Option.mapWithDefault(
      defaultStewardAddressFromChainId(chainId),
      a => Some(a),
    )
}
let loyaltyTokenAddressFromChainId = x =>
  switch x {
  | 1 => Some(loyaltyTokenAddressMainnet)
  | 5 => Some(loyaltyTokenAddressGoerli)
  | _ => None
  }

let useStewardContract = () => {
  let context = RootProvider.useWeb3React()
  let stewardContractAddress = useStewardAddress()
  let stewardAbi = useStewardAbi()

  React.useMemo3(() =>
    switch (context.library, context.chainId) {
    | (Some(library), Some(chainId)) =>
      stewardContractAddress(chainId)->oMap(
        getExchangeContract(_, stewardAbi, library, context.account),
      )

    | _ => None
    }
  , (context.library, context.account, context.chainId))
}

let useLoyaltyTokenContract = () => {
  let context = RootProvider.useWeb3React()

  React.useMemo3(() =>
    switch (context.library, context.chainId) {
    | (Some(library), Some(chainId)) =>
      chainId
      ->loyaltyTokenAddressFromChainId
      ->oMap(getLoyaltyTokenContract(_, library, context.account))

    | _ => None
    }
  , (context.library, context.account, context.chainId))
}

type transactionState =
  | UnInitialised
  | DaiPermit(BN.t)
  | SignMetaTx
  | Created
  | SubmittedMetaTx
  | SignedAndSubmitted(txHash)
  // TODO: get the error message when it is declined.
  //      4001 - means the transaction was declined by the signer
  //      -32000 - means the transaction is always failing (exceeds gas allowance)
  | Declined(string)
  // | DaiPermitDclined(string)
  // | SignMetaTxDclined(string)
  | ServerError(string)
  | Complete(txResult)
  | Failed

module ExecuteMetaTxMutation = %graphql(`
    mutation (
      $network: String!,
      $r: String!,
      $s: String!,
      $v: Int!,
      $userAddress: String!,
      $functionSignature: String!
    ) {
      metaTx(
        functionSignature: $functionSignature,
        network: $network ,
        r: $r,
        s: $s,
        userAddress: $userAddress,
        v: $v
      ) {
        txHash
        success
        errorMsg
      }
    }
  `)

let execDaiPermitMetaTx = (
  daiNonce,
  networkName,
  stewardNonce,
  setTxState,
  sendMetaTx: ApolloClient__React_Hooks_UseMutation.MutationTuple.t_mutationFn<
    ExecuteMetaTxMutation.ExecuteMetaTxMutation_inner.t,
    ExecuteMetaTxMutation.ExecuteMetaTxMutation_inner.t_variables,
    ExecuteMetaTxMutation.ExecuteMetaTxMutation_inner.Raw.t_variables,
  >,
  userAddress,
  spender,
  lib: Web3.web3Library,
  generateFunctionSignature: (
    Web3.Contract.MaticSteward.steward,
    DaiPermit.v,
    DaiPermit.r,
    DaiPermit.s,
  ) => string,
  chainId,
  verifyingContract,
) =>
  DaiPermit.createPermitSig(
    lib.provider,
    verifyingContract,
    daiNonce,
    chainId,
    userAddress,
    spender,
  )
  ->Js.Promise.then_(rsvSig => {
    setTxState(_ => SignMetaTx)
    open ContractUtil
    let {r, s, v} = rsvSig

    let web3 = Web3.new_(lib.provider)

    let steward = Web3.Contract.MaticSteward.getStewardContract(web3, spender)

    let functionSignature = generateFunctionSignature(steward, v, r, s)

    let messageToSign = ContractUtil.constructMetaTransactionMessage(
      stewardNonce,
      chainId->BN.toString,
      functionSignature,
      spender,
    )

    web3
    ->Web3.personalSign(messageToSign, userAddress)
    ->Js.Promise.then_(signature => Js.Promise.resolve((functionSignature, signature)), _)
  }, _)
  ->Js.Promise.then_(((functionSignature, signature)) => {
    open ContractUtil
    let {r, s, v} = getEthSig(signature)
    sendMetaTx(
      ExecuteMetaTxMutation.makeVariables(
        ~network=networkName,
        ~r,
        ~s,
        ~v,
        ~functionSignature,
        ~userAddress,
        (),
      ),
    )->Js.Promise.resolve
  }, _)
  ->Js.Promise.catch(err => {
    Js.log2("this error was caught", err)
    Js.Promise.resolve(""->Obj.magic)
  }, _)
  ->ignore

let handleMetaTxSumbissionState = (
  result: ApolloClient__React_Types.MutationResult.t<
    ExecuteMetaTxMutation.ExecuteMetaTxMutation_inner.t,
  >,
  setTxState,
  currentState,
) => {
  let networkId = RootProvider.useNetworkId()

  switch result {
  | {called: false, _} => ()
  //  | {loading: true} =>
  | {data: Some(data), error: None, _} =>
    let success = data.metaTx.success
    let errorMsg = data.metaTx.errorMsg
    let txHash = data.metaTx.txHash
    if success {
      @warning("-4")
      switch currentState {
      | SignedAndSubmitted(_txHash) => ()
      | _ => setTxState(_ => SignedAndSubmitted(txHash))
      }

      let providerUrl = switch networkId {
      | Some(1) => "https://rpc-mainnet.matic.network"
      | Some(4) => "https://goerli.infura.io/v3/c401b8ee3a324619a453f2b5b2122d7a"
      | Some(5) => "https://rpc-mumbai.matic.today"
      | Some(_)
      | None => "No Network"
      }
      let maticProvider = Ethers.makeProvider(providerUrl)

      let waitForTx = maticProvider->Ethers.waitForTransaction(txHash)->Promise.Js.toResult

      waitForTx->Promise.getOk(tx => setTxState(_ => Complete(tx)))
      waitForTx->Promise.getError(error => {
        setTxState(_ => Failed)
        Js.log("GOT AN ERROR")
        Js.log(error)
      })
    } else {
      @warning("-4")
      switch currentState {
      | ServerError(_txHash) => ()
      | _ => setTxState(_ => ServerError(errorMsg->Option.getWithDefault("Unknown error")))
      }
    }
  | {data: None, _}
  | {error: _, _} =>
    @warning("-4")
    switch currentState {
    | ServerError(_txHash) => ()
    | _ => setTxState(_ => ServerError("Query Error"))
    }
  }
}
let useBuy = (
  ~chain,
  animal: TokenId.t,
  library: option<Web3.web3Library>,
  account,
  parentChainId,
) => {
  let animalId = animal->TokenId.toString

  let optSteward = useStewardContract()
  let (txState, setTxState) = React.useState(() => UnInitialised)

  let (mutate, result) = ExecuteMetaTxMutation.use()
  handleMetaTxSumbissionState(result, setTxState, txState)

  let chainIdInt = parentChainId->getChildChainId
  let chainId = chainIdInt->BN.newInt_
  let verifyingContract = getDaiContractAddress(chain, chainIdInt)
  let spender = getStewardAddress(chain, chainIdInt)
  let networkName = chainIdInt->getMaticNetworkName

  let maticState =
    account
    ->Option.getWithDefault(CONSTANTS.nullEthAddress)
    ->QlHooks.useMaticState(~forceRefetch=false, networkName)

  switch chain {
  | Client.Neither
  | Client.MaticQuery => (
      (newPrice, oldPrice, wildcardsPercentage, value: string) =>
        switch (library, account, maticState) {
        // TODO: This function should not take in options of these values, they should be defined
        | (Some(lib), Some(userAddress), Some(maticState)) =>
          let daiNonce = maticState.daiNonce
          let stewardNonce = maticState.stewardNonce

          setTxState(_ => DaiPermit(value->BN.new_))
          execDaiPermitMetaTx(
            daiNonce,
            networkName,
            stewardNonce,
            setTxState,
            mutate,
            userAddress,
            spender,
            lib,
            (steward, v, r, s) =>
              (
                steward->Web3.Contract.MaticSteward.buyWithPermit(
                  BN.new_(daiNonce),
                  BN.new_("0"),
                  true,
                  v,
                  r,
                  s,
                  animalId,
                  newPrice->Web3Utils.toWeiFromEth,
                  oldPrice,
                  wildcardsPercentage,
                  value,
                )
              ).encodeABI(),
            chainId,
            verifyingContract,
          )
        | _ => ()
        },
      txState,
    )
  | Client.MainnetQuery => (
      (newPrice, oldPrice, wildcardsPercentage, value: string) => {
        let newPriceEncoded = parseUnits(. newPrice, 18)

        let value = parseUnits(. value, 0)
        let oldPriceParsed = parseUnits(. oldPrice, 0)

        setTxState(_ => Created)
        switch optSteward {
        | Some(steward) =>
          let buyPromise = steward.buy(.
            animalId,
            newPriceEncoded,
            oldPriceParsed,
            wildcardsPercentage,
            {
              gasLimit: "500302", //calculateGasMargin(estimatedGasLimit, GAS_MARGIN),
              value: value,
            },
          )->Promise.Js.toResult
          buyPromise->Promise.getOk(tx => {
            setTxState(_ => SignedAndSubmitted(tx.hash))
            let txMinedPromise = tx.wait(.)->Promise.Js.toResult
            txMinedPromise->Promise.getOk(txOutcome => {
              Js.log(txOutcome)
              setTxState(_ => Complete(txOutcome))
            })
            txMinedPromise->Promise.getError(error => {
              setTxState(_ => Failed)
              Js.log(error)
            })
            ()
          })
          buyPromise->Promise.getError(error => setTxState(_ => Declined(error.message)))
          ()
        | None => ()
        }
      },
      txState,
    )
  }
}

let useBuyAuction = (~chain, animal, library: option<Web3.web3Library>, account, parentChainId) => {
  let (txState, setTxState) = React.useState(() => UnInitialised)

  let animalId = animal->TokenId.toString

  let optSteward = useStewardContract()

  let (mutate, result) = ExecuteMetaTxMutation.use()
  handleMetaTxSumbissionState(result, setTxState, txState)

  let chainIdInt = parentChainId->getChildChainId
  let chainId = chainIdInt->BN.newInt_
  let verifyingContract = getDaiContractAddress(chain, chainIdInt)
  let spender = getStewardAddress(chain, chainIdInt)
  let networkName = chainIdInt->getMaticNetworkName

  let maticState =
    account
    ->Option.getWithDefault(CONSTANTS.nullEthAddress)
    ->QlHooks.useMaticState(~forceRefetch=false, networkName)

  switch chain {
  | Client.Neither
  | Client.MaticQuery => (
      (newPrice, wildcardsPercentage, value: string) =>
        switch (library, account, maticState) {
        | (Some(lib), Some(userAddress), Some(maticState)) =>
          let daiNonce = maticState.daiNonce
          let stewardNonce = maticState.stewardNonce
          setTxState(_ => DaiPermit(value->BN.new_))
          execDaiPermitMetaTx(
            daiNonce,
            networkName,
            stewardNonce,
            setTxState,
            mutate,
            userAddress,
            spender,
            lib,
            (steward, v, r, s) =>
              (
                steward->Web3.Contract.MaticSteward.buyAuctionWithPermit(
                  BN.new_(daiNonce),
                  BN.new_("0"),
                  true,
                  v,
                  r,
                  s,
                  animalId,
                  newPrice->Web3Utils.toWeiFromEth,
                  wildcardsPercentage,
                  value,
                )
              ).encodeABI(),
            chainId,
            verifyingContract,
          )

        | _ =>
          Js.log("something important is null")
          Js.log3(library, account, maticState)
          ()
        },
      txState,
    )
  | Client.MainnetQuery => (
      (newPrice, wildcardsPercentage, value: string) => {
        let newPriceEncoded = parseUnits(. newPrice, 18)

        let value = parseUnits(. value, 0)

        setTxState(_ => Created)
        switch optSteward {
        | Some(steward) =>
          // let buyPromise =
          let buyPromise = steward.buyAuction(.
            animalId,
            newPriceEncoded,
            wildcardsPercentage,
            {
              gasLimit: "500302", //calculateGasMargin(estimatedGasLimit, GAS_MARGIN),
              value: value,
            },
          )->Promise.Js.toResult
          buyPromise->Promise.getOk(tx => {
            setTxState(_ => SignedAndSubmitted(tx.hash))
            let txMinedPromise = tx.wait(.)->Promise.Js.toResult
            txMinedPromise->Promise.getOk(txOutcome => {
              Js.log(txOutcome)
              setTxState(_ => Complete(txOutcome))
            })
            txMinedPromise->Promise.getError(error => {
              setTxState(_ => Failed)
              Js.log(error)
            })
            ()
          })
          buyPromise->Promise.getError(error => setTxState(_ => Declined(error.message)))
          ()
        | None => ()
        }
      },
      txState,
    )
  }
}

let useRedeemLoyaltyTokens = (patron: string) => {
  let (txState, setTxState) = React.useState(() => UnInitialised)
  let optSteward = useStewardContract()
  let buyFunction = () => {
    let value = parseUnits(. "0", 0)

    setTxState(_ => Created)
    switch optSteward {
    | Some(steward) =>
      let claimLoyaltyTokenPromise = steward._collectPatronagePatron(.
        patron,
        {
          gasLimit: "500302", //calculateGasMargin(estimatedGasLimit, GAS_MARGIN),
          value: value,
        },
      )->Promise.Js.toResult
      claimLoyaltyTokenPromise->Promise.getOk(tx => {
        setTxState(_ => SignedAndSubmitted(tx.hash))
        let txMinedPromise = tx.wait(.)->Promise.Js.toResult
        txMinedPromise->Promise.getOk(txOutcome => {
          Js.log(txOutcome)
          setTxState(_ => Complete(txOutcome))
        })
        txMinedPromise->Promise.getError(error => {
          setTxState(_ => Failed)
          Js.log(error)
        })
        ()
      })
      claimLoyaltyTokenPromise->Promise.getError(error => setTxState(_ => Declined(error.message)))
      ()
    | None => ()
    }
  }

  (buyFunction, txState)
}

let useUpdateDeposit = (~chain, library: option<Web3.web3Library>, account, parentChainId) => {
  let (txState, setTxState) = React.useState(() => UnInitialised)

  let optSteward = useStewardContract()

  let (mutate, result) = ExecuteMetaTxMutation.use()
  handleMetaTxSumbissionState(result, setTxState, txState)

  let chainIdInt = parentChainId->getChildChainId
  let chainId = chainIdInt->BN.newInt_
  let verifyingContract = getDaiContractAddress(chain, chainIdInt)
  let spender = getStewardAddress(chain, chainIdInt)
  let networkName = chainIdInt->getMaticNetworkName

  let maticState =
    account
    ->Option.getWithDefault(CONSTANTS.nullEthAddress)
    ->QlHooks.useMaticState(~forceRefetch=false, networkName)

  switch chain {
  | Client.Neither
  | Client.MaticQuery => (
      amountToAdd =>
        switch (library, account, maticState) {
        | (Some(lib), Some(userAddress), Some(maticState)) =>
          let daiNonce = maticState.daiNonce
          let stewardNonce = maticState.stewardNonce
          setTxState(_ => DaiPermit(amountToAdd->BN.new_))
          execDaiPermitMetaTx(
            daiNonce,
            networkName,
            stewardNonce,
            setTxState,
            mutate,
            userAddress,
            spender,
            lib,
            (steward, v, r, s) =>
              (
                steward->Web3.Contract.MaticSteward.depositWithPermit(
                  BN.new_(daiNonce),
                  BN.new_("0"),
                  true,
                  v,
                  r,
                  s,
                  userAddress,
                  BN.new_(amountToAdd),
                )
              ).encodeABI(),
            chainId,
            verifyingContract,
          )

        | _ =>
          Js.log("something important is null")
          Js.log3(library, account, maticState)
          ()
        },
      txState,
    )
  | Client.MainnetQuery => (
      (value: string) => {
        let value = parseUnits(. value, 0)

        setTxState(_ => Created)
        switch optSteward {
        | Some(steward) =>
          let updateDepositPromise = steward.depositWei(. {
            gasLimit: "500302", //calculateGasMargin(estimatedGasLimit, GAS_MARGIN),
            value: value,
          })->Promise.Js.toResult
          updateDepositPromise->Promise.getOk(tx => {
            setTxState(_ => SignedAndSubmitted(tx.hash))
            let txMinedPromise = tx.wait(.)->Promise.Js.toResult
            txMinedPromise->Promise.getOk(txOutcome => setTxState(_ => Complete(txOutcome)))
            txMinedPromise->Promise.getError(_error => setTxState(_ => Failed))
            ()
          })
          updateDepositPromise->Promise.getError(error =>
            Js.log("error processing transaction: " ++ error.message)
          )
          ()
        | None => ()
        }
      },
      txState,
    )
  }
}

let useWithdrawDeposit = (~chain, library: option<Web3.web3Library>, account, parentChainId) => {
  let (txState, setTxState) = React.useState(() => UnInitialised)

  let optSteward = useStewardContract()

  let chainIdInt = parentChainId->getChildChainId
  let spender = getStewardAddress(chain, chainIdInt)

  switch chain {
  | Client.Neither
  | Client.MaticQuery => (
      amountToWithdraw =>
        switch (library, account) {
        | (Some(lib), Some(account)) =>
          {
            let web3 = Web3.new_(lib.provider)

            let steward = Web3.Contract.MaticSteward.getStewardContract(web3, spender)

            (steward->Web3.Contract.MaticSteward.withdrawDeposit(amountToWithdraw)).send({
              from: account,
            })
          }
          ->Js.Promise.catch(err => {
            Js.log2("this error was caught", err)
            Js.Promise.resolve(""->Obj.magic)
          }, _)
          ->ignore
        | _ => ()
        },
      txState,
    )
  | Client.MainnetQuery => (
      amountToWithdraw => {
        let value = parseUnits(. "0", 0)
        let amountToWithdrawEncoded = parseUnits(. amountToWithdraw, 0)

        setTxState(_ => Created)
        switch optSteward {
        | Some(steward) =>
          let updateDepositPromise = steward.withdrawDeposit(.
            amountToWithdrawEncoded,
            {
              gasLimit: "500302", //calculateGasMargin(estimatedGasLimit, GAS_MARGIN),
              value: value,
            },
          )->Promise.Js.toResult
          updateDepositPromise->Promise.getOk(tx => {
            setTxState(_ => SignedAndSubmitted(tx.hash))
            let txMinedPromise = tx.wait(.)->Promise.Js.toResult
            txMinedPromise->Promise.getOk(txOutcome => {
              Js.log(txOutcome)
              setTxState(_ => Complete(txOutcome))
            })
            txMinedPromise->Promise.getError(error => {
              setTxState(_ => Failed)
              Js.log(error)
            })
            ()
          })
          updateDepositPromise->Promise.getError(error => setTxState(_ => Declined(error.message)))
          ()
        | None => ()
        }
      },
      txState,
    )
  }
}

let useUserLoyaltyTokenBalance = (address: Web3.ethAddress) => {
  let (result, setResult) = React.useState(() => None)
  let (counter, setCounter) = React.useState(() => 0)

  let optSteward = useLoyaltyTokenContract()

  React.useEffect4(() => {
    switch optSteward {
    | Some(steward) =>
      let _ = %Async({
        let balance = steward.balanceOf(. address)
        let balanceString = balance->ethersBnToString
        setResult(_ => Some(BN.new_(balanceString)))
        ()->async
      })
    | None => ()
    }
    None
  }, (counter, setResult, optSteward, address))

  (result, () => setCounter(_ => counter + 1))
}

let useChangePrice = animal => {
  let animalId = TokenId.toString(animal)
  let (txState, setTxState) = React.useState(() => UnInitialised)

  let optSteward = useStewardContract()

  (
    newPrice => {
      let value = parseUnits(. "0", 0)
      let newPriceEncoded = parseUnits(. newPrice, 0)

      setTxState(_ => Created)
      switch optSteward {
      | Some(steward) =>
        let updatePricePromise = steward.changePrice(.
          animalId,
          newPriceEncoded,
          {
            gasLimit: "500302", //calculateGasMargin(estimatedGasLimit, GAS_MARGIN),
            value: value,
          },
        )->Promise.Js.toResult
        updatePricePromise->Promise.getOk(tx => {
          setTxState(_ => SignedAndSubmitted(tx.hash))
          let txMinedPromise = tx.wait(.)->Promise.Js.toResult
          txMinedPromise->Promise.getOk(txOutcome => {
            Js.log(txOutcome)
            setTxState(_ => Complete(txOutcome))
          })
          txMinedPromise->Promise.getError(error => {
            setTxState(_ => Failed)
            Js.log(error)
          })
          ()
        })
        updatePricePromise->Promise.getError(error => setTxState(_ => Declined(error.message)))
        ()
      | None => ()
      }
    },
    txState,
  )
}
