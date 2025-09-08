@send external padStart: (string, int, string) => string = "padStart"
let createPermitSig = (provider, verifyingContract, nonce, chainId, holder, spender) => {
  open Web3
  open Erc712

  let domain = {
    name: "(PoS) Dai Stablecoin",
    // name: "Dai Stablecoin",
    version: "1",
    verifyingContract: verifyingContract,
    salt: "0x" ++ chainId->BN.toStringRad(16)->padStart(64, "0"),
  }

  let message = {
    "holder": holder,
    "spender": spender,
    "nonce": nonce,
    "expiry": 0,
    // "expiry": deadline,
    "allowed": true,
  }

  let data = {
    "types": {
      "EIP712Domain": eip712Domain,
      "Permit": permit,
    },
    "domain": domain,
    "primaryType": "Permit",
    "message": message,
  }
  let dataString = data->Obj.magic->Js.Json.stringifyAny->Option.getWithDefault("")

  let exampleRpcDefinition = {
    // method: "eth_signTypedData",
    method: "eth_signTypedData_v3",
    // params: [|from, data|],
    params: [holder, dataString],
    from: holder,
  }

  Js.Promise.make((~resolve, ~reject) =>
    provider
    ->Web3.sendAsync(exampleRpcDefinition, (. err, result) =>
      switch err->Js.Nullable.toOption {
      | Some(err) =>
        Js.log2("There was an error", err)
        reject(. err->Obj.magic)
      | None =>
        let sigString = result.result->Obj.magic

        resolve(. ContractUtil.getEthSig(sigString))
      }
    )
    ->ignore
  )
}

type v = int
type r = string
type s = string
