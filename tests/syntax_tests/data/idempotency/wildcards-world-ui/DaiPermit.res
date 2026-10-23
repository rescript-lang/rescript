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
  let dataString = data->Obj.magic->JSON.stringifyAny->Option.getWithDefault("")

  let exampleRpcDefinition = {
    // method: "eth_signTypedData",
    method: "eth_signTypedData_v3",
    // params: [|from, data|],
    params: [holder, dataString],
    from: holder,
  }

  Promise.make((resolve, reject) =>
    provider
    ->Web3.sendAsync(exampleRpcDefinition, (err, result) =>
      switch err->Nullable.toOption {
      | Some(err) =>
        Console.log2("There was an error", err)
        reject(err->Obj.magic)
      | None =>
        let sigString = result.result->Obj.magic

        resolve(ContractUtil.getEthSig(sigString))
      }
    )
    ->ignore)
}

type v = int
type r = string
type s = string
