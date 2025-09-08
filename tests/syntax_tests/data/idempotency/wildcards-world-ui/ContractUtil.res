let getProviderOrSigner = (library: Web3.web3Library, account: option<Web3.ethAddress>) =>
  switch account {
  | Some(account) => library.getSigner(. account)
  | None => library
  }

type arrayOfValues
type sha3Result
@send external sha3ToString: (sha3Result, string) => string = "toString"
@module("ethereumjs-abi")
external soliditySHA3: (array<string>, arrayOfValues) => sha3Result = "soliditySHA3"

type buffer
@module("ethereumjs-util")
external toBuffer: string => buffer = "toBuffer"

let constructMetaTransactionMessage: (string, string, string, string) => string = (
  nonce,
  chainId,
  functionSignature,
  contractAddress,
) =>
  "0x" ++
  soliditySHA3(
    ["uint256", "address", "uint256", "bytes"],
    (nonce, contractAddress, chainId, toBuffer(functionSignature))->Obj.magic,
  )->sha3ToString("hex")

type ethSig = {
  r: string,
  s: string,
  v: int,
}

@send external slice: (string, int, int) => string = "slice"
@val external parseInt: (string, int) => int = "parseInt"

let getEthSig = sigString => {
  r: sigString->slice(0, 66),
  s: "0x" ++ sigString->slice(66, 130),
  v: parseInt(sigString->slice(130, 132), 16),
}
