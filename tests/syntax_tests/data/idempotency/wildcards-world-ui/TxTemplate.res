open Globals

@react.component
let make = (
  ~children: React.element,
  ~txState: ContractActions.transactionState,
  ~closeButtonText: string,
  ~chain,
) => {
  let etherscanUrl = RootProvider.useEtherscanUrl()
  let sidechainUrl = RootProvider.useSidechainEtherscanUrl()
  let clearNonUrlState = RootProvider.useClearNonUrlState()

  let isSideChainTx = switch chain {
  | Client.Neither
  | Client.MainnetQuery => false
  | Client.MaticQuery => true
  }
  let txExplererUrl = isSideChainTx ? sidechainUrl : etherscanUrl
  switch txState {
  | ContractActions.UnInitialised => children
  | DaiPermit(value) =>
    <React.Fragment>
      <Rimble.Heading>
        {("Please sign the message to allow use of " ++
        (value->Web3Utils.fromWeiBNToEthPrecision(~digits=2) ++
        " DAI."))->restr}
        <WildcardsLoader />
      </Rimble.Heading>
      <Rimble.Flex justifyContent="center"> <Rimble.Loader size="80px" /> </Rimble.Flex>
    </React.Fragment>
  | SignMetaTx =>
    <React.Fragment>
      <Rimble.Heading>
        {"Please sign the message to submit this transaction."->restr} <WildcardsLoader />
      </Rimble.Heading>
      <Rimble.Flex justifyContent="center"> <Rimble.Loader size="80px" /> </Rimble.Flex>
    </React.Fragment>
  | SubmittedMetaTx =>
    <React.Fragment>
      <Rimble.Heading> {"Transaction Submitted "->restr} <WildcardsLoader /> </Rimble.Heading>
      <Rimble.Text> {"Awaiting transaction details."->restr} </Rimble.Text>
      <Rimble.Flex justifyContent="center"> <Rimble.Loader size="80px" /> </Rimble.Flex>
    </React.Fragment>
  | ContractActions.Created =>
    <React.Fragment>
      <Rimble.Heading> {"Processing Transaction "->restr} <WildcardsLoader /> </Rimble.Heading>
      <Rimble.Text> {"Tx created."->restr} </Rimble.Text>
      <Rimble.Flex justifyContent="center"> <Rimble.Loader size="80px" /> </Rimble.Flex>
    </React.Fragment>
  | ContractActions.SignedAndSubmitted(txHash) =>
    <React.Fragment>
      <Rimble.Heading> {"Processing Transaction "->restr} <WildcardsLoader /> </Rimble.Heading>
      <Rimble.Text>
        <a href=j`https://$txExplererUrl/tx/$txHash` target="_blank" rel="noopener noreferrer">
          {("View the transaction on " ++ txExplererUrl)->restr}
        </a>
      </Rimble.Text>
      <Rimble.Loader className=Styles.centerItems size="80px" />
    </React.Fragment>
  | ContractActions.Complete(result) =>
    let txHash = result.transactionHash
    <React.Fragment>
      <Rimble.Heading> {"Transaction Complete "->restr} <WildcardsLoader /> </Rimble.Heading>
      <Rimble.Text>
        <a href=j`https://$txExplererUrl/tx/$txHash` target="_blank" rel="noopener noreferrer">
          {("View the transaction on " ++ txExplererUrl)->restr}
        </a>
      </Rimble.Text>
      <Rimble.Button onClick={_e => clearNonUrlState()}> {closeButtonText->restr} </Rimble.Button>
    </React.Fragment>
  | ContractActions.Declined(message) =>
    <React.Fragment>
      <Rimble.Heading>
        {"The transaction was declined by your wallet, please try again."->restr}
      </Rimble.Heading>
      <p> {("Failure reason: " ++ message)->restr} </p>
      children
    </React.Fragment>
  | ServerError(message) =>
    <React.Fragment>
      <Rimble.Heading>
        {"There was a server error when submitting your transaction."->restr}
      </Rimble.Heading>
      <p> {("Failure reason: " ++ message)->restr} </p>
      children
    </React.Fragment>
  | ContractActions.Failed =>
    <React.Fragment>
      <Rimble.Heading> {"The transaction failed."->restr} <WildcardsLoader /> </Rimble.Heading>
      <Rimble.Text>
        {"It is possible that someone else bought the token before you, or the price changed. If you are unsure please feel free to contact our support."->restr}
      </Rimble.Text>
      children
    </React.Fragment>
  }
}
