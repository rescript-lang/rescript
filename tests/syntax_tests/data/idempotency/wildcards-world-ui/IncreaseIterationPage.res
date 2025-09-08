open Globals

@react.component
let make = () => {
  let (increaseIteration, _increaseIterationTxState) = ContractActions.useIncreaseVoteIteration()
  <Rimble.Box className=Styles.topBody>
    <Rimble.Box>
      <Rimble.Button onClick={_ => increaseIteration()}>
        {"Increase iteration"->restr}
      </Rimble.Button>
    </Rimble.Box>
  </Rimble.Box>
}
