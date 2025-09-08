open Globals

@react.component
let make = () =>
  <Rimble.Box className=Styles.horizantalBlueTile>
    <p className=Styles.explainerLargeText>
      <span className=Styles.boldExplainerText> {"Wildcards"->restr} </span>
      {" connects global conservation organisations to users. "->restr}
    </p>
    <p className=Styles.explainerMediumText>
      {"Social, gamified and transparent giving makes wildcards the perfect platform to start making your difference."->restr}
    </p>
  </Rimble.Box>
