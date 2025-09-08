open Globals

@react.component
let make = () =>
  <Rimble.Box className=Styles.horizantalBlueTile>
    <p className=Styles.explainerMediumText>
      <span className=Styles.boldExplainerText> {"Wildcards"->restr} </span>
      {" is an idea born out of the "->restr}
      <a href="https://ethcapetown.com/"> {"#ETHCapeTown2019"->restr} </a>
      {" hackathon which saw team Wildcards as overall "->restr}
      <a href="https://devpost.com/software/ethcapetown_wildcards"> {"winners"->restr} </a>
      {". The focus of the project is raising funds for at risk animals."->restr}
    </p>
  </Rimble.Box>
