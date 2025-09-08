open Globals

let dragonImg = "/img/animals/Glen.svg"
let refreshImg = "/img/icons/refresh.svg"

let wildTomorrowFundImg = "/img/conservation-partners/wild-tomorrow-fund.png"

let laSendaVerdeImg = "/img/conservation-partners/senda-verde.png"

let greatWhaleConservancyImg = "/img/conservation-partners/great-whale-conservancy-small.png"

let darwinAnimalDoctorsImg = "/img/conservation-partners/darwin-animal-doctors.svg"

type conservationPartnerType = {
  name: string,
  image: string,
  link: string,
  index: int,
}

let conservationPartners: array<conservationPartnerType> = [
  {
    name: "Wild Tomorrow Fund",
    image: wildTomorrowFundImg,
    link: "https://www.wildtomorrowfund.org/",
    index: 1,
  },
  {
    name: "La Senda Verde",
    image: laSendaVerdeImg,
    link: "http://www.sendaverde.org/",
    index: 2,
  },
  {
    name: "The Great Whale Conservancy",
    image: greatWhaleConservancyImg,
    link: "http://greatwhaleconservancy.org/",
    index: 3,
  },
  {
    name: "Darwin Animal Doctors",
    image: darwinAnimalDoctorsImg,
    link: "http://darwinanimaldoctors.org/",
    index: 4,
  },
]

type organisationArrayIndex = int

type voteStep =
  | DefaultView // sub-states can either be loading data, ready, or user is not eligible to vote
  | SelectedOrganisationToVote(organisationArrayIndex, float => unit)
  | // | SelectedOrganisationToVote(organisationArrayIndex, currentVote)
  ProcessTransaction
  | ViewResults

module HackyComponentThatCallsAFunctionOnce = {
  @react.component
  let make = (~reloadFunction) => {
    let (hasCalledFunction, setHasCalledFunction) = React.useState(_ => false)
    if !hasCalledFunction {
      reloadFunction()
      setHasCalledFunction(_ => true)
    } else {
      ()
    }

    React.null
  }
}

// TODO: this shouldn't be done using a component, it should be done with a 'useEffect'
module HackyComponentThatReloadsOnTimeout = {
  @react.component
  let make = (~reloadFunction: unit => unit, ~timeoutTime) => {
    let (hasCalledFunction, setHasCalledFunction) = React.useState(_ => false)

    React.useEffect4(() => {
      let timeout = Js.Global.setTimeout(() =>
        if !hasCalledFunction {
          reloadFunction()
          setHasCalledFunction(_ => true)
        } else {
          ()
        }
      , timeoutTime)
      Some(() => Js.Global.clearTimeout(timeout))
    }, (reloadFunction, hasCalledFunction, setHasCalledFunction, timeoutTime))

    React.null
  }
}

module OrganisationVote = {
  @react.component
  let make = (~conservationPartner, ~selectConservation, ~index) =>
    <Rimble.Box width=[1., 0.25]>
      <a href=conservationPartner.link target="_blank">
        <img
          className={
            open Css
            style(list{display(#block), width(#percent(70.)), maxWidth(#px(800)), margin(auto)})
          }
          src=conservationPartner.image
        />
      </a>
      <Rimble.Button
        className={
          open Css
          style(list{display(#block), margin(auto), width(#percent(90.))})
        }
        disabled=true
        onClick={_ => selectConservation(index)}>
        {"Voting Disabled"->restr}
      </Rimble.Button>
    </Rimble.Box>
}

@react.component
let make = (~chain) => {
  let (voteStep, setVoteStep) = React.useState(() => DefaultView)

  let selectConservation = conservationArrayIndex => {
    let submitVoteFunction: float => unit = _ => setVoteStep(_ => ProcessTransaction)

    setVoteStep(_ => SelectedOrganisationToVote(conservationArrayIndex, submitVoteFunction))
  }

  let resetVoting = () => setVoteStep(_ => DefaultView)

  let glen = TokenId.makeFromInt(13)
  let optCurrentPrice = PriceDisplay.usePrice(~chain, glen)
  let (_, _, ratio, _) = QlHooks.usePledgeRateDetailed(~chain, glen)
  // TODO: investigate why the USD price doesn't load here.
  let (optMonthlyPledgeEth, optMonthlyPledgeUsd) = switch optCurrentPrice {
  | Some((priceEth, optPriceUsd)) => (
      Some(
        toFixedWithPrecisionNoTrailingZeros(
          Float.fromString(priceEth)->Accounting.defaultZeroF *. ratio,
          ~digits=5,
        ),
      ),
      switch optPriceUsd {
      | Some(_priceUsd) => None
      | None => None
      },
    )
  | None => (None, None)
  }

  <Rimble.Box className=Styles.topBody>
    <Rimble.Box>
      <Rimble.Flex flexWrap="wrap">
        <Rimble.Box width=[1., 1., 0.3]>
          <img
            className={
              open Css
              style(list{maxWidth(#px(800)), margin(auto)})
            }
            src=dragonImg
          />
          <a href="/#details/13">
            <h3
              className={
                open Css
                style(list{textAlign(#center)})
              }>
              {"Glen the Dragon from Turin"->restr}
            </h3>
          </a>
          <p
            className={
              open Css
              style(list{textAlign(#center)})
            }>
            {"Monthly contribution: "->restr}
            {switch optMonthlyPledgeEth {
            | Some(monthlyPledgeEth) => (monthlyPledgeEth ++ " ETH")->restr
            | None =>
              <Rimble.Loader
                className={
                  open Css
                  style(list{margin(auto)})
                }
              />
            }}
            <br />
            <small>
              {switch optMonthlyPledgeUsd {
              | Some(monthlyPledgeUsd) => ("(" ++ (monthlyPledgeUsd ++ " USD)"))->restr
              | None => React.null
              }}
            </small>
          </p>
          <br />
          <br />
          <br />
          <h4
            className={
              open Css
              style(list{width(#percent(100.)), textAlign(center)})
            }>
            {"Voting coming again soon!"->restr}
          </h4>
        </Rimble.Box>
        <Rimble.Box width=[1., 1., 0.7]>
          <h3
            className={
              open Css
              style(list{textDecoration(#underline)})
            }>
            {"How it works"->restr}
          </h3>
          <p>
            {"Glen is a special Wildcard, this mystical creature is not tied to a specific conservation but rather each month the owners of Wildcards vote for a conservation they think should receive the funds raised by Glen."->restr}
          </p>
          <p>
            {"The voting mechanism uses quadratic voting. Wildcards owners vote using Wildcards Loyalty tokens which they earn from holding a Wildcard. Quadratic voting means that the number of loyalty tokens don't represent the exact number of votes but rather the number of loyalty tokens is square rooted to represent the number of votes."->restr}
          </p>
          <h3
            className={
              open Css
              style(list{textDecoration(#underline)})
            }>
            {"Quadratic Voting    "->restr}
            {voteStep != DefaultView
              ? <img
                  onClick={_ => resetVoting()}
                  src=refreshImg
                  className={
                    open Css
                    style(list{maxHeight(#px(16)), paddingLeft(#rem(1.))})
                  }
                />
              : React.null}
          </h3>
          <small>
            <p>
              {"Unfortunately we have decided to stop running our DAO on mainnet ethereum. We are moving all of this code to Matic where voting will be much cheaper and more frictionless"->restr}
            </p>
          </small>
          <Rimble.Flex flexWrap="wrap" alignItems="center">
            {conservationPartners
            ->Array.mapWithIndex((index, conservationPartner) =>
              <OrganisationVote
                conservationPartner
                index
                selectConservation
                key=//  currentIteration=1
                conservationPartner.name
              />
            )
            ->React.array}
          </Rimble.Flex>
        </Rimble.Box>
      </Rimble.Flex>
    </Rimble.Box>
  </Rimble.Box>
}
