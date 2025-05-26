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

// type reasonUneligeableToVote =
//   | DontOwnAWildcard
//   | EligeableToVote;
type organisationArrayIndex = int
// type currentVote = {
//   vote: BN.t,
//   maxPossibleVote: BN.t // This should be calculated at the beginning
// };
type voteStep =
  | DefaultView // sub-states can either be loading data, ready, or user is not eligible to vote
  | SelectedOrganisationToVote(organisationArrayIndex, float => unit)
  | // | SelectedOrganisationToVote(organisationArrayIndex, currentVote)
  ProcessTransaction
  | ViewResults

// TODO: this shouldn't be done using a component, it should be done with a 'useEffect'
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
  let make = (
    ~currentIteration,
    ~conservationPartner,
    ~cannotVote,
    ~selectConservation,
    ~index,
    ~currentUser,
  ) => {
    // TODO: add a reload on timeout if this doesn't load
    let (
      hasVotedForProposal1Votes,
      _reloadHasVoted,
    ) = ContractActions.useHasUserVotedForProposalIteration(
      currentIteration->string_of_int,
      currentUser,
      conservationPartner.index->string_of_int,
    )

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
        disabled={\"||||"(hasVotedForProposal1Votes, true) || cannotVote}
        onClick={_ => selectConservation(index)}>
        {\"||||"(
          \">>="(hasVotedForProposal1Votes, hasVoted =>
            hasVoted ? Some("Cannot Vote Twice") : None
          ),
          "Vote",
        )->restr}
      </Rimble.Button>
    </Rimble.Box>
  }
}
module OrganisationVoteResult = {
  @react.component
  let make = (~conservationPartner, ~currentIteration, ~totalVotes) => {
    // TODO: add a reload on timeout (incase this value doesn't load first time)
    let (proposal1Votes, _reload) = ContractActions.useProposalVotes(
      currentIteration,
      conservationPartner.index->string_of_int,
    )

    let displayText = switch proposal1Votes {
    | Some(proposalVotes) =>
      ((\"||||"(
        \"|/|"(\"|*|"(proposalVotes, BN.new_("10000")), totalVotes)->BN.toString->Float.fromString,
        0.,
      ) /. 100.)->Float.toString ++ "%")->restr
    | None => <Rimble.Loader />
    }
    let numberOfVotes = switch proposal1Votes {
    | Some(proposalVotes) =>
      "with " ++
      (\"|*|"(proposalVotes, BN.new_("1000000000"))->Web3Utils.fromWeiBNToEthPrecision(~digits=2) ++
      " votes in total")
    | None => ""
    }

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
      <p> displayText </p>
      <p> {numberOfVotes->restr} </p>
    </Rimble.Box>
  }
}

module VoteResults = {
  @react.component
  let make = (~currentIteration) => {
    // TODO: add a reload on timeout (incase this value doesn't load first time)
    let (totalVotes, _reload) = ContractActions.useTotalVotes()
    let currentIteration = currentIteration->string_of_int
    let (currentWinner, _reload) = ContractActions.useCurrentWinner()

    <>
      {switch totalVotes {
      | Some(totalVotes) =>
        <Rimble.Flex flexWrap="wrap" alignItems="center">
          {conservationPartners
          ->Array.map(conservationPartner =>
            <OrganisationVoteResult
              key=conservationPartner.name currentIteration totalVotes conservationPartner
            />
          )
          ->React.array}
        </Rimble.Flex>
      | None => <p> <Rimble.Loader /> {"loading current standings"->restr} </p>
      }}
      {switch currentWinner {
      | Some(currentWinner) =>
        <p
          className={
            open Css
            style(list{fontSize(em(2.))})
          }>
          <strong>
            {\"||||"(
              \"<$>"(conservationPartners[currentWinner - 1], partner => partner.name),
              "Unknown",
            )->restr}
          </strong>
          {" is currently winning"->restr}
        </p>
      //  <h2> {currentWinner->string_of_int->restr} </h2>
      | None => React.null
      }}
    </>
  }
}

module ApproveLoyaltyTokens = {
  @react.component
  let make = (~reloadFunction) => {
    let (approveLoyaltyTokens, transactionStatus) = ContractActions.useApproveLoyaltyTokens()
    let etherScanUrl = RootProvider.useEtherscanUrl()

    <div>
      {switch transactionStatus {
      | UnInitialised => <>
          <Rimble.Loader />
          <p>
            <a onClick={_ => approveLoyaltyTokens()}>
              {">>Click here to enable wildcards vote with your tokens<<"->restr}
            </a>
          </p>
        </>
      | DaiPermit(_)
      | SignMetaTx
      | ServerError(_)
      | SubmittedMetaTx
      | Created => <>
          <Rimble.Loader />
          <p> {"Transaction Created - please check the details and confirm."->restr} </p>
        </>
      | SignedAndSubmitted(txHash) => <>
          <Rimble.Loader />
          <p>
            {"Processing: "->restr}
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={"https://" ++ (etherScanUrl ++ ("/tx/" ++ txHash))}>
              {"view transaction"->restr}
            </a>
          </p>
        </>
      | Declined(message) => <p> {("Submitting transaction failed: " ++ message)->restr} </p>
      | Complete(_txResult) =>
        <p>
          <HackyComponentThatCallsAFunctionOnce reloadFunction />
          {"You are now ready to vote :)"->restr}
        </p>
      | Failed => <p> {"Transaction failed"->restr} </p>
      }}
    </div>
  }
}

@react.component
let make = (~chain) => {
  let (voteStep, setVoteStep) = React.useState(() => DefaultView)
  // let (voteStep, setVoteStep) = React.useState(() => ViewResults);

  let (voteForProject, transactionStatus) = ContractActions.useVoteForProject()
  let selectConservation = conservationArrayIndex => {
    let submitVoteFunction: float => unit = votes => {
      let conservationVotedContractIndex = (
        conservationPartners->Array.getUnsafe(conservationArrayIndex)
      ).index

      // NOTE: if they vote with more than about 300000 there will be an overflow here...
      let numberOfVotes =
        (votes *. 100000.)->int_of_float->Int.toString->BN.new_->BN.mul(BN.new_("10000"))

      voteForProject(conservationVotedContractIndex->string_of_int, numberOfVotes)

      setVoteStep(_ => ProcessTransaction)
    }

    setVoteStep(_ => SelectedOrganisationToVote(conservationArrayIndex, submitVoteFunction))
  }

  let resetVoting = () => setVoteStep(_ => DefaultView)

  let networkIdOpt = RootProvider.useNetworkId()

  let currentUser = RootProvider.useCurrentUser()

  let userAddressLowerCase = switch currentUser {
  | Some(currentUser) => currentUser->Js.String.toLowerCase
  | _ => CONSTANTS.nullEthAddress
  }

  let patronQueryOpt = QlHooks.usePatronQuery(~chain=Client.MainnetQuery, userAddressLowerCase)

  let (optProposalDeadline, _reloadProposalDeadline) = ContractActions.useProposalDeadline()

  let currentlyOwnedTokens = switch patronQueryOpt {
  | Some(patronQueryResult) =>
    \"||||"(
      patronQueryResult["patron"]->oMap(patron =>
        patron["tokens"]->Array.map(token => token["id"])
      ),
      [],
    )
  | None => []
  }

  let isProviderSelucted = RootProvider.useIsProviderSelected()

  let (cannotVote, incorrectNetworkWarning) = {
    let cannotVote: bool = currentlyOwnedTokens->Array.length <= 0

    switch isProviderSelucted {
    | false => (
        false,
        <>
          <h4
            className={
              open Css
              style(list{color(red)})
            }>
            {"You need to be logged in to vote"->restr}
          </h4>
        </>,
      )
    | true =>
      switch networkIdOpt {
      | Some(5) => (cannotVote, React.null)
      | Some(1) =>
        let launchTime = MomentRe.momentUtcDefaultFormat("2020-05-08T08:00:00")
        let isBeforeDate = MomentRe.diff(launchTime, MomentRe.momentNow(), #seconds) > 0.
        isBeforeDate
          ? (
              true,
              <>
                <h4
                  className={
                    open Css
                    style(list{color(red)})
                  }>
                  {"Voting is currently only available on the Goerli testnet right."->restr}
                </h4>
                <h2
                  className={
                    open Css
                    style(list{color(red)})
                  }>
                  {"Launching in: "->restr} <CountDown endDateMoment=launchTime />
                </h2>
              </>,
            )
          : (cannotVote, React.null)
      | _ => (
          true,
          <>
            <h4
              className={
                open Css
                style(list{color(red)})
              }>
              {"We only support mainnet and goerli testnet."->restr}
            </h4>
          </>,
        )
      }
    }
  }

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

  let etherScanUrl = RootProvider.useEtherscanUrl()

  let (
    redeemedLoyaltyTokenBalanceBn,
    resetLoyaltyTokenBalance,
  ) = ContractActions.useUserLoyaltyTokenBalance(userAddressLowerCase)
  let (optCurrentIteration, reloadCurrentIteration) = ContractActions.useCurrentIteration()

  let redeemedLoyaltyTokenBalanceOpt =
    redeemedLoyaltyTokenBalanceBn->oFlatMap(balance =>
      balance->Web3Utils.fromWeiBNToEthPrecision(~digits=3)->Float.fromString
    )

  let txStateDisplay =
    <div>
      {switch transactionStatus {
      | UnInitialised => <> <Rimble.Loader /> <p> {"Starting Transaction"->restr} </p> </>
      | DaiPermit(_)
      | SignMetaTx
      | ServerError(_)
      | SubmittedMetaTx
      | Created => <> <Rimble.Loader /> <p> {"Transaction Created"->restr} </p> </>
      | SignedAndSubmitted(txHash) => <>
          <Rimble.Loader />
          <p>
            {"Processing: "->restr}
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={"https://" ++ (etherScanUrl ++ ("/tx/" ++ txHash))}>
              {"view transaction"->restr}
            </a>
          </p>
        </>
      | Declined(message) => <p> {("Submitting transaction failed: " ++ message)->restr} </p>
      | Complete(_txResult) => <>
          <HackyComponentThatCallsAFunctionOnce reloadFunction=resetLoyaltyTokenBalance />
          <p> {"Congratulations for voting"->restr} </p>
          <Rimble.Button
            className={
              open Css
              style(list{display(#block), margin(auto), width(#percent(90.))})
            }
            onClick={_ => setVoteStep(_ => ViewResults)}>
            {"Reveal Rankings"->restr}
          </Rimble.Button>
        </>
      | Failed => <p> {"Transaction failed"->restr} </p>
      }}
    </div>

  let (amountApproved, resetAmountApproved) = ContractActions.useVoteApprovedTokens(
    userAddressLowerCase,
  )
  // let hasApprovedFullBalance =
  //   amountApproved
  //   |||| BN.new_("0")
  //   ->| (
  //     redeemedLoyaltyTokenBalanceBn |||| BN.new_("10000000000000000000000")
  //   );

  // TODO: This gets the value from the graph rather - use this in the future rather than querying the chain.
  // let totalLoyaltyTokensOpt =
  //   QlHooks.useTotalLoyaltyToken(userAddressLowerCase);
  // let redeemedLoyaltyTokenBalance: float =
  //   totalLoyaltyTokensOpt->oFlatMap(((_, claimedLoyaltyTokens)) =>
  //     claimedLoyaltyTokens
  //     ->Web3Utils.fromWeiBNToEthPrecision(~digits=3)
  //     ->Float.fromString
  //   )
  //   |||| 0.;

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
          {switch optProposalDeadline {
          | Some(proposalDeadline) =>
            let proposalDeadlineMoment = proposalDeadline->MomentRe.momentWithUnix
            let isBeforeDate =
              MomentRe.diff(proposalDeadlineMoment, MomentRe.momentNow(), #seconds) > 0.
            isBeforeDate
              ? <>
                  <h4> {"Winner will be paid out in:"->restr} </h4>
                  <CountDown endDateMoment=proposalDeadlineMoment />
                </>
              : <h4>
                  {"This voting cycle is over, the winner will be paid out shortly."->restr}
                </h4> /* TODO:
                          this should say who the winner is. But in general we need think about the flow of this,
                              because technically users can still vote here.
                              Should the vote buttons be disabled in the UI?
 */

          // TODO: when the voting interval is over, it should re-request every few seconds to update the page if it does come back.
          | None => React.null
          }}
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
            {currentlyOwnedTokens->Array.length <= 0
              ? <p
                  className={
                    open Css
                    style(list{color(red)})
                  }>
                  {"You can only vote if you are the owner of a wildcard"->restr}
                </p>
              : switch redeemedLoyaltyTokenBalanceOpt {
                | Some(redeemedLoyaltyTokenBalance) =>
                  <p>
                    {("Redeemed loyalty token balance: " ++
                    (redeemedLoyaltyTokenBalance->Float.toString ++
                    " WLT"))->restr}
                  </p>
                | None => <Rimble.Loader />
                }}
            incorrectNetworkWarning
          </small>
          <Rimble.Flex flexWrap="wrap" alignItems="center">
            {switch voteStep {
            | DefaultView =>
              switch optCurrentIteration {
              | Some(currentIteration) =>
                conservationPartners
                ->Array.mapWithIndex((index, conservationPartner) =>
                  <OrganisationVote
                    currentUser=userAddressLowerCase
                    currentIteration
                    conservationPartner
                    cannotVote
                    index
                    selectConservation
                    key=conservationPartner.name
                  />
                )
                ->React.array
              | None => <>
                  <HackyComponentThatReloadsOnTimeout
                  // If the loyalty token balance or the amountApproved are still not loaded after 1 second, attempt to reload them.
                    reloadFunction=reloadCurrentIteration timeoutTime=1000
                  />
                  <Rimble.Loader />
                  <p> {"loading vote data"->restr} </p>
                </>
              }
            | SelectedOrganisationToVote(conservationVotedArrayIndex, submitVoteFunction) =>
              let selectedConservationPartner =
                conservationPartners->Array.getUnsafe(conservationVotedArrayIndex)
              <>
                <Rimble.Box width=[1., 0.25]>
                  <a href=selectedConservationPartner.link target="_blank">
                    <img
                      className={
                        open Css
                        style(list{
                          display(#block),
                          width(#percent(70.)),
                          maxWidth(#px(800)),
                          margin(auto),
                        })
                      }
                      src=selectedConservationPartner.image
                    />
                  </a>
                </Rimble.Box>
                <Rimble.Box width=[1., 0.75]>
                  {switch (amountApproved, redeemedLoyaltyTokenBalanceBn) {
                  | (Some(amountApproved), Some(redeemedLoyaltyTokenBalanceBn)) =>
                    let hasApprovedFullBalance = \"|>|"(
                      amountApproved,
                      redeemedLoyaltyTokenBalanceBn,
                    )
                    hasApprovedFullBalance
                      ? switch redeemedLoyaltyTokenBalanceOpt {
                        | Some(redeemedLoyaltyTokenBalance) =>
                          let maxVote = Js.Math.sqrt(redeemedLoyaltyTokenBalance)
                          <QVSelect submitVoteFunction maxVote />
                        | None => <> <Rimble.Loader /> <p> {"Loading your balance"->restr} </p> </>
                        }
                      : <ApproveLoyaltyTokens reloadFunction=resetAmountApproved />
                  | _ => <>
                      <HackyComponentThatReloadsOnTimeout
                      // If the loyalty token balance or the amountApproved are still not loaded after 1 second, attempt to reload them.
                        reloadFunction={() => {
                          resetLoyaltyTokenBalance()
                          resetAmountApproved()
                        }}
                        timeoutTime=1000
                      />
                      <Rimble.Loader />
                    </>
                  }}
                </Rimble.Box>
              </>
            | ProcessTransaction => txStateDisplay
            | ViewResults =>
              switch optCurrentIteration {
              | Some(currentIteration) => <VoteResults currentIteration />
              // NOTE: it is logically impossible that this is null here since it needs to be defined to go through the previous steps.
              | None => <p> {"loading vote data"->restr} </p>
              }
            }}
          </Rimble.Flex>
        </Rimble.Box>
      </Rimble.Flex>
    </Rimble.Box>
  </Rimble.Box>
}
