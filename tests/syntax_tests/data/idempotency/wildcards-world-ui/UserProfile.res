open Globals

let centreAlignOnMobile = {
  open Css
  style(list{media("(max-width: 831px)", list{alignItems(center), justifyContent(center)})})
}

// TODO:: check that the address is valid:
// Something like this maybe? https://docs.ethers.io/ethers.js/html/api-utils.html
module Token = {
  @react.component
  let make = (~tokenId: TokenId.t) => {
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
    let image = Animal.useAvatar(tokenId)

    <div
      className={
        open Css
        style(list{width(vh(12.)), cursor(#pointer)})
      }>
      <img
        className={
          open Css
          style(list{width(#percent(100.))})
        }
        onClick={_e => clearAndPush("/#details/" ++ tokenId->TokenId.toString)}
        src=image
      />
    </div>
  }
}

module ClaimLoyaltyTokenButtons = {
  @react.component
  let make = (
    ~chain,
    ~userAddress: string,
    ~id: string,
    ~refreshLoyaltyTokenBalance,
    ~numberOfTokens,
  ) => {
    let tokenId = id->TokenId.fromStringUnsafe

    let (redeemLoyaltyTokens, transactionStatus) = ContractActions.useRedeemLoyaltyTokens(
      userAddress,
    )
    let balanceAvailableOnTokens = QlHooks.useUnredeemedLoyaltyTokenDueForUser(
      ~chain,
      tokenId,
      numberOfTokens,
    )

    let etherScanUrl = RootProvider.useEtherscanUrl()

    React.useEffect2(() => {
      switch transactionStatus {
      | Complete(_) => refreshLoyaltyTokenBalance()
      | UnInitialised
      | Created
      | DaiPermit(_)
      | SubmittedMetaTx
      | SignMetaTx
      | ServerError(_)
      | SignedAndSubmitted(_)
      | Declined(_)
      | Failed => ()
      }
      None
    }, (transactionStatus, refreshLoyaltyTokenBalance))

    switch balanceAvailableOnTokens {
    | None => React.null
    | Some(balanceAvailableOnTokens) =>
      <small>
        {switch transactionStatus {
        | UnInitialised =>
          <p>
            <a onClick={_ => redeemLoyaltyTokens()}>
              {("Redeem " ++
              (balanceAvailableOnTokens->Web3Utils.fromWeiBNToEthPrecision(~digits=5) ++
              " loyalty tokens"))->restr}
            </a>
          </p>
        | DaiPermit(_)
        | SignMetaTx
        | ServerError(_)
        | SubmittedMetaTx
        | Created =>
          <p> {"Transaction Created"->restr} </p>
        | SignedAndSubmitted(txHash) =>
          <p>
            {"Processing: "->restr}
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={"https://" ++ (etherScanUrl ++ ("/tx/" ++ txHash))}>
              {"view transaction"->restr}
            </a>
          </p>
        | Declined(message) => <p> {("Submitting transaction failed: " ++ message)->restr} </p>
        | Complete(_txResult) =>
          <p> {"Tokens claimed (please reload the page, this will be improved soon)"->restr} </p>
        | Failed => <p> {"Transaction failed"->restr} </p>
        }}
      </small>
    }
  }
}

module UserDetails = {
  @react.component
  let make = (
    ~chain,
    ~patronQueryResult: QlHooks.LoadPatron.LoadPatron_inner.t_patron,
    ~optThreeBoxData: option<UserProvider.threeBoxUserInfo>,
    ~userAddress: string,
  ) => {
    let isForeclosed = QlHooks.useIsForeclosed(~chain=Client.MainnetQuery, userAddress)
    let isAddressCurrentUser = RootProvider.useIsAddressCurrentUser(userAddress)

    @ocaml.doc("
     * 1 of four scenarios for each user:
     * User has never owned a wildcard.
     * User owns a wildcard(s), hasn't owned others in the past.
     * User owns a wildcard(s), has owned others in the past.
     * User owned (past tense) wildcards, but they have foreclosed / been bought from them.
     *
     * TODO: It might make the code more readable to encode the above for options into an variant.
     *       It is hard to reason about if we are just checking if the array is null, or not etc.
     ")
    let currentlyOwnedTokens = isForeclosed
      ? []
      : patronQueryResult.tokens->Array.map(token => token.id)

    let allPreviouslyOwnedTokens =
      patronQueryResult.previouslyOwnedTokens->Array.map(token => token.id)

    let uniquePreviouslyOwnedTokens = isForeclosed
      ? allPreviouslyOwnedTokens
      : Set.String.fromArray(allPreviouslyOwnedTokens)
        ->Set.String.removeMany(currentlyOwnedTokens)
        ->Set.String.toArray

    let optProfile = \">>="(optThreeBoxData, a => a.profile)
    let image: string =
      \">>="(
        \"<$>"(\">>="(\">>="(optProfile, a => a.image), img => img->Array.get(0)), a =>
          a.contentUrl
        ),
        content => Js.Dict.get(content, "/"),
      )->Option.mapWithDefault(Blockie.makeBlockie(. userAddress), hash =>
        "https://ipfs.infura.io/ipfs/" ++ hash
      )
    let optName = \">>="(optProfile, a => a.name)
    let optDescription = \">>="(optProfile, a => a.description)
    let optTwitter = {
      open UserProvider
      \"<$>"(\">>="(\">>="(optThreeBoxData, a => a.verifications), a => a.twitter), a => a.username)
    }
    let etherScanUrl = RootProvider.useEtherscanUrl()

    let optUsdPrice = UsdPriceProvider.useUsdPrice()

    let monthlyCotributionWei =
      patronQueryResult.patronTokenCostScaledNumerator
      ->BN.mul(BN.new_("2592000")) // A month with 30 days has 2592000 seconds
      ->BN.div(
        // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
        BN.new_("31536000000000000000"),
      )

    let monthlyContributionEth = monthlyCotributionWei->Web3Utils.fromWeiBNToEthPrecision(~digits=4)
    let optMonthlyContributionUsd = \"<$>"(optUsdPrice, currentUsdEthPrice =>
      toFixedWithPrecisionNoTrailingZeros(
        Float.fromString(monthlyContributionEth)->Option.mapWithDefault(0., a => a) *.
          currentUsdEthPrice,
        ~digits=2,
      )
    )

    // This is the value of tokens that are currently in the users account (IE their spendable balance)
    let (totalLoyaltyTokensOpt, updateFunction) = ContractActions.useUserLoyaltyTokenBalance(
      userAddress,
    )

    // This is the value of ALL tokens that this address has ever claimed, or is able to claim (even if they have spent those tokens)!
    let totalLoyaltyTokensAvailableAndClaimedOpt = QlHooks.useTotalLoyaltyToken(
      ~chain=Client.MainnetQuery,
      userAddress,
    )

    let nonUrlState = RootProvider.useNonUrlState()
    let clearNonUrlState = RootProvider.useClearNonUrlState()

    <div
      className={
        open Css
        style(list{width(#percent(100.))})
      }>
      <Rimble.Flex flexWrap="wrap" alignItems="start">
        <Rimble.Box
          p=1
          width=[1., 1., 0.3333]
          className={
            open Css
            style(list{textAlign(#center)})
          }>
          <img
            className={
              open Css
              style(list{
                borderRadius(#percent(100.)),
                width(#vh(25.)),
                height(#vh(25.)),
                objectFit(#cover),
              })
            }
            src=image
          />
          <br />
          {switch nonUrlState {
          | UserVerificationScreen =>
            <div
              className={
                open Css
                style(list{position(#relative)})
              }>
              <Rimble.Button.Text
                icononly=true
                icon="Close"
                color="moon-gray"
                position="absolute"
                top=0
                right=0
                m=1
                onClick={_ => clearNonUrlState()}
              />
              <React.Suspense fallback={<Rimble.Loader />}> <LazyThreeBoxUpdate /> </React.Suspense>
            </div>
          | UpdateDepositScreen =>
            <div
              className={
                open Css
                style(list{position(#relative)})
              }>
              <Rimble.Button.Text
                icononly=true
                icon="Close"
                color="moon-gray"
                position="absolute"
                top=0
                right=0
                m=1
                onClick={_ => clearNonUrlState()}
              />
              <UpdateDeposit chain=Client.MainnetQuery closeButtonText="Close" />
            </div>
          | LoginScreen(_)
          | UpdatePriceScreen(_)
          | BuyScreen(_)
          | AuctionScreen(_)
          | NoExtraState => <>
              {optName->reactMap(name => <h2> {name->restr} </h2>)}
              {optTwitter->reactMap(twitterHandle =>
                <a
                  className=Styles.navListText
                  target="_blank"
                  rel="noopener noreferrer"
                  href={"https://twitter.com/" ++ twitterHandle}>
                  {("@" ++ twitterHandle)->restr}
                </a>
              )}
              <br />
              {optDescription->reactMap(description => <p> {description->restr} </p>)}
              <a
                className=Styles.navListText
                target="_blank"
                rel="noopener noreferrer"
                href={"https://" ++ (etherScanUrl ++ ("/address/" ++ userAddress))}>
                {Helper.elipsify(userAddress, 10)->restr}
              </a>
              <br />
              {
                // NOTE: the number of loyalty tokens of a user currently will always show.
                //       We are thinking of making this "private" only to the current logged in user. To enable this remove the `|| true` from the line below
                isAddressCurrentUser
                  ? <>
                      <small>
                        <p>
                          {("Claimed Loyalty Token Balance: " ++
                          (totalLoyaltyTokensOpt->Option.mapWithDefault(
                            "Loading",
                            claimedLoyaltyTokens =>
                              claimedLoyaltyTokens->Web3Utils.fromWeiBNToEthPrecision(~digits=6),
                          ) ++
                          " WLT"))->restr}
                        </p>
                      </small>
                      {switch currentlyOwnedTokens {
                      | [] => React.null
                      | currentlyOwnedTokens =>
                        <ClaimLoyaltyTokenButtons
                          id={currentlyOwnedTokens->Array.getUnsafe(0)}
                          chain
                          refreshLoyaltyTokenBalance=updateFunction
                          userAddress
                          numberOfTokens={currentlyOwnedTokens->Array.length}
                        />
                      }}
                      <a href="/#ethturin-quadratic-voting"> {"vote"->restr} </a>
                    </>
                  : <small>
                      <p>
                        {("Loyalty Token Balance Generated: " ++
                        (totalLoyaltyTokensAvailableAndClaimedOpt->Option.mapWithDefault(
                          "Loading",
                          ((totalLoyaltyTokens, _)) =>
                            totalLoyaltyTokens->Web3Utils.fromWeiBNToEthPrecision(~digits=5),
                        ) ++
                        " WLT"))->restr}
                      </p>
                    </small>
              }
              {if isAddressCurrentUser {
                <React.Fragment>
                  <br />
                  <ActionButtons.UpdateDeposit />
                  <br />
                  // {UserProvider.useIsUserValidated(currentAccount)
                  //    ? <ShareSocial /> : <Validate />}
                  <Validate />
                </React.Fragment>
              } else {
                React.null
              }}
            </>
          }}
        </Rimble.Box>
        <Rimble.Box p=1 width=[1., 1., 0.3333]>
          <h2> {"Monthly Contribution"->restr} </h2>
          <p>
            <React.Fragment>
              {j`$monthlyContributionEth ETH\\xa0`->restr}
              {optMonthlyContributionUsd->reactMap(usdValue =>
                <small> {j`($usdValue USD)`->restr} </small>
              )}
            </React.Fragment>
          </p>
        </Rimble.Box>
        <Rimble.Box p=1 width=[1., 1., 0.3333]>
          {switch currentlyOwnedTokens {
          | [] =>
            uniquePreviouslyOwnedTokens->Array.length > 0
              ? <p> {"User currently doesn't currently own a wildcard."->restr} </p>
              : <p> {"User has never owned a wildcard."->restr} </p>
          | currentlyOwnedTokens =>
            <React.Fragment>
              <Rimble.Heading> {"Currently owned tokens"->React.string} </Rimble.Heading>
              <Rimble.Flex flexWrap="wrap" className=centreAlignOnMobile>
                {React.array(
                  currentlyOwnedTokens->Array.map(tokenId =>
                    <Token key=tokenId tokenId={TokenId.fromStringUnsafe(tokenId)} />
                  ),
                )}
              </Rimble.Flex>
              <br />
              <br />
              <br />
            </React.Fragment>
          }}
          {switch uniquePreviouslyOwnedTokens {
          | [] => React.null
          | uniquePreviouslyOwnedTokens =>
            <React.Fragment>
              <Rimble.Heading> {"Previously owned tokens"->React.string} </Rimble.Heading>
              <Rimble.Flex flexWrap="wrap" className=centreAlignOnMobile>
                {React.array(
                  uniquePreviouslyOwnedTokens->Array.map(tokenId =>
                    <Token key=tokenId tokenId={TokenId.fromStringUnsafe(tokenId)} />
                  ),
                )}
              </Rimble.Flex>
            </React.Fragment>
          }}
        </Rimble.Box>
      </Rimble.Flex>
    </div>
  }
}

@react.component
let make = (~chain, ~userAddress: string) => {
  let userAddressLowerCase = userAddress->Js.String.toLowerCase
  let patronQuery = QlHooks.useQueryPatron(~chain=Client.MainnetQuery, userAddressLowerCase)
  let userInfoContext = UserProvider.useUserInfoContext()
  let reloadUser = forceReload => userInfoContext.update(userAddressLowerCase, forceReload) // double check that data is loaded.
  reloadUser(false)
  let optThreeBoxData = UserProvider.use3BoxUserData(userAddressLowerCase)

  <Rimble.Flex flexWrap="wrap" alignItems="center" className=Styles.topBody>
    {switch patronQuery {
    | Some(patronQueryResult) => <UserDetails chain optThreeBoxData patronQueryResult userAddress />
    | None =>
      <div>
        <Rimble.Heading> {"Loading user profile:"->React.string} </Rimble.Heading> <Rimble.Loader />
      </div>
    }}
  </Rimble.Flex>
}
