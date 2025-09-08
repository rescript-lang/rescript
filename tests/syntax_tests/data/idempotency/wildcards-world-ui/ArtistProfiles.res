open Globals

let centreAlignOnMobile = {
  open Css
  style(list{media("(max-width: 831px)", list{alignItems(center), justifyContent(center)})})
}

module ArtistDetails = {
  @react.component
  let make = (
    ~optArtistEthAddress,
    ~optArtistName,
    ~optArtistWebsite,
    ~optArtistLaunchedWildcards,
    ~optArtistUnlaunchedWildcards,
    ~optArtistOrgs,
  ) => {
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
    let artistEthAddress =
      \"||||"(optArtistEthAddress, CONSTANTS.nullEthAddress)->Js.String.toLowerCase
    let userInfoContext = UserProvider.useUserInfoContext()
    let reloadUser = forceReload => userInfoContext.update(artistEthAddress, forceReload)
    reloadUser(false)
    let optThreeBoxData = UserProvider.use3BoxUserData(artistEthAddress)
    let optProfile = \">>="(optThreeBoxData, a => a.profile)

    let image: string =
      \">>="(
        \"<$>"(\">>="(\">>="(optProfile, a => a.image), img => img->Array.get(0)), a =>
          a.contentUrl
        ),
        content => Js.Dict.get(content, "/"),
      )->Option.mapWithDefault(Blockie.makeBlockie(. artistEthAddress), hash =>
        "https://ipfs.infura.io/ipfs/" ++ hash
      )
    let optName = \">>="(optProfile, a => a.name)
    let optDescription = \">>="(optProfile, a => a.description)
    let optTwitter = {
      open UserProvider
      \"<$>"(\">>="(\">>="(optThreeBoxData, a => a.verifications), a => a.twitter), a => a.username)
    }
    let etherScanUrl = RootProvider.useEtherscanUrl()

    let artistsAnimalsArrayLaunched =
      optArtistLaunchedWildcards->Option.mapWithDefault([], (
        tokens: array<QlHooks.ArtistQuery.ArtistQuery_inner.t_artist_by_pk_launchedWildcards>,
      ) =>
        tokens->Array.map(token => token.id->Option.getWithDefault("_")->TokenId.fromStringUnsafe)
      )
    Js.log(optArtistUnlaunchedWildcards)

    let currentUsdEthPrice = UsdPriceProvider.useUsdPrice()
    let (totalCollectedMainnetEth, totalCollectMaticDai) = QlHooks.useTotalRaisedAnimalGroup(
      artistsAnimalsArrayLaunched,
    )

    let (totalPatronageUsd, totalBreakdownString) = switch (
      totalCollectedMainnetEth,
      totalCollectMaticDai,
    ) {
    | (Some(mainnetEth), Some(maticDai)) => (
        (currentUsdEthPrice->Option.mapWithDefault(0., usdEthRate =>
          mainnetEth->Eth.getFloat(Eth.Usd(usdEthRate, 2))
        ) +. maticDai->Eth.getFloat(Eth.Eth(#ether)))->Js.Float.toFixedWithPrecision(~digits=6),
        mainnetEth->Web3Utils.fromWeiBNToEthPrecision(~digits=4) ++
          (" ETH + " ++
          (maticDai->Web3Utils.fromWeiBNToEthPrecision(~digits=2) ++ " DAI")),
      )
    | _ => ("loading", "loading")
    }

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
          {optArtistEthAddress->reactMap(_ => <>
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
          </>)}
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
          | UpdateDepositScreen
          | LoginScreen(_)
          | UpdatePriceScreen(_)
          | BuyScreen(_)
          | AuctionScreen(_)
          | NoExtraState => <>
              <h2>
                {optName
                ->Option.getWithDefault(\"||||"(optArtistName, "Loading artist name"))
                ->React.string}
              </h2>
              {optTwitter->reactMap(twitterHandle =>
                <a
                  className=Styles.navListText
                  target="_blank"
                  rel="noopener noreferrer"
                  href={"https://twitter.com/" ++ twitterHandle}>
                  {("@" ++ twitterHandle)->React.string}
                </a>
              )}
              <br />
              {optDescription->reactMap(description =>
                <p> {BadWords.make()->BadWords.clean(description)->React.string} </p>
              )}
              {optArtistWebsite->Option.mapWithDefault(React.null, website =>
                <a
                  className=Styles.navListText
                  target="_blank"
                  rel="noopener noreferrer"
                  href=website>
                  {(\"||||"(optArtistName, "Artist") ++ "'s website")->React.string}
                </a>
              )}
              <br />
              <br />
              {optArtistEthAddress->reactMap(_ => <>
                <a
                  className=Styles.navListText
                  target="_blank"
                  rel="noopener noreferrer"
                  href={"https://" ++ (etherScanUrl ++ ("/address/" ++ artistEthAddress))}>
                  {Helper.elipsify(artistEthAddress, 10)->React.string}
                </a>
                <br />
              </>)}
            </>
          }}
        </Rimble.Box>
        <Rimble.Box p=1 width=[1., 1., 0.3333]>
          <h2> {"Total contributed by artist"->React.string} </h2>
          {(totalPatronageUsd ++ "USD")->React.string}
          <br />
          <small> {totalBreakdownString->React.string} </small>
          {switch optArtistOrgs {
          | None => <h2> {"Loading orgs"->React.string} </h2>
          | Some([]) => React.null
          | Some(orgList) => <>
              <br />
              <br />
              <h4> {"Organisations this artist has contributed to:"->React.string} </h4>
              {orgList
              ->Array.map(org => {
                open QlHooks
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
                    onClick={_e => clearAndPush("/#org/" ++ org.id)}
                    src={CONSTANTS.cdnBase ++ org.logo}
                  />
                </div>
              })
              ->React.array}
            </>
          }}
        </Rimble.Box>
        <Rimble.Box p=1 width=[1., 1., 0.3333]>
          {switch optArtistLaunchedWildcards {
          | None => <p> {"loading artists wildcards"->React.string} </p>
          | Some([]) => <p> {"Artist doesn't have any launched wildcards"->React.string} </p>
          | Some(_) =>
            <React.Fragment>
              <Rimble.Heading> {"Wildcards created by artist"->React.string} </Rimble.Heading>
              <Rimble.Flex flexWrap="wrap" className=centreAlignOnMobile>
                {React.array(
                  artistsAnimalsArrayLaunched->Array.map(token => {
                    let id = token->TokenId.toString
                    <UserProfile.Token key=id tokenId=token />
                  }),
                )}
              </Rimble.Flex>
              <br />
              <br />
              <br />
            </React.Fragment>
          }}
          {switch optArtistUnlaunchedWildcards {
          | None
          | Some([]) => React.null
          | Some(unlaunchedWildcards) =>
            <React.Fragment>
              <Rimble.Heading> {"Coming soon"->React.string} </Rimble.Heading>
              <Rimble.Flex flexWrap="wrap" className=centreAlignOnMobile>
                {React.array(
                  unlaunchedWildcards->Array.map((
                    token: QlHooks.ArtistQuery.ArtistQuery_inner.t_artist_by_pk_unlaunchedWildcards,
                  ) =>
                    token.image->Option.mapWithDefault(React.null, image =>
                      <OrgProfile.ComingSoonAnimal
                        key={token.key->string_of_int}
                        image={CONSTANTS.cdnBase ++ image}
                        onClick={() => ()}
                      />
                    )
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
let make = (~artistIdentifier: string) => {
  // let artistAddress =
  Js.log(artistIdentifier)

  let optArtistEthAddress = QlHooks.useArtistEthAddress(~artistIdentifier)
  let optArtistName = QlHooks.useArtistName(~artistIdentifier)
  let optArtistWebsite = QlHooks.useArtistWebsite(~artistIdentifier)
  let optArtistLaunchedWildcards = QlHooks.useArtistLaunchedWildcards(~artistIdentifier)
  let optArtistUnlaunchedWildcards = QlHooks.useArtistUnlaunchedWildcards(~artistIdentifier)
  let optArtistOrgs = QlHooks.useArtistOrgs(~artistIdentifier)

  <Rimble.Flex flexWrap="wrap" alignItems="center" className=Styles.topBody>
    <ArtistDetails
      optArtistEthAddress
      optArtistName
      optArtistWebsite
      optArtistLaunchedWildcards
      optArtistUnlaunchedWildcards
      optArtistOrgs
    />
  </Rimble.Flex>
}
