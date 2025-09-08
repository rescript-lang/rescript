open Globals

let flameImg = "/img/streak-flame.png"

module ShareSocial = {
  @module("./components/shareSocialMedia") @react.component
  external make: unit => React.element = "default"
}
module EditButton = {
  @react.component
  let make = (~animal: TokenId.t) => {
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
    let isExplorer = Router.useIsExplorer()

    <Rimble.Button
      onClick={event => {
        ReactEvent.Form.preventDefault(event)
        clearAndPush(
          "#" ++ (InputHelp.getPagePrefix(isExplorer) ++ ("details/" ++ animal->TokenId.toString)),
        )
      }}>
      {React.string("Edit")}
    </Rimble.Button>
  }
}

module Streak = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let animalName = \"||||"(QlHooks.useWildcardName(animal), "Loading")

    let daysHeld = QlHooks.useDaysHeld(~chain, animal)

    switch daysHeld {
    | Some((daysHeldFloat, _timeAquired)) =>
      let numDaysStr = daysHeldFloat->Js.Float.toFixed

      <Rimble.Tooltip message=j`$animalName has been held for $numDaysStr days by the same owner.`>
        <div id="inner" className=Styles.positionRelative>
          <img className=Styles.flameImg src=flameImg />
          <p className=Styles.streakText> <strong> {React.string(numDaysStr)} </strong> </p>
        </div>
      </Rimble.Tooltip>
    | None => React.null
    }
  }
}

module DisplayAfterDate = {
  @react.component
  let make = (~endDateMoment, ~beforeComponent, ~afterComponent) => {
    let isBeforeDate = React.useCallback1(
      () => MomentRe.diff(endDateMoment, MomentRe.momentNow(), #seconds) > 0.,
      [endDateMoment],
    )

    let (beforeDate, setIsBeforeDate) = React.useState(() => isBeforeDate())
    React.useEffect2(() => {
      let timeout = Js.Global.setTimeout(() => setIsBeforeDate(_ => isBeforeDate()), 1500)
      Some(() => Js.Global.clearTimeout(timeout))
    }, (setIsBeforeDate, isBeforeDate))

    beforeDate ? beforeComponent : afterComponent
  }
}

module AuctionDisplay = {
  @react.component
  let make = (~chain, ~launchTime, ~animal) => {
    let currentPriceWei = Animal.useAuctionPriceWei(~chain, animal, launchTime)

    let optCurrentUsdEthPrice = UsdPriceProvider.useUsdPrice()

    <>
      <ActionButtons.Auction animal />
      <br />
      <br />
      {switch chain {
      | Client.MaticQuery =>
        <p className={Styles.noMarginTop ++ (" " ++ Styles.noMarginBottom)}>
          {currentPriceWei->Option.mapWithDefault("Loading"->React.string, price => <>
            {(price->Web3Utils.fromWeiBNToEthPrecision(~digits=4) ++ " USD")->React.string}
            <small> {" (DAI)"->React.string} </small>
          </>)}
        </p>
      | Client.Neither
      | Client.MainnetQuery =>
        let (priceEth, optPriceUsd) = PriceDisplay.priceWeiToTuple(
          currentPriceWei->Option.getWithDefault(BN.new_("0")),
          optCurrentUsdEthPrice,
        )
        <PriceDisplay.PurePriceDisplay priceEth optPriceUsd />
      }}
    </>
  }
}
module AuctionDetails = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let launchTimeOpt = QlHooks.useLaunchTimeBN(~chain, animal)
    let foreclosureTimeOpt = QlHooks.useForeclosureTimeBn(~chain, animal->TokenId.toString)

    switch (launchTimeOpt, foreclosureTimeOpt) {
    | (Some(launchTime), Some(foreclosurTime)) =>
      if foreclosurTime->BN.lt(launchTime) {
        <AuctionDisplay chain animal launchTime />
      } else {
        <AuctionDisplay chain animal launchTime=foreclosurTime />
      }
    | (Some(launchTime), None) => <AuctionDisplay chain animal launchTime />
    | (None, Some(foreclosureTime)) => <AuctionDisplay chain animal launchTime=foreclosureTime />
    | (None, None) => <p> {"Loading"->React.string} </p>
    }
  }
}

module BasicAnimalDisplay = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let owned = animal->QlHooks.useIsAnimalOwened(~chain)
    let currentPatron = \"||||"(QlHooks.usePatron(~chain, animal), "Loading")
    let displayName = UserProvider.useDisplayName(currentPatron)

    let displayNameStr = UserProvider.displayNameToString(displayName)
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
    let nonUrlRouting = RootProvider.useNonUrlState()

    let isOnAuction = Animal.useIsOnAuction(~chain, animal)

    isOnAuction
      ? <AuctionDetails chain animal />
      : <>
          {switch nonUrlRouting {
          | UserVerificationScreen
          | UpdateDepositScreen
          | UpdatePriceScreen(_)
          | BuyScreen(_)
          | AuctionScreen(_) => React.null
          | LoginScreen(_)
          | NoExtraState =>
            owned ? <EditButton animal /> : <ActionButtons.Buy animal chain />
          }}
          <br />
          <br />
          <PriceDisplay chain animal />
          <a
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              clearAndPush(j`/#user/$currentPatron`)
            }}>
            {displayNameStr->restr}
          </a>
        </>
  }
}

module SquareBox = {
  // Css taken from here: https://spin.atomicobject.com/2015/07/14/css-responsive-square/
  @react.component
  let make = (~children) =>
    <div
      className={
        open Css
        style(list{
          width(#percent(100.)),
          position(relative),
          after(list{unsafe("content", "\"\""), display(block), paddingBottom(#percent(100.))}),
        })
      }>
      <div
        className={
          open Css
          style(list{position(absolute), width(#percent(100.)), height(#percent(100.))})
        }>
        children
      </div>
    </div>
}

module AnimalOnLandingPage = {
  @react.component
  let make = (
    ~animal: TokenId.t,
    ~scalar: float=1.,
    ~chain,
    ~enlargement: float=1.,
    ~optionEndDateMoment: option<MomentRe.Moment.t>,
    ~isGqlLoaded,
  ) => {
    let name = \"||||"(QlHooks.useWildcardName(animal), "Loading")
    let isExplorer = Router.useIsExplorer()

    let orgBadge = Animal.useGetOrgBadgeImage(~tokenId=animal)
    let orgId = \"||||"(QlHooks.useWildcardOrgId(~tokenId=animal), "")

    let currentPriceWei = QlHooks.usePrice(~chain, animal)

    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
    let image = Animal.useAvatar(animal)

    let normalImage = () =>
      <SquareBox>
        <img
          className={
            open Css
            style(list{width(#percent(100.)), height(#percent(100.)), objectFit(#contain)})
          }
          src=image
        />
      </SquareBox>

    let isOnAuction = Animal.useIsOnAuction(~chain, animal)

    let componentWithoutImg = (img, ~hideBadges: bool) =>
      <SquareBox>
        <div className={Styles.headerImg(enlargement, scalar)}> {img()} </div>
        {if hideBadges {
          React.null
        } else {
          <>
            {isGqlLoaded
              ? switch optionEndDateMoment {
                | Some(_endDateMoment) => React.null
                | None =>
                  isOnAuction
                    ? React.null
                    : switch currentPriceWei {
                      | Price(_) =>
                        <div className=Styles.overlayFlameImg> <Streak chain animal /> </div>
                      | Loading
                      | Foreclosed(_) => React.null
                      }
                }
              : React.null}
            <div
              onClick={e => {
                ReactEvent.Mouse.stopPropagation(e)
                ReactEvent.Mouse.preventDefault(e)
                clearAndPush("#org/" ++ orgId)
              }}
              className={Styles.overlayBadgeImg(~x=100., ~y=80.)}>
              <img className=Styles.flameImg src=orgBadge />
            </div>
          </>
        }}
      </SquareBox>

    <div
      id="animalBox"
      className={
        open Css
        style(list{textAlign(center)})
      }>
      <a
        className=Styles.clickableLink
        onClick={event => {
          ReactEvent.Mouse.preventDefault(event)
          clearAndPush(
            "#" ++
            (InputHelp.getPagePrefix(isExplorer) ++
            ("details/" ++ animal->TokenId.toString)),
          )
        }}>
        {componentWithoutImg(normalImage, ~hideBadges=false)}
        <div> <h2> {React.string(name)} </h2> </div>
      </a>
      {switch optionEndDateMoment {
      | Some(endDateMoment) =>
        <div>
          <h3 className=Styles.colorGreen> {React.string("COMING IN")} </h3>
          <CountDown endDateMoment displayUnits=false />
        </div>
      | None => isGqlLoaded ? <div> <BasicAnimalDisplay chain animal /> </div> : React.null
      }}
    </div>
  }
}

module CarouselAnimal = {
  @react.component
  let make = (~animal: TokenId.t, ~scalar, ~enlargement: float=1., ~isGqlLoaded=true, ~chain) => {
    let isLaunched = animal->Animal.useIsLaunched(~chain)

    let makeAnimalOnLandingPage = optionEndDateMoment =>
      <AnimalOnLandingPage animal chain scalar optionEndDateMoment enlargement isGqlLoaded />
    switch isLaunched {
    | Animal.LaunchDate(endDateMoment) =>
      <DisplayAfterDate
        endDateMoment
        afterComponent={makeAnimalOnLandingPage(None)}
        beforeComponent={makeAnimalOnLandingPage(Some(endDateMoment))}
      />
    | Animal.Launched => makeAnimalOnLandingPage(None)
    | Animal.Loading => makeAnimalOnLandingPage(None)
    }
  }
}

module AnimalCarousel = {
  @react.component
  let make = (~isGqlLoaded) => {
    let (carouselIndex, setCarouselIndex) = React.useState(() => 17)
    let homePageAnimals = QlHooks.useHomePageAnimalArray()
    let numItems = homePageAnimals->Array.length
    // let numItems = Animal.orderedArray->Array.length;

    <Rimble.Box className=Styles.positionRelative>
      <Carousel
        className=Styles.carousel
        slidesPerPage=5
        centered=true
        value=carouselIndex
        animationSpeed=1000
        infinite=true
        autoPlay=5000
        arrowLeft={<span
          className={Styles.carouselArrow(true)}
          onClick={event => {
            ReactEvent.Mouse.preventDefault(event)
            setCarouselIndex(_ => carouselIndex - 1)
            ReactEvent.Mouse.stopPropagation(event)
          }}>
          {`◄`->React.string}
        </span>}
        arrowRight={<span
          className={Styles.carouselArrow(false)}
          onClick={event => {
            ReactEvent.Mouse.preventDefault(event)
            setCarouselIndex(_ => carouselIndex + 1)
            ReactEvent.Mouse.stopPropagation(event)
          }}>
          {`►`->React.string}
        </span>}
        arrows=true
        onChange={test => setCarouselIndex(_ => test)}>
        {React.array(
          homePageAnimals->Array.mapWithIndex((index, animalInfo) => {
            let (opacity, scalar) = switch index {
            | x if x == mod(carouselIndex, numItems) => (1., 1.0)
            | x
              if x == mod(carouselIndex - 1, numItems) || x == mod(carouselIndex + 1, numItems) => (
                0.8,
                0.8,
              )
            | x
              if x == mod(carouselIndex - 2, numItems) || x == mod(carouselIndex + 2, numItems) => (
                0.1,
                0.7,
              )
            | _ => (0., 0.6)
            }

            <div key={animalInfo.id->TokenId.toString} className={Styles.fadeOut(opacity)}>
              <CarouselAnimal
                animal=animalInfo.id
                chain={animalInfo.id->Animal.getChainIdFromAnimalId}
                isGqlLoaded
                scalar
                enlargement=1.
              />
            </div>
          }),
        )}
      </Carousel>
    </Rimble.Box>
  }
}

module AnimalActionsOnDetailsPage = {
  module Unowned = {
    @react.component
    let make = (~chain, ~animal, ~price) => <>
      {switch animal->Animal.useIsLaunched(~chain) {
      | LaunchDate(endDateMoment) =>
        <DisplayAfterDate
          endDateMoment
          afterComponent={price()}
          beforeComponent={<React.Fragment> <CountDown endDateMoment /> </React.Fragment>}
        />
      | Launched => price()
      | Loading => <Rimble.Loader />
      }}
    </>
  }

  @react.component
  let make = (~chain, ~animal) => {
    let owned = animal->QlHooks.useIsAnimalOwened(~chain)
    // let currentAccount =
    //   RootProvider.useCurrentUser()->mapWithDefault("loading", a => a);
    let currentPatron = \"||||"(QlHooks.usePatron(~chain, animal), "Loading")
    let displayName = UserProvider.useDisplayName(currentPatron)
    let displayNameStr = UserProvider.displayNameToString(displayName)
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

    let nonUrlRouting = RootProvider.useNonUrlState()
    let isOnAuction = Animal.useIsOnAuction(~chain, animal)

    let price = () =>
      isOnAuction
        ? <AuctionDetails chain animal />
        : <React.Fragment>
            <a
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                clearAndPush(j`/#user/$currentPatron`)
              }}>
              {displayNameStr->restr}
            </a>
            <PriceDisplay chain animal />
            {switch nonUrlRouting {
            | UserVerificationScreen
            | UpdateDepositScreen
            | UpdatePriceScreen(_)
            | BuyScreen(_)
            | AuctionScreen(_) => React.null
            | LoginScreen(_)
            | NoExtraState =>
              <ActionButtons.Buy chain animal />
            }}
          </React.Fragment>

    if owned {
      <React.Fragment>
        <PriceDisplay animal chain />
        <ActionButtons.UpdatePrice animal />
        <br />
        <ActionButtons.UpdateDeposit />
        <br />
        // {UserProvider.useIsUserValidated(currentAccount)
        //    ? <ShareSocial /> : <Validate />}
        <Validate />
      </React.Fragment>
    } else {
      <Unowned chain animal price />
    }
  }
}

module DetailsViewAnimal = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let orgId = \"||||"(QlHooks.useWildcardOrgId(~tokenId=animal), "")

    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

    let image = Animal.useAvatar(animal)
    let optArtistInfo = QlHooks.useWildcardArtist(animal)

    let ownedAnimalImg = {
      open Css
      style(list{
        position(absolute),
        zIndex(1),
        maxWidth(#percent(100.)),
        top(#percent(50.)),
        transform(translate(#percent(-50.), #percent(-50.))),
        left(#percent(50.)),
      })
    }

    let normalImage = () => <img className=ownedAnimalImg src=image />
    let orgBadge = Animal.useGetOrgBadgeImage(~tokenId=animal)

    let displayAnimal = animalImage =>
      <SquareBox>
        {animalImage()}
        {optArtistInfo->Option.mapWithDefault(React.null, artistInfo =>
          <p
            className={
              open Css
              style(list{
                position(absolute),
                bottom(#percent(2.)),
                textAlign(center),
                width(#percent(100.)),
              })
            }>
            {"Art by: "->React.string}
            <a
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                clearAndPush("/#artist/" ++ artistInfo.id)
              }}>
              {artistInfo.name->React.string}
            </a>
          </p>
        )}
        <div
          onClick={e => {
            ReactEvent.Mouse.stopPropagation(e)
            ReactEvent.Mouse.preventDefault(e)
            clearAndPush("#org/" ++ orgId)
          }}
          className={Styles.overlayBadgeImg(~x=80., ~y=80.)}>
          <img className=Styles.flameImg src=orgBadge />
        </div>
      </SquareBox>

    <React.Fragment>
      {displayAnimal(() => normalImage())}
      <h2> {\"||||"(QlHooks.useWildcardName(animal), "Loading")->restr} </h2>
      <AnimalActionsOnDetailsPage chain animal />
    </React.Fragment>
  }
}

module DetailsView = {
  @react.component
  let make = (~chain, ~optionAnimal: option<TokenId.t>) => {
    %log.info(
      "optionAnimal"
      ("a", optionAnimal)
    )

    switch optionAnimal {
    | None =>
      <div>
        <h1> {React.string("We are unable to find that animal in our system.")} </h1>
        <p> {"Please check the spelling and try again."->restr} </p>
      </div>
    | Some(animal) => <DetailsViewAnimal chain animal />
    }
  }
}

module DefaultLook = {
  @react.component
  let make = (~isGqlLoaded) => {
    let url = ReasonReactRouter.useUrl()

    <div className=Styles.centerText>
      {switch Js.String.split("/", url.hash) {
      | ["details", animalStr]
      | ["explorer", "details", animalStr]
      | ["explorer", "details", animalStr, ""] =>
        %log.info(
          "the animalString"
          ("a", animalStr)
        )
        let optionAnimal = TokenId.make(animalStr)
        let chain =
          optionAnimal->Option.mapWithDefault(Client.MainnetQuery, Animal.getChainIdFromAnimalId)

        <DetailsView chain optionAnimal={TokenId.make(animalStr)} />
      | _ =>
        <React.Fragment>
          <AnimalCarousel isGqlLoaded />
          <Rimble.Box className=Styles.dappImagesCounteractOffset>
            {isGqlLoaded ? <TotalRaised /> : React.null}
          </Rimble.Box>
        </React.Fragment>
      }}
    </div>
  }
}

module DefaultLeftPanel = {
  @react.component
  let make = () => {
    let translationModeContext = ReactTranslate.useTranslationModeContext()
    let translation = ReactTranslate.useTranslate(. translationModeContext.translationMode)
    <React.Fragment>
      <h1 className=Styles.heading>
        <span className=Styles.colorBlue> {translation(. "bluetext")->restr} </span>
        <br />
        {translation(. "ethereum")->restr}
        <br />
        <span className=Styles.colorGreen> {"conservation"->restr} </span>
        {(" " ++ translation(. "tokens"))->restr}
      </h1>
      <hr />
      <h3 className=Styles.subHeading> {translation(. "subHeading")->restr} </h3>
    </React.Fragment>
  }
}

module UnlaunchedAnimalInfo = {
  @react.component
  let make = (~chain, ~endDateMoment, ~animal: TokenId.t) => {
    let animalName = \"||||"(QlHooks.useWildcardName(animal), "Loading")

    let ratio = QlHooks.usePledgeRate(~chain, animal)
    let monthlyRate = Js.Float.toString(ratio *. 100.)

    <DisplayAfterDate
      endDateMoment
      afterComponent={<Info chain tokenId=animal />}
      beforeComponent={<React.Fragment>
        <h2> {"This animal will launch in:"->React.string} </h2>
        <CountDown endDateMoment />
        <br />
        <br />
        <br />
        {if ratio == 0. {
          <p> {"The monthly pledge rate will be revealed at launch."->restr} </p>
        } else {
          <>
            <small>
              <strong>
                {"Monthly Pledge Rate:"->restr}
                <Rimble.Tooltip
                  message={"This is the monthly percentage contribution of " ++
                  (animalName ++
                  "'s sale price that will go towards conservation of at risk animals. This is deducted continuously from the deposit and paid by the owner of the animal")}
                  placement="top">
                  <span> {`ⓘ`->restr} </span>
                </Rimble.Tooltip>
              </strong>
            </small>
            <br />
            {(monthlyRate ++ " %")->restr}
          </>
        }}
      </React.Fragment>}
    />
  }
}

module AnimalInfo = {
  @react.component
  let make = (~chain, ~animal: TokenId.t) => {
    let animalDescription = \"||||"(QlHooks.useWildcardDescription(animal), ["Loading"])
    let optAnimalMedia = animal->Animal.useAlternateImage

    let animalStatus = animal->Animal.useTokenStatus(~chain)

    // TODO: the ethereum address is really terribly displayed. But the default in Rimble UI includes a QR code scanner (which is really ugly).
    // https://rimble.consensys.design/components/rimble-ui/EthAddress#props
    // https://github.com/ConsenSys/rimble-ui/blob/dd470f00374a05c860b558a2cb9317861e4a0d89/src/EthAddress/index.js (maybe make a PR here with some changes)
    <Rimble.Box m=5>
      <ReactTabs>
        <ReactTabs.TabList>
          <ReactTabs.Tab> {"Story"->React.string} </ReactTabs.Tab>
          <ReactTabs.Tab> {"Details"->React.string} </ReactTabs.Tab>
          {optAnimalMedia->mapd(React.null, _ =>
            <ReactTabs.Tab> {"Media"->React.string} </ReactTabs.Tab>
          )}
        </ReactTabs.TabList>
        <ReactTabs.TabPanel>
          <h2> {"Story"->React.string} </h2>
          <div
            className={
              open Css
              style(list{maxHeight(#em(26.)), overflow(#scroll)})
            }>
            {React.array(
              animalDescription->Array.mapWithIndex((i, paragraphText) =>
                <p key={i->string_of_int}> {paragraphText->React.string} </p>
              ),
            )}
          </div>
          {animal->TokenId.toString == "13"
          // Glen
            ? <a href="/#dao">
                <span
                  className={
                    open Css
                    style(list{color(hex("72c7d7"))})
                  }>
                  {"Vote for your favourite conservation"->restr}
                </span>
              </a>
            : React.null}
        </ReactTabs.TabPanel>
        // [@warning "-102"]
        <ReactTabs.TabPanel>
          {switch animalStatus {
          | Loading => <Rimble.Loader />
          | WaitingForLaunch(endDateMoment) => <UnlaunchedAnimalInfo chain endDateMoment animal />
          | Launched(auctionStartTime) =>
            <Info.Auction chain auctionStartTime abandoned=false tokenId=animal />
          | Foreclosed(auctionStartTime) =>
            <Info.Auction chain auctionStartTime abandoned=true tokenId=animal />
          | Owned(_) => <Info chain tokenId=animal />
          }}
        </ReactTabs.TabPanel>
        <ReactTabs.TabPanel>
          {optAnimalMedia->mapd(React.null, media =>
            <img
              className={
                open Css
                style(list{width(#percent(100.))})
              }
              src={CONSTANTS.cdnBase ++ media}
            />
          )}
        </ReactTabs.TabPanel>
      </ReactTabs>
    </Rimble.Box>
  }
}

@react.component
let make = (~currentAnimal) => {
  // let isGqlLoaded = QlStateManager.useIsInitialized();
  let isGqlLoaded = true
  let nonUrlRouting = RootProvider.useNonUrlState()
  let clearNonUrlState = RootProvider.useClearNonUrlState()
  let isDetailView = Router.useIsDetails()
  let optAnimalForDetails = Router.useAnimalForDetails()

  <Rimble.Flex flexWrap={isDetailView ? "wrap-reverse" : "wrap"} alignItems="start">
    <Rimble.Box p=1 width=[1., 1., 0.5]>
      <React.Fragment>
        {switch nonUrlRouting {
        | LoginScreen(_followOnAction) => <Login />
        | BuyScreen(animal) =>
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
            <Buy chain={Animal.getChainIdFromAnimalId(animal)} tokenId=animal />
          </div>
        | AuctionScreen(animal) =>
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
            <Buy chain={Animal.getChainIdFromAnimalId(animal)} tokenId=animal />
          </div>
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
            <UpdateDeposit
              chain={currentAnimal->Option.mapWithDefault(
                Client.MainnetQuery,
                Animal.getChainIdFromAnimalId,
              )}
              closeButtonText="Back to animal view"
            />
          </div>
        | UpdatePriceScreen(animal) =>
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
            <UpdatePrice chain={Animal.getChainIdFromAnimalId(animal)} tokenId=animal />
          </div>
        | NoExtraState =>
          switch optAnimalForDetails {
          | Some(animal) => <AnimalInfo chain={Animal.getChainIdFromAnimalId(animal)} animal />
          | None => <DefaultLeftPanel />
          }
        }}
      </React.Fragment>
    </Rimble.Box>
    <Rimble.Box
      p=1
      className={
        open Css
        style(list{media("(max-width: 831px)", list{overflow(#hidden)})})
      }
      width=[1., 1., 0.5]>
      <DefaultLook isGqlLoaded />
    </Rimble.Box>
  </Rimble.Flex>
}
