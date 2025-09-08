open Globals

// TODO: there must be a better way of importing images in reason react...
let betaBanner = "/img/beta-banner.png"

module AnimalFocusDetails = {
  @react.component
  let make = (~currentAnimal: option<TokenId.t>, ~showForwardBackButtons) => {
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
    let animalDetails = QlHooks.useDetailsPageNextPrevious(
      \"||||"(currentAnimal, TokenId.fromStringUnsafe("0")),
    )
    <div
      className={Cn.make(list{
        Styles.topBody,
        {
          open Css
          style(list{position(relative)})
        },
      })}>
      {currentAnimal->mapd(false, _ => true)
        ? <Rimble.Button.Text
            icononly=true
            icon="Close"
            color="black"
            className={
              open Css
              style(list{zIndex(10000)})
            }
            position="absolute"
            top=0
            right=0
            m=1
            size="large"
            onClick={_ => clearAndPush("#" ++ (showForwardBackButtons ? "" : "explorer"))}
          />
        : React.null}
      <Rimble.Flex flexWrap="wrap" alignItems="center">
        {showForwardBackButtons
          ? <div
              className={
                open Css
                style(list{width(#percent(5.))})
              }>
              <span
                className={Styles.carouselArrow(true, ~absolutePosition=false)}
                onClick={InputHelp.handleMouseEvent(() =>
                  clearAndPush("#details/" ++ animalDetails.prev->TokenId.toString)
                )}>
                {`◄`->React.string}
              </span>
            </div>
          : React.null}
        <div
          className={
            open Css
            style(list{
              width(showForwardBackButtons ? #percent(90.) : #percent(100.)),
              // style([width(`percent(animalCarousel->mapd(100., _ => 90.)))])
            })
          }>
          <Dapp currentAnimal />
        </div>
        {showForwardBackButtons
          ? <div
              className={
                open Css
                style(list{width(#percent(5.))})
              }>
              <span
                className={Styles.carouselArrow(false, ~absolutePosition=false) ++
                (" " ++
                {
                  open Css
                  style(list{width(#percent(3.))})
                })}
                onClick={InputHelp.handleMouseEvent(() =>
                  clearAndPush("#details/" ++ animalDetails.next->TokenId.toString)
                )}>
                {`►`->React.string}
              </span>
            </div>
          : React.null}
      </Rimble.Flex>
    </div>
  }
}

@react.component
let make = () => {
  let urlState = Router.useUrlState()

  // TODO: add code that scrolls to the correct height on mobile (to buy etc).
  // let ref = React.useRef(Js.Nullable.null);
  // ref.current.scrollHeight;
  // ReactDOMRe.Ref.domRef(ref);
  /* ref={ReactDOMRe.Ref.domRef(ref)} */

  let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()
  let isExplorer = Router.useIsExplorer()
  let isDetails = Router.useIsDetails()
  let isHome = Router.useIsHome()
  let translationModeContext = ReactTranslate.useTranslationModeContext()

  <div className=Styles.app>
    <div
      className={
        open Css
        style(list{minHeight(vh(88.))})
      }>
      <Announcement announcementBannerColor="72D6B5">
        <span>
          <a href="/#explorer/2nd-edition" className=AnimalAnnouncement.linkToAnimal>
            {"New wildcards"->restr}
          </a>
          {" launched on the "->React.string}
          <a
            href="https://matic.network"
            rel="noopener noreferrer"
            className=AnimalAnnouncement.linkToAnimal>
            {"MATIC network. "->restr}
          </a>
        </span>
      </Announcement>
      // <AnimalAnnouncement
      //   nextReleasedAnimals=[|TokenId.makeFromInt(25)|]
      //   announcementBannerColor="f49229" //next color
      //   // FFCD47 - Pendo
      //   // 2493AD - Star
      //   // D6564B - whacky
      //   // 0624a6 - Arthur
      //   // DE4C38
      //   // f49229
      //   // 2493AD
      //   // 72D6B5
      //   // AEE79A
      // />
      <div
        className={
          open Css
          style(list{position(relative)})
        }>
        <img src=betaBanner className=Styles.betaBanner />
      </div>
      <Header
        navItems=[
          {
            shouldDisplay: isHome,
            shouldDisplayMobile: false,
            component: (_, _) =>
              <div className=Styles.navListItemToggle>
                <span className=Styles.someMarginRight>
                  {(
                    translationModeContext.translationModeCrypto ? "EXPERT MODE" : "DEFAULT MODE"
                  )->restr}
                </span>
                <ReactSwitch
                  onChange=translationModeContext.setTranslationModeCrypto
                  checked=translationModeContext.translationModeCrypto
                  height=16
                  handleDiameter=18
                  width=30
                  onColor="#6BAD3F"
                  onHandleColor="#346D4C"
                  offHandleColor="#aaaaaa"
                  uncheckedIcon=false
                  checkedIcon=false
                  className=Styles.translationSwitch
                />
              </div>,
          },
          {
            shouldDisplay: !isHome,
            shouldDisplayMobile: !isHome,
            component: (closeModal, _) =>
              <a
                className=Styles.navListText
                href=""
                onClick={event => {
                  closeModal()
                  ReactEvent.Mouse.preventDefault(event)
                  clearAndPush("#")
                }}>
                {"HOME"->restr}
              </a>,
          },
          {
            shouldDisplay: true,
            shouldDisplayMobile: true,
            component: (closeModal, _) =>
              <a
                className=Styles.navListText
                onClick={event => {
                  closeModal()
                  ReactEvent.Mouse.preventDefault(event)
                  clearAndPush(j`/#leaderboards/monthly-contribution`)
                }}>
                {"LEADERBOARDS"->restr}
              </a>,
          },
          {
            shouldDisplay: true,
            shouldDisplayMobile: true,
            component: (closeModal, _) =>
              <a
                className=Styles.navListText
                onClick={event => {
                  closeModal()
                  ReactEvent.Mouse.preventDefault(event)
                  clearAndPush(j`/#dao`)
                }}>
                {"DAO"->restr}
              </a>,
          },
          {
            shouldDisplay: true,
            shouldDisplayMobile: true,
            component: (closeModal, _) =>
              <a
                className=Styles.navListText
                target="_blank"
                onClick={_ => closeModal()}
                rel="noopener noreferrer"
                href="https://blog.wildcards.world/">
                {"BLOG"->restr}
              </a>,
          },
          {
            shouldDisplay: !isExplorer || isDetails,
            shouldDisplayMobile: !isExplorer || isDetails,
            component: (closeModal, _) =>
              <div>
                <Rimble.Button
                  onClick={event => {
                    closeModal()
                    ReactEvent.Form.preventDefault(event)
                    clearAndPush("#explorer")
                  }}
                  className=Styles.whiteText>
                  {"VIEW WILDCARDS"->restr}
                </Rimble.Button>
              </div>,
          },
          {
            shouldDisplay: true,
            shouldDisplayMobile: true,
            component: (clickAction, _) => <Web3Connect clickAction />,
          },
          {
            shouldDisplay: true,
            shouldDisplayMobile: true,
            component: (clickAction, isMobile) => <ProfileIcon clickAction isMobile />,
          },
        ]
      />
      {switch urlState {
      | VotePage => <VotePage chain=Client.MainnetQuery />
      | Team => <Team />
      | IncreaseVoteIteration => React.null // DEPRECATED FUNCTIONALITY. // <IncreaseIterationPage />
      | Explorer(wildcardsEdition, subState) =>
        switch subState {
        | DetailView(currentAnimal) =>
          <AnimalFocusDetails currentAnimal showForwardBackButtons=false />
        | NormalView => <BuyGrid wildcardsEdition />
        }
      | Home(animalPageState) =>
        switch animalPageState {
        | DetailView(currentAnimal) =>
          <AnimalFocusDetails currentAnimal showForwardBackButtons=true />
        | NormalView =>
          <React.Fragment>
            <AnimalFocusDetails currentAnimal=None showForwardBackButtons=false />
            <FeaturedIn />
            <HomepageLeaderBoard />
            <CustomerBenefit />
            <HowItWorks />
            <EmailSignup />
            <FAQs />
            <Partners />
          </React.Fragment>
        }
      | User(userAddress) => <UserProfile chain=Client.MainnetQuery userAddress />
      | Artist(artistIdentifier) => <ArtistProfiles artistIdentifier />
      | Org(orgId) => <OrgProfile orgId />
      | Leaderboards(leaderboardType) =>
        <Rimble.Flex
          flexWrap="wrap"
          alignItems="center"
          className={
            open Css
            style(list{padding(em(2.))})
          }>
          <LeaderBoards leaderboardType />
        </Rimble.Flex>
      }}
    </div>
    <Footer />
  </div>
}
