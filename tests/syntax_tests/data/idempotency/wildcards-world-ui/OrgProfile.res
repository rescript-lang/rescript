open Globals

@decco.decode
type orgDescriptionArray = array<string>

module YoutubeVid = {
  @module("./StaticContent//YoutubeVideo.js") @react.component
  external make: (~videoCode: string) => React.element = "default"
}

module ComingSoonAnimal = {
  @react.component
  let make = (~image, ~onClick) =>
    <div
      className={
        open Css
        style(list{width(#percent(32.))})
      }>
      <img
        className={
          open Css
          style(list{width(#percent(100.))})
        }
        onClick={_e => onClick()}
        src=image
      />
    </div>
}

module ImageCarousel = {
  @react.component
  let make = (
    ~orgComingSoon: array<
      QlHooks.LoadOrganisationData.LoadOrganisationData_inner.t_organisations_by_pk_unlaunched,
    >,
    ~selectedIndex,
  ) =>
    <ResponsiveCarousel
      showArrows=true
      showStatus=true
      showIndicators=true
      infiniteLoop=true
      showThumbs=false
      useKeyboardArrows=true
      autoPlay=true
      stopOnHover=true
      swipeable=true
      dynamicHeight=true
      emulateTouch=true
      selectedItem=selectedIndex>
      {React.array(
        orgComingSoon->Array.mapWithIndex((key, animal) =>
          animal.real_wc_photos[0]->Option.mapWithDefault(React.null, photos =>
            <img key={key->string_of_int} src={CONSTANTS.cdnBase ++ photos.image} />
          )
        ),
      )}
    </ResponsiveCarousel>
}

module ComingSoonModal = {
  @react.component
  let make = (
    ~selectedComingSoonAnimal,
    ~setSelectedComingSoonAnimal,
    ~orgComingSoon: array<
      QlHooks.LoadOrganisationData.LoadOrganisationData_inner.t_organisations_by_pk_unlaunched,
    >,
  ) => {
    let (openImage, setOpenImage) = React.useState(_ => None)

    <Rimble.Modal isOpen={selectedComingSoonAnimal->Option.isSome}>
      <Rimble.Card width=Rimble.AnyStr("80vw") p=0>
        <Rimble.Button.Text
          icononly=true
          icon="Close"
          color="moon-gray"
          position="absolute"
          right=0
          m=1
          top=0
          onClick={_ =>
            switch openImage {
            | Some(_) => setOpenImage(_ => None)
            | None => setSelectedComingSoonAnimal(_ => None)
            }}
        />
        <Rimble.Box p=1 mb=1>
          {switch selectedComingSoonAnimal {
          | Some(selectedComingSoonAnimal) =>
            switch openImage {
            | Some(selectedIndex) => <ImageCarousel orgComingSoon selectedIndex />
            | None =>
              let animal = orgComingSoon->Array.getUnsafe(selectedComingSoonAnimal)

              <div>
                <h3
                  className={
                    open Css
                    style(list{textAlign(center)})
                  }>
                  {(animal.name->Option.getWithDefault("Unamed") ++
                    animal.commonName->Option.mapWithDefault("", commonName => " - " ++ commonName))
                    ->React.string}
                </h3>
                <Rimble.Flex flexWrap="wrap" alignItems="start" alignContent="space-arround">
                  <Rimble.Box
                    width=[1., 1., 0.49]
                    className={
                      open Css
                      style(list{textAlign(#center), alignSelf(center), padding(em(0.5))})
                    }>
                    <div
                      className={
                        open Css
                        style(list{maxHeight(#em(26.)), overflow(#scroll)})
                      }>
                      {React.array(
                        animal.description
                        ->QlHooks.animalDescription_decode
                        ->Belt.Result.getWithDefault([])
                        ->Array.mapWithIndex((i, paragraphText) =>
                          <p key={i->string_of_int}> {paragraphText->React.string} </p>
                        ),
                      )}
                    </div>
                  </Rimble.Box>
                  <Rimble.Box
                    width=[1., 1., 0.45]
                    className={
                      open Css
                      style(list{textAlign(#center), alignSelf(center), padding(em(2.))})
                    }>
                    <div
                      className={
                        open Css
                        style(list{maxHeight(#vh(80.))})
                      }>
                      <PhotoGallery
                        onClick={(_, photoData) => setOpenImage(_ => Some(photoData.index))}
                        targetRowHeight=30
                        photos={animal.real_wc_photos->Array.map(photo => {
                          open PhotoGallery
                          {
                            src: CONSTANTS.cdnBase ++ photo.image,
                            width: 4,
                            height: 3,
                          }
                        })}
                      />
                    </div>
                  </Rimble.Box>
                </Rimble.Flex>
              </div>
            }
          | None => React.null
          }}
        </Rimble.Box>
      </Rimble.Card>
    </Rimble.Modal>
  }
}

module OrgPage = {
  @react.component
  let make = (
    ~orgData: QlHooks.LoadOrganisationData.LoadOrganisationData_inner.t_organisations_by_pk,
    ~orgId,
  ) => {
    let orgName = orgData.name
    let orgDescription = orgData.description->orgDescriptionArray_decode
    let orgAnimals = orgData.wildcard
    let orgComingSoon = orgData.unlaunched
    let (selectedComingSoonAnimal, setSelectedComingSoonAnimal) = React.useState(() => None)
    let orgAnimalsArray =
      orgAnimals->Array.map(animal => animal.id->Option.getWithDefault(TokenId.makeFromInt(99999)))
    let currentUsdEthPrice = UsdPriceProvider.useUsdPrice()
    let (totalCollectedMainnetEth, totalCollectMaticDai) = QlHooks.useTotalRaisedAnimalGroup(
      orgAnimalsArray,
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

    let orgWebsite = orgData.website
    let optOrgYoutubeVid = orgData.youtube_vid
    let orgImage = Animal.useGetOrgImage(orgId)

    <div>
      <ComingSoonModal selectedComingSoonAnimal setSelectedComingSoonAnimal orgComingSoon />
      <div
        className={
          open Css
          style(list{width(#percent(100.))})
        }>
        <Rimble.Flex flexWrap="wrap" alignItems="start" alignContent="space-arround">
          <Rimble.Box
            width=[1., 1., 0.3333]
            className={
              open Css
              style(list{textAlign(#center), alignSelf(center), padding(em(2.))})
            }>
            <a
              className={Cn.make(list{
                Styles.navListText,
                {
                  open Css
                  style(list{fontSize(em(3.))})
                },
              })}
              target="_blank"
              rel="noopener noreferrer"
              href=orgWebsite>
              <img
                className={
                  open Css
                  style(list{
                    // borderRadius(`percent(100.)),
                    width(#vh(25.)),
                    height(#vh(25.)),
                    objectFit(#contain),
                    // objectFit(`cover),
                  })
                }
                src=orgImage
              />
            </a>
            <br />
            <a
              className={Cn.make(list{
                Styles.navListText,
                {
                  open Css
                  style(list{fontSize(em(3.))})
                },
              })}
              target="_blank"
              rel="noopener noreferrer"
              href=orgWebsite>
              {orgName->restr}
            </a>
            <br />
            <div
              className={
                open Css
                style(list{maxHeight(#em(15.)), overflow(#scroll)})
              }>
              {switch orgDescription {
              | Ok(descriptionArray) =>
                React.array(
                  descriptionArray->Array.mapWithIndex((i, paragraphText) =>
                    <p key={i->string_of_int}> {paragraphText->React.string} </p>
                  ),
                )
              | Error(_) => <p> {"error loading description"->restr} </p>
              }}
            </div>
            <br />
          </Rimble.Box>
          <Rimble.Box
            width=[1., 1., 0.3333]
            className={
              open Css
              style(list{alignSelf(center), padding(em(2.))})
            }>
            {switch optOrgYoutubeVid {
            | Some(videoCode) => <YoutubeVid videoCode />
            | None => React.null
            }}
            <h2> {"Total Raised"->restr} </h2>
            {(totalPatronageUsd ++ "USD")->restr}
            <br />
            <small> {totalBreakdownString->restr} </small>
          </Rimble.Box>
          <Rimble.Box
            width=[1., 1., 0.3333]
            className={
              open Css
              style(list{alignSelf(center), padding(em(2.))})
            }>
            {switch orgAnimals {
            | [] => <p> {"This organisation doesn't have any wildcards yet"->React.string} </p>
            | uniquePreviouslyOwnedTokens =>
              <React.Fragment>
                <Rimble.Heading> {"Organisations animals"->React.string} </Rimble.Heading>
                <Rimble.Flex flexWrap="wrap" className=UserProfile.centreAlignOnMobile>
                  {React.array(
                    uniquePreviouslyOwnedTokens->Array.map(animal => {
                      let tokenId = animal.id->Option.getWithDefault(TokenId.makeFromInt(99999))
                      <UserProfile.Token key={tokenId->TokenId.toString} tokenId />
                    }),
                  )}
                </Rimble.Flex>
              </React.Fragment>
            }}
            {switch orgComingSoon {
            | [] => React.null
            | _orgAnimals =>
              <React.Fragment>
                <Rimble.Heading> {"Coming soon"->React.string} </Rimble.Heading>
                <Rimble.Flex flexWrap="wrap" className=UserProfile.centreAlignOnMobile>
                  React.null
                  {React.array(
                    orgComingSoon->Array.mapWithIndex((key, animal) =>
                      animal.real_wc_photos[0]->Option.mapWithDefault(React.null, photos =>
                        <ComingSoonAnimal
                          key={key->string_of_int}
                          image={CONSTANTS.cdnBase ++ photos.image}
                          onClick={() => setSelectedComingSoonAnimal(_ => Some(key))}
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
    </div>
    //  {animal->Array.map(animal => animal##)}
  }
}

@react.component
let make = (~orgId: string) => {
  let orgData = QlHooks.useLoadOrganisationQuery(orgId)

  <Rimble.Flex flexWrap="wrap" alignItems="center" className=Styles.topBody>
    {switch orgData {
    | Some(organisations_by_pk) =>
      let orgData = organisations_by_pk
      <OrgPage orgId orgData />
    | None =>
      <div>
        <Rimble.Heading> {"Loading Organisation Profile"->React.string} </Rimble.Heading>
        <Rimble.Loader />
      </div>
    }}
  </Rimble.Flex>
}
