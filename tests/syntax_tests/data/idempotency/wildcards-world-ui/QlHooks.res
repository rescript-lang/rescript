open Globals
module QueryFetchPolicy = ApolloClient__React_Hooks_UseQuery.WatchQueryFetchPolicy

type owner = {"address": Js.Json.t}

type price = {"price": Eth.t}

type wildcard = {
  id: string,
  tokenId: TokenId.t,
  patronageNumerator: Js.Json.t,
  owner: owner,
  price: price,
}

let decodeBN: Js.Json.t => BN.t = number =>
  number
  ->Js.Json.decodeString
  ->Belt.Option.mapWithDefault("0", a => a) /* trusting that gql will be reliable here */
  ->BN.new_

open GqlConverters

module InitialLoad = %graphql(`
       query($amount: Int!, $globalId: String!) {
         wildcards(first: $amount) {
           id
           animal: tokenId @ppxCustom(module: "GqlTokenId")
           owner {
             address
             id
           }
           price {
             price @ppxCustom(module: "Price")
             id
           }
           totalCollected @ppxCustom(module: "Price")
           timeCollected @ppxCustom(module: "BigInt")
           patronageNumeratorPriceScaled @ppxCustom(module: "BigInt")
           timeAcquired @ppxCustom(module: "GqlMoment")
           auctionStartPrice @ppxCustom(module: "BigInt")
           launchTime @ppxCustom(module: "BigInt")
         }
         global(id: $globalId) {
           id
           totalCollectedOrDueAccurate @ppxCustom(module: "BigInt")
           timeLastCollected @ppxCustom(module: "BigInt")
           totalTokenCostScaledNumeratorAccurate @ppxCustom(module: "BigInt")
           defaultAuctionLength @ppxCustom(module: "BigInt")
           defaultAuctionEndPrice @ppxCustom(module: "BigInt")
           defaultAuctionStartPrice @ppxCustom(module: "BigInt")
         }
       }
     `)

let createContext: Client.queryContext => Js.Json.t = Obj.magic

let useInitialDataLoad = (~chain) => {
  let initLoadQuery = InitialLoad.use(
    ~notifyOnNetworkStatusChange=true,
    ~fetchPolicy=QueryFetchPolicy.CacheFirst,
    ~context={context: chain}->createContext,
    InitialLoad.makeVariables(
      ~amount=switch chain {
      | Client.MaticQuery => 31
      | Client.Neither
      | Client.MainnetQuery => 30
      },
      ~globalId=switch chain {
      | Client.MaticQuery => "Matic-Global"
      | Client.Neither
      | Client.MainnetQuery => "1"
      },
      (),
    ),
  )

  switch initLoadQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data, _} => data
  }
}

let useAnimalList = (~chain) => {
  let allData = useInitialDataLoad(~chain)
  React.useMemo2(
    () => \"||||"(allData->oMap(data => data.wildcards->Array.map(wc => wc.animal)), []),
    (allData, chain),
  )
}

module SubWildcardQuery = %graphql(`
       query ($tokenId: String!) {
         wildcard(id: $tokenId) {
           id
           animal: tokenId @ppxCustom(module: "GqlTokenId")
           timeAcquired @ppxCustom(module: "GqlMoment")
           totalCollected @ppxCustom(module: "Price")
           patronageNumerator @ppxCustom(module: "BigInt")
           patronageNumeratorPriceScaled @ppxCustom(module: "BigInt")
           timeCollected @ppxCustom(module: "BigInt")
           # timeCollected @ppxCustom(module: "GqlMoment")
           price {
             id
             price @ppxCustom(module: "Price")
           }
           owner {
             address @ppxCustom(module: "GqlAddress")
             id
           }
           auctionStartPrice @ppxCustom(module: "BigInt")
           launchTime @ppxCustom(module: "BigInt")
         }
       }
     `)

module WildcardDataQuery = %graphql(`
    query ($tokenId: String!) {
      launchedWildcards_by_pk(id: $tokenId) {
        wildcard {
          id
          name
          description
          organization {
            name
            id
          }
          image
          real_wc_photos {
            image
            photographer
          }
          artistOfWildcard {
            name
            id
          }
        }
      }
    }
  `)

module MaticStateQuery = %graphql(`
    query ($address: String!, $network: String!) {
      maticState(address: $address, network: $network) {
        balance
        daiNonce
        error
        stewardNonce
      }
    }
  `)
module HomeAnimalsQuery = %graphql(`
    {
      homeAnimals {
        id
        next
        prev
        wildcardData {
          description
          id
          name
          organisationId
        }
      }
    }
  `)

module ArtistQuery = %graphql(`
    query ($artistIdentifier: String!) {
      artist_by_pk(id: $artistIdentifier) {
        eth_address
        id
        name
        website
        launchedWildcards: wildcardData (where: {id: { _is_null: false}}) {
          key
          id
          name
          image
          organization {
            id
            name
            logo
          }
        }
        unlaunchedWildcards: wildcardData (where: {id: { _is_null: true}}) {
          key
          name
          image
          organization {
            id
            name
            logo
          }
        }
      }
    }
  `)
// // NOTE: If multiple transactions happen in the same block they may get missed, maybe one day that will be a problem for us ;)
// module SubStateChangeEvents = [%graphql
//   {|
//        subscription {
//          stateChanges(first: 1, orderBy: timestamp, orderDirection: desc) {
//            id
//            timestamp
//            wildcardChanges {
//              id
//              tokenId
//              timeAcquired
//              totalCollected
//              patronageNumeratorPriceScaled
//              timeCollected
//              price {
//                id
//                price
//              }
//              owner {
//                address
//                id
//              }
//            }
//            patronChanges {
//              id
//              address
//              lastUpdated
//              # lastUpdated @ppxCustom(module: "GqlMoment")
//              previouslyOwnedTokens {
//                id
//              }
//              tokens {
//                id
//              }
//              availableDeposit
//              patronTokenCostScaledNumerator
//              foreclosureTime
//            }
//          }
//        }
//      |}
// ];

module LoadPatron = %graphql(`
       query ($patronId: String!) {
         patron(id: $patronId) {
           id
           previouslyOwnedTokens {
             id
           }
           tokens {
             id
           }
           availableDeposit  @ppxCustom(module: "Price")
           patronTokenCostScaledNumerator  @ppxCustom(module: "BigInt")
           foreclosureTime  @ppxCustom(module: "BigInt")
           address @ppxCustom(module: "GqlAddress")
           lastUpdated @ppxCustom(module: "BigInt")
           totalLoyaltyTokens @ppxCustom(module: "BigInt")
           totalLoyaltyTokensIncludingUnRedeemed @ppxCustom(module: "BigInt")
         }
       }
     `)

module LoadTokenDataArray = %graphql(`
        query ($wildcardIdArray: [String!]!) {
          wildcards (where: {id_in: $wildcardIdArray}) {
            # totalCollected
            # patronageNumeratorPriceScaled
            # timeCollected
            id
            totalCollected @ppxCustom(module: "Price")
            patronageNumeratorPriceScaled @ppxCustom(module: "BigInt")
            timeCollected @ppxCustom(module: "BigInt")
          }
        }
     `)

module LoadOrganisationData = %graphql(`
      query ($orgId: String!) {
        organisations_by_pk(id: $orgId) {
          description
          name
          website
          wildcard (where: {id: {_is_null: false}}) {
            id @ppxCustom(module: "GqlTokenIdStr")
          }
          unlaunched: wildcard(where: {id: {_is_null: true}, real_wc_photos: {image: {_is_null: false}}}) {
            key
            real_wc_photos {
              image
              photographer
            }
            name
            commonName
            description
          }
          logo
          logo_badge
          youtube_vid
        }
      }
     `)

module LoadTopContributors = %graphql(`
      query ($numberOfLeaders: Int!) {
        patrons(first: $numberOfLeaders, orderBy: patronTokenCostScaledNumerator, orderDirection: desc, where: {id_not: "NO_OWNER"}) {
          id
          patronTokenCostScaledNumerator  @ppxCustom(module: "BigInt")
        }
      }
  `)

module SubTotalRaisedOrDueQuery = %graphql(`
       query {
         global(id: "1") {
           id
           totalCollectedOrDueAccurate @ppxCustom(module: "BigInt")
           timeLastCollected @ppxCustom(module: "BigInt")
           totalTokenCostScaledNumeratorAccurate @ppxCustom(module: "BigInt")
         }
       }
     `)

let getQueryPrefix = (chain: Client.context) =>
  switch chain {
  | MainnetQuery
  | Neither => ""
  | MaticQuery => "matic"
  }

type data = {tokenId: string}

let useWildcardQuery = (~chain, tokenId) => {
  let wildcardQuery = SubWildcardQuery.use(
    ~context={context: chain}->createContext,
    SubWildcardQuery.makeVariables(~tokenId=chain->getQueryPrefix ++ tokenId->TokenId.toString, ()),
  )

  switch wildcardQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data, _} => data
  }
}

let useLoadTokenDataArrayQuery = (~chain, tokenIdArray) => {
  let tokenDataQuery = LoadTokenDataArray.use(
    ~context={context: chain}->createContext,
    LoadTokenDataArray.makeVariables(
      ~wildcardIdArray=tokenIdArray->Array.map(id => id->TokenId.toString),
      (),
    ),
  )
  switch tokenDataQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data, _} => data
  }
}
let useWildcardDataQuery = tokenId => {
  let wildcardQuery = WildcardDataQuery.use(
    WildcardDataQuery.makeVariables(~tokenId=tokenId->TokenId.toString, ()),
  )
  switch wildcardQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({launchedWildcards_by_pk}), _} => launchedWildcards_by_pk
  | {data: None, _} => None
  }
}

let useHomeAnimalsQuery = () =>
  switch HomeAnimalsQuery.use() {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({homeAnimals}), _} => Some(homeAnimals)
  | {data: None, _} => None
  }

let useLoadOrganisationQuery = orgId => {
  let orgQuery = LoadOrganisationData.use(LoadOrganisationData.makeVariables(~orgId, ()))
  switch orgQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({organisations_by_pk}), _} => organisations_by_pk
  | {data: None, _} => None
  }
}
let useLoadOrganisationLogo = orgId =>
  switch useLoadOrganisationQuery(orgId) {
  | Some({logo, _}) => Some(logo)
  | None => None
  }
let useLoadOrganisationLogoBadge = orgId =>
  switch useLoadOrganisationQuery(orgId) {
  | Some({logo_badge: Some(badge), _}) => Some(badge)
  | Some({logo, _}) => Some(logo)
  | None => None
  }

type homePageAnimal = {
  id: TokenId.t,
  prev: TokenId.t,
  next: TokenId.t,
}

let useHomePageAnimalArray = () =>
  switch useHomeAnimalsQuery() {
  | Some(homeAnimals) =>
    homeAnimals->Array.map(animal => {
      id: TokenId.fromStringUnsafe(animal.id),
      prev: TokenId.fromStringUnsafe(animal.prev),
      next: TokenId.fromStringUnsafe(animal.next),
    })
  | None => []
  }
let useDetailsPageNextPrevious = (currentToken: TokenId.t) => {
  let homepageAnimalData = useHomePageAnimalArray()
  let defaultValue = {
    id: TokenId.fromStringUnsafe("2"),
    next: TokenId.fromStringUnsafe("1"),
    prev: TokenId.fromStringUnsafe("0"),
  }
  let forwardNextLookup = React.useMemo1(() =>
    homepageAnimalData->Array.reduce(Js.Dict.empty(), (dict, item) => {
      dict->Js.Dict.set(item.id->TokenId.toString, item)
      dict
    })
  , [homepageAnimalData])

  \"||||"(forwardNextLookup->Js.Dict.get(currentToken->TokenId.toString), defaultValue)
}

@decco.decode
type animalDescription = array<string>
let useWildcardDescription = tokenId =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {description, _}, _}) =>
    description
    ->animalDescription_decode
    ->Belt.Result.mapWithDefault(None, descriptionArray => Some(descriptionArray))
  | None => None
  }

let useWildcardName = tokenId =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {name, _}, _}) => name
  | None => None
  }
let useWildcardAvatar = tokenId =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {image, _}, _}) => image
  | None => None
  }
let useWildcardArtist = tokenId =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {artistOfWildcard, _}, _}) => artistOfWildcard
  | None => None
  }
let useRealImages = tokenId =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {real_wc_photos, _}, _}) => Some(real_wc_photos)
  | None => None
  }
let useWildcardOrgId = (~tokenId) =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {organization: Some({id, _}), _}, _}) => Some(id)
  | _ => None
  }
let useWildcardOrgName = (~tokenId) =>
  switch useWildcardDataQuery(tokenId) {
  | Some({wildcard: {organization: Some({name, _}), _}, _}) => Some(name)
  | _ => None
  }

let useLoadTopContributors = numberOfLeaders => {
  let topContributorsQuery = LoadTopContributors.use(
    LoadTopContributors.makeVariables(~numberOfLeaders, ()),
  )
  switch topContributorsQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({patrons}), _} => Some(patrons)
  | {data: None, _} => None
  }
}
let useLoadTopContributorsData = numberOfLeaders =>
  switch useLoadTopContributors(numberOfLeaders) {
  | Some(topContributors) =>
    topContributors
    ->Array.map(patron => {
      let monthlyContribution =
        patron.patronTokenCostScaledNumerator
        ->BN.mul(BN.new_("2592000")) // A month with 30 days has 2592000 seconds
        ->BN.div(
          // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
          BN.new_("31536000000000000000"),
        )
        ->Web3Utils.fromWeiBNToEthPrecision(~digits=4)
      (patron.id, monthlyContribution)
    })
    ->Some
  | None => None
  }
let usePatron: (~chain: Client.context, TokenId.t) => option<string> = (~chain, animal) =>
  switch useWildcardQuery(~chain, animal) {
  | Some({wildcard: Some({owner: {address, _}, _})}) => Some(address)
  | Some({wildcard: None})
  | None =>
    None
  }

let useIsAnimalOwened = (~chain, ownedAnimal) => {
  let currentAccount = RootProvider.useCurrentUser()->Belt.Option.mapWithDefault("loading", a => a)

  let currentPatron =
    usePatron(~chain, ownedAnimal)->Belt.Option.mapWithDefault("no-patron-defined", a => a)

  currentAccount->Js.String.toLowerCase == currentPatron->Js.String.toLocaleLowerCase
}

let useTimeAcquired: (~chain: Client.context, TokenId.t) => option<MomentRe.Moment.t> = (
  ~chain,
  animal,
) =>
  switch useWildcardQuery(~chain, animal) {
  | Some({wildcard: Some({timeAcquired, _})}) => Some(timeAcquired)
  | Some({wildcard: None})
  | None =>
    None
  }

let useQueryPatron = (~chain, patron) => {
  let loadPatronQuery = LoadPatron.use(
    ~context={context: chain}->createContext,
    LoadPatron.makeVariables(~patronId=chain->getQueryPrefix ++ patron, ()),
  )
  switch loadPatronQuery {
  | {data: Some({patron}), _} => patron
  | {data: None, _} => None
  // | {error: Some(_error), _} => None
  // | {loading: true, _} => None
  }
}

let useForeclosureTimeBn = (~chain, patron) =>
  switch useQueryPatron(~chain, patron) {
  | Some({foreclosureTime, _}) => Some(foreclosureTime)
  | None => None
  }

let useForeclosureTime = (~chain, patron) =>
  useForeclosureTimeBn(~chain, patron)->Option.map(Helper.bnToMoment)

let useTimeAcquiredWithDefault = (~chain, animal, default: MomentRe.Moment.t) =>
  \"||||"(useTimeAcquired(~chain, animal), default)
let useDaysHeld = (~chain, tokenId) =>
  useTimeAcquired(~chain, tokenId)->oMap(moment => (
    MomentRe.diff(MomentRe.momentNow(), moment, #days),
    moment,
  ))
let useTotalCollectedOrDue = () => {
  let subTotalRaisedQuery = SubTotalRaisedOrDueQuery.use()
  switch subTotalRaisedQuery {
  | {error: Some(_error), _} => None
  | {data: None, _} => None
  | {data: Some({global}), _} => global
  }
}

let getCurrentTimestamp = () => string_of_int(Js.Math.floor(Js.Date.now() /. 1000.))

let useCurrentTime = () => {
  let (currentTime, setTimeLeft) = React.useState(() => getCurrentTimestamp())

  React.useEffect1(() => {
    let interval = Js.Global.setInterval(() => setTimeLeft(_ => getCurrentTimestamp()), 2000)
    Some(() => Js.Global.clearInterval(interval))
  }, [setTimeLeft])
  currentTime
}
let useCurrentTimestampBn = () => useCurrentTime()->BN.new_
let useAmountRaised = () => {
  let currentTimestamp = useCurrentTime()

  useTotalCollectedOrDue()->oMap(({
    totalCollectedOrDueAccurate,
    timeLastCollected,
    totalTokenCostScaledNumeratorAccurate,
    _,
  }) => {
    let timeElapsed = BN.new_(currentTimestamp)->BN.sub(timeLastCollected)

    let amountRaisedSinceLastCollection =
      totalTokenCostScaledNumeratorAccurate
      ->BN.mul(timeElapsed)
      ->BN.div(
        // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
        BN.new_("31536000000000000000"),
      )
    totalCollectedOrDueAccurate->BN.add(amountRaisedSinceLastCollection)
  })
}

let useTotalCollectedToken: (~chain: Client.context, TokenId.t) => option<(Eth.t, BN.t, BN.t)> = (
  ~chain,
  animal,
) =>
  switch useWildcardQuery(~chain, animal) {
  | Some({wildcard: Some({totalCollected, timeCollected, patronageNumeratorPriceScaled, _})}) =>
    Some((totalCollected, timeCollected, patronageNumeratorPriceScaled))
  | Some({wildcard: None})
  | None =>
    None
  }

let usePatronageNumerator = (~chain, tokenId: TokenId.t) =>
  switch useWildcardQuery(~chain, tokenId) {
  | Some({wildcard: Some({patronageNumerator, _})}) => Some(patronageNumerator)
  | Some({wildcard: None})
  | None =>
    None
  }

// TODO: fix this, this is a hardcoded pledgerate. It should be fetched from the graph!
let usePledgeRate = (~chain, tokenId) => {
  let optPatronageNumerator = usePatronageNumerator(~chain, tokenId)
  React.useMemo1(() =>
    switch optPatronageNumerator {
    | Some(patronageNumerator) =>
      let result = \"|/|"(patronageNumerator, BN.new_("12000000000"))
      result->BN.toNumberFloat /. 1000.
    | None => 0.
    }
  , [optPatronageNumerator])
}

let usePledgeRateDetailed = (~chain, tokenId) => {
  let pledgeRate = usePledgeRate(~chain, tokenId)
  let inversePledgeRate = 1. /. pledgeRate
  let numeratorOverYear = (pledgeRate *. 1200.)->Float.toInt->string_of_int
  (numeratorOverYear, "100", pledgeRate, inversePledgeRate)
}

type patronLoyaltyTokenDetails = {
  currentLoyaltyTokens: Eth.t,
  currentLoyaltyTokensIncludingUnredeemed: Eth.t,
  lastCollected: BN.t,
  numberOfAnimalsOwned: BN.t,
}
let usePatronLoyaltyTokenDetails = (~chain, address) =>
  switch useQueryPatron(~chain, address) {
  | Some(patron) =>
    Some({
      currentLoyaltyTokens: patron.totalLoyaltyTokens,
      currentLoyaltyTokensIncludingUnredeemed: patron.totalLoyaltyTokensIncludingUnRedeemed,
      lastCollected: patron.lastUpdated,
      numberOfAnimalsOwned: BN.new_(patron.tokens->Obj.magic->Array.length->string_of_int),
    })
  | _ => None
  }

// TODO:: Take min of total deposit and amount raised
let useAmountRaisedToken: (~chain: Client.context, TokenId.t) => option<Eth.t> = (
  ~chain,
  animal,
) => {
  let currentTimestamp = useCurrentTime()

  switch useTotalCollectedToken(~chain, animal) {
  | Some((amountCollectedOrDue, timeCalculated, patronageNumeratorPriceScaled)) =>
    let timeElapsed = BN.new_(currentTimestamp)->BN.sub(timeCalculated)

    let amountRaisedSinceLastCollection =
      patronageNumeratorPriceScaled
      ->BN.mul(timeElapsed)
      ->BN.div(
        // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
        BN.new_("31536000000000000000"),
      )

    Some(amountCollectedOrDue->BN.add(amountRaisedSinceLastCollection))
  | None => None
  }
}
let calculateTotalRaised = (
  currentTimestamp,
  (amountCollectedOrDue, timeCalculated, patronageNumeratorPriceScaled),
) => {
  let timeElapsed = BN.new_(currentTimestamp)->BN.sub(timeCalculated)

  let amountRaisedSinceLastCollection =
    patronageNumeratorPriceScaled
    ->BN.mul(timeElapsed)
    ->BN.div(
      // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
      BN.new_("31536000000000000000"),
    )

  amountCollectedOrDue->BN.add(amountRaisedSinceLastCollection)
}
let useTotalRaisedAnimalGroup: array<TokenId.t> => (option<Eth.t>, option<Eth.t>) = animals => {
  let currentTimestamp = useCurrentTime()

  let detailsMainnet = useLoadTokenDataArrayQuery(~chain=Client.MainnetQuery, animals)
  let detailsMatic = useLoadTokenDataArrayQuery(
    ~chain=Client.MaticQuery,
    animals->Array.map(id => ("matic" ++ id->Obj.magic)->Obj.magic),
  )

  (
    switch detailsMainnet {
    | Some(detailsArray) =>
      Some(
        detailsArray.wildcards->Array.reduce(BN.new_("0"), (acc, animalDetails) =>
          \"|+|"(
            calculateTotalRaised(
              currentTimestamp,
              (
                animalDetails.totalCollected,
                animalDetails.timeCollected,
                animalDetails.patronageNumeratorPriceScaled,
              ),
            ),
            acc,
          )
        ),
      )
    | None => None
    },
    switch detailsMatic {
    | Some(detailsArray) =>
      Some(
        detailsArray.wildcards->Array.reduce(BN.new_("0"), (acc, animalDetails) =>
          \"|+|"(
            calculateTotalRaised(
              currentTimestamp,
              (
                animalDetails.totalCollected,
                animalDetails.timeCollected,
                animalDetails.patronageNumeratorPriceScaled,
              ),
            ),
            acc,
          )
        ),
      )
    | None => None
    },
  )
}

let useTimeSinceTokenWasLastSettled: (~chain: Client.context, TokenId.t) => option<BN.t> = (
  ~chain,
  animal,
) => {
  let currentTimestamp = useCurrentTime()

  switch useTotalCollectedToken(~chain, animal) {
  | Some((_, timeCalculated, _)) =>
    let timeElapsed = BN.new_(currentTimestamp)->BN.sub(timeCalculated)

    Some(timeElapsed)
  | None => None
  }
}

let useUnredeemedLoyaltyTokenDueForUser: (
  ~chain: Client.context,
  TokenId.t,
  int,
) => option<Eth.t> = (~chain, animal, numberOfTokens) =>
  switch useTimeSinceTokenWasLastSettled(~chain, animal) {
  | Some(timeSinceTokenWasLastSettled) =>
    let totalLoyaltyTokensPerSecondPerAnimal = BN.new_("11574074074074")
    let totalLoyaltyTokensForAllAnimals = \"|*|"(
      \"|*|"(timeSinceTokenWasLastSettled, totalLoyaltyTokensPerSecondPerAnimal),
      BN.newInt_(numberOfTokens),
    )
    Some(totalLoyaltyTokensForAllAnimals)
  | None => None
  }
let useTotalLoyaltyToken: (~chain: Client.context, Web3.ethAddress) => option<(Eth.t, Eth.t)> = (
  ~chain,
  patron,
) => {
  let currentTimestamp = useCurrentTime()

  switch usePatronLoyaltyTokenDetails(~chain, patron) {
  | Some({
      currentLoyaltyTokens,
      currentLoyaltyTokensIncludingUnredeemed,
      lastCollected,
      numberOfAnimalsOwned,
    }) =>
    let timeElapsed = \"|-|"(BN.new_(currentTimestamp), lastCollected)
    // Reference: https://github.com/wild-cards/contracts-private/blob/v2testing/migrations/7_receipt_tokens.js#L6
    let totalLoyaltyTokensPerSecondPerAnimal = BN.new_("11574074074074")
    let totalLoyaltyTokensFor1Animal = \"|*|"(totalLoyaltyTokensPerSecondPerAnimal, timeElapsed)
    let totalLoyaltyTokensForAllAnimals = \"|*|"(numberOfAnimalsOwned, totalLoyaltyTokensFor1Animal)
    let totalLoyaltyTokensForUser = \"|+|"(
      currentLoyaltyTokensIncludingUnredeemed,
      totalLoyaltyTokensForAllAnimals,
    )
    Some((totalLoyaltyTokensForUser, currentLoyaltyTokens))
  | None => None
  }
}

// TODO:: Take min of total deposit and amount raised
let useRemainingDepositEth: (~chain: Client.context, string) => option<Eth.t> = (
  ~chain,
  patron,
) => {
  let currentTimestamp = useCurrentTime()

  switch useQueryPatron(~chain, patron) {
  | Some({availableDeposit, lastUpdated, patronTokenCostScaledNumerator, _}) =>
    let timeElapsed = BN.new_(currentTimestamp)->BN.sub(lastUpdated)

    let amountRaisedSinceLastCollection =
      patronTokenCostScaledNumerator
      ->BN.mul(timeElapsed)
      ->BN.div(
        // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
        BN.new_("31536000000000000000"),
      )
    Some(availableDeposit->BN.sub(amountRaisedSinceLastCollection))
  | None => None
  }
}

type animalPrice =
  | Foreclosed(BN.t)
  | Price(Eth.t)
  | Loading

let usePrice: (~chain: Client.context, TokenId.t) => animalPrice = (~chain, tokenId) => {
  let wildcardData = useWildcardQuery(~chain, tokenId)
  let optCurrentPatron = usePatron(~chain, tokenId)
  let currentPatron = optCurrentPatron->Belt.Option.mapWithDefault("no-patron-defined", a => a)
  let currentTime = useCurrentTime()
  let foreclosureTime = useForeclosureTimeBn(~chain, currentPatron)

  switch (wildcardData, optCurrentPatron, foreclosureTime) {
  | (Some({wildcard: Some({price: {price, _}, _})}), Some(_currentPatron), Some(foreclosureTime)) =>
    if foreclosureTime->BN.lt(currentTime->BN.new_) {
      Foreclosed(foreclosureTime)
    } else {
      Price(price)
    }
  | (Some({wildcard: Some({price: {price, _}, _})}), Some(_currentPatron), None) => Price(price)
  | _ => Loading
  }
}

let useIsForeclosed = (~chain, currentPatron) => {
  let optAvailableDeposit = useRemainingDepositEth(~chain, currentPatron)

  optAvailableDeposit->Option.mapWithDefault(true, availableDeposit =>
    !(availableDeposit->BN.gt(BN.new_("0")))
  )
}

let useAuctionStartPrice = (~chain, _tokenId: TokenId.t) => {
  let optData = useInitialDataLoad(~chain)

  optData
  ->Option.flatMap(data => data.global)
  ->Option.map(global => global.defaultAuctionStartPrice)
}
let useAuctionEndPrice = (~chain, _tokenId: TokenId.t) => {
  let optData = useInitialDataLoad(~chain)

  optData->Option.flatMap(data => data.global)->Option.map(global => global.defaultAuctionEndPrice)
}
let useAuctioLength = (~chain, _tokenId: TokenId.t) => {
  let optData = useInitialDataLoad(~chain)

  optData->Option.flatMap(data => data.global)->Option.map(global => global.defaultAuctionLength)
}
let useLaunchTimeBN = (~chain, tokenId: TokenId.t) =>
  switch useWildcardQuery(~chain, tokenId) {
  | Some({wildcard: Some({launchTime, _})}) => Some(launchTime)
  | Some({wildcard: None})
  | None =>
    None
  }

let useMaticState = (~forceRefetch, address, network) => {
  let query = MaticStateQuery.use(
    ~fetchPolicy=forceRefetch ? QueryFetchPolicy.CacheAndNetwork : QueryFetchPolicy.CacheFirst,
    MaticStateQuery.makeVariables(~address, ~network, ()),
  )
  switch query {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({maticState}), _} => Some(maticState)
  | _ => None
  }
}

let useArtistData = (~artistIdentifier) => {
  let artistQuery = ArtistQuery.use(ArtistQuery.makeVariables(~artistIdentifier, ()))

  switch artistQuery {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({artist_by_pk}), _} => artist_by_pk
  | _ => None
  }
}

let useArtistEthAddress = (~artistIdentifier) => {
  let artistData = useArtistData(~artistIdentifier)
  artistData->Option.flatMap(data => data.eth_address)
}
let useArtistName = (~artistIdentifier) => {
  let artistData = useArtistData(~artistIdentifier)
  artistData->Option.map(data => data.name)
}
let useArtistWebsite = (~artistIdentifier) => {
  let artistData = useArtistData(~artistIdentifier)
  artistData->Option.flatMap(data => data.website)
}
let useArtistLaunchedWildcards = (~artistIdentifier) => {
  let artistData = useArtistData(~artistIdentifier)
  artistData->Option.map(data => data.launchedWildcards)
}
let useArtistUnlaunchedWildcards = (~artistIdentifier) => {
  let artistData = useArtistData(~artistIdentifier)
  artistData->Option.map(data => data.unlaunchedWildcards)
}
type wildcardKey = int
type artistOrg = {
  id: string,
  name: string,
  logo: string,
  wildcards: array<wildcardKey>,
}
let useArtistOrgs = (~artistIdentifier) => {
  let artistData = useArtistData(~artistIdentifier)
  artistData->Option.map(data => {
    let dict = Js.Dict.empty()
    data.launchedWildcards
    ->Array.map(wildcard =>
      switch wildcard.organization {
      | Some(org) =>
        let orgId = org.id
        switch dict->Js.Dict.get(orgId) {
        | Some(orgObj) =>
          let newOrgObj = {
            ...orgObj,
            wildcards: orgObj.wildcards->Array.concat([wildcard.key]),
          }
          dict->Js.Dict.set(orgId, newOrgObj)
        | None =>
          dict->Js.Dict.set(
            orgId,
            {
              id: orgId,
              name: org.name,
              logo: org.logo,
              wildcards: [wildcard.key],
            },
          )
        }

      | None => ()
      }
    )
    ->ignore
    dict->Js.Dict.values
  })
}
