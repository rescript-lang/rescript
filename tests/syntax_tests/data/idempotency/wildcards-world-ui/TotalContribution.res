open GqlConverters

module LoadMostContributed = %graphql(`
    query {
      patrons(first: 30, orderBy: totalContributed, orderDirection: desc, where: {id_not: "0x6d47cf86f6a490c6410fc082fd1ad29cf61492d0"}) {
        id
        patronTokenCostScaledNumerator  @ppxCustom(module: "BigInt")
        totalContributed @ppxCustom(module: "BigInt")
        lastUpdated @ppxCustom(module: "BigInt")
      }
    }
  `)

let useLoadMostContributed = () => Obj.magic
// ApolloHooks.useSubscription(LoadMostContributed.definition);
let useLoadMostContributedData = () => {
  let currentTimestamp = QlHooks.useCurrentTime()

  switch LoadMostContributed.use() {
  | {loading: true, _} => None
  | {error: Some(_error), _} => None
  | {data: Some({patrons}), _} =>
    patrons
    ->Array.map(patron => {
      let timeElapsed = BN.new_(currentTimestamp)->BN.sub(patron.lastUpdated)

      let amountContributedSinceLastUpdate =
        patron.patronTokenCostScaledNumerator
        ->BN.mul(timeElapsed) // A month with 30 days has 2592000 seconds
        ->BN.div(
          // BN.new_("1000000000000")->BN.mul( BN.new_("31536000")),
          BN.new_("31536000000000000000"),
        )

      let totalContributedWei = patron.totalContributed->BN.add(amountContributedSinceLastUpdate)

      (patron.id, totalContributedWei)
    })
    ->Js.Array2.sortInPlaceWith(((_, first), (_, second)) => second->BN.cmp(first))
    ->Some
  | _ => None
  }
}

open Css

let flameImg = "/img/streak-flame.png"
let goldTrophyImg = "/img/icons/gold-trophy.png"
let silverTrophyImg = "/img/icons/silver-trophy.png"
let bronzeTrophyImg = "/img/icons/bronze-trophy.png"

let leaderboardTable = style(list{
  width(#percent(100.)),
  tableLayout(#fixed),
  overflowWrap(#breakWord),
})

let leaderboardHeader = style(list{backgroundColor(#hex("73c7d7ff"))})

let streakTextLeaderboard = style(list{
  position(absolute),
  zIndex(100),
  bottom(#percent(-10.)),
  right(#percent(50.)),
  transform(translateX(#px(-5))),
})
let flameImgLeaderboard = style(list{width(#percent(100.)), maxWidth(px(50))})

let rankText = style(list{
  position(absolute),
  zIndex(100),
  bottom(#percent(-10.)),
  right(#percent(50.)),
  transform(translate(#px(-4), #px(-15))),
})

let trophyImg = style(list{width(#percent(100.)), width(px(50)), height(px(50))})

let centerFlame = style(list{display(block), margin(auto), width(#px(70)), position(relative)})

let rankMetric = style(list{fontSize(#px(16))})

let rankingColor = index =>
  style(list{backgroundColor(#hex(mod(index, 2) == 1 ? "b5b5bd22" : "ffffffff"))})

module ContributorsRow = {
  @react.component
  let make = (~contributor, ~amount, ~index) => {
    UserProvider.useUserInfoContext().update(contributor, false)

    let optThreeBoxData = UserProvider.use3BoxUserData(contributor)
    let optUserName = {
      open Belt.Option
      optThreeBoxData
      ->flatMap(threeBoxData => threeBoxData.profile)
      ->flatMap(threeBoxData => threeBoxData.name)
    }
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

    <tr key=contributor className={rankingColor(index)}>
      <td>
        <span className=centerFlame>
          {index == 0
            ? <img className=trophyImg src=goldTrophyImg />
            : index == 1
            ? <img className=trophyImg src=silverTrophyImg />
            : index == 2
            ? <img className=trophyImg src=bronzeTrophyImg />
            : <div className=trophyImg />}
          <p className=rankText>
            <strong> {"#"->React.string} {(index + 1)->string_of_int->React.string} </strong>
          </p>
        </span>
      </td>
      <td>
        <a
          onClick={e => {
            ReactEvent.Mouse.preventDefault(e)
            clearAndPush(j`/#user/$contributor`)
          }}>
          {switch optUserName {
          | Some(name) => <span> {name->React.string} </span>
          | None => <span> {Helper.elipsify(contributor, 20)->React.string} </span>
          }}
        </a>
      </td>
      //  <td>
      //    <span className=centerFlame>
      //      <img className=flameImgLeaderboard src=flameImg />
      //      <p className=streakTextLeaderboard>
      //        <strong> "1"->React.string </strong>
      //      </p>
      //    </span>
      //  </td>
      <td className=rankMetric> {(amount ++ " ETH")->React.string} </td>
    </tr>
  }
}

module MostContributed = {
  @react.component
  let make = (~highestContributors) =>
    React.array(
      Array.mapWithIndex(highestContributors, (index, (contributor, amount)) =>
        <ContributorsRow
          contributor amount={amount->Web3Utils.fromWeiBNToEthPrecision(~digits=4)} index
        />
      ),
    )
}

@react.component
let make = (~numberOfLeaders) => {
  let highestContributorsOpt = useLoadMostContributedData()

  <div>
    <Rimble.Heading> {"Wildcards Total Contribution Leaderboard"->React.string} </Rimble.Heading>
    <br />
    <Rimble.Table className=leaderboardTable>
      <thead className=leaderboardHeader>
        <tr>
          <th> {"Rank"->React.string} </th>
          <th> {"Guardian"->React.string} </th>
          // <th> "Longest Streak"->React.string </th>
          <th> {"Total Contribution"->React.string} </th>
        </tr>
      </thead>
      <tbody>
        {switch highestContributorsOpt {
        | Some(highestContributorsFull) =>
          let highestContributors = Belt.Array.slice(
            highestContributorsFull,
            ~offset=0,
            ~len=numberOfLeaders,
          )
          <MostContributed highestContributors />
        | None => React.null
        }}
      </tbody>
    </Rimble.Table>
  </div>
}
