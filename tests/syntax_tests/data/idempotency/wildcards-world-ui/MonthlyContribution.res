// Todo
// - Streak Score
// - Move to components

open Css
open Globals

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

module ContributorName = {
  @react.component
  let make = (~contributor) => {
    UserProvider.useUserInfoContext().update(contributor, false)

    let optThreeBoxData = UserProvider.use3BoxUserData(contributor)
    let optUserName = {
      open Belt.Option
      optThreeBoxData
      ->flatMap(threeBoxData => threeBoxData.profile)
      ->flatMap(threeBoxData => threeBoxData.name)
    }
    let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

    <span>
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
    </span>
  }
}

@react.component
let make = (~numberOfLeaders) => {
  let topContributorsOpt = QlHooks.useLoadTopContributorsData(numberOfLeaders)

  <div>
    <Rimble.Heading> {"Wildcards Monthly Contribution Leaderboard"->React.string} </Rimble.Heading>
    <br />
    <Rimble.Table className=leaderboardTable>
      <thead className=leaderboardHeader>
        <tr>
          <th> {"Rank"->React.string} </th>
          <th> {"Guardian"->React.string} </th>
          // <th> "Longest Streak"->React.string </th>
          <th> {"Monthly Contribution"->React.string} </th>
        </tr>
      </thead>
      <tbody>
        {switch topContributorsOpt {
        | Some(topContributors) =>
          React.array(
            topContributors->Array.mapWithIndex((index, (contributor, amount)) => {
              let amountRaisedFloat = \"||||"(amount->Float.fromString, 0.)

              amountRaisedFloat < 0.0000001
                ? React.null
                : <tr key=contributor className={rankingColor(index)}>
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
                          <strong>
                            {"#"->React.string} {(index + 1)->string_of_int->React.string}
                          </strong>
                        </p>
                      </span>
                    </td>
                    <td> <ContributorName contributor /> </td>
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
            }),
          )
        | None => React.null
        }}
      </tbody>
    </Rimble.Table>
  </div>
}
