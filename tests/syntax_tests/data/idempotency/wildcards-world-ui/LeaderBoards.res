let indexToType = tabIndex =>
  switch tabIndex {
  | 0 => "monthly-contribution"
  | 1 => "total-contribution"
  | 2 => "days-held"
  | _ => "unknown"
  }

@react.component
let make = (~leaderboardType) => {
  let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

  let index = switch leaderboardType {
  | Router.MonthlyContribution => 0
  | Router.TotalContribution => 1
  | Router.TotalDaysHeld => 2
  | Router.Unknown => -1
  }
  let selectLeaderBoard = (newIndex, _oldIndex) => {
    clearAndPush("#leaderboards/" ++ indexToType(newIndex))

    true
  }

  <ReactTabs selectedIndex=index onSelect=selectLeaderBoard>
    <ReactTabs.TabList>
      <ReactTabs.Tab> {"Monthly Contribution"->React.string} </ReactTabs.Tab>
      <ReactTabs.Tab> {"Total Contribution"->React.string} </ReactTabs.Tab>
      <ReactTabs.Tab> {"Days Held"->React.string} </ReactTabs.Tab>
    </ReactTabs.TabList>
    <ReactTabs.TabPanel> <MonthlyContribution numberOfLeaders=10 /> </ReactTabs.TabPanel>
    <ReactTabs.TabPanel> <TotalContribution numberOfLeaders=10 /> </ReactTabs.TabPanel>
    <ReactTabs.TabPanel> <TotalDaysHeld numberOfLeaders=10 /> </ReactTabs.TabPanel>
  </ReactTabs>
}
