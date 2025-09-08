open Globals

let backgroundStyle = {
  open Css
  style(list{
    paddingTop(rem(1.)),
    unsafe("backgroundImage", "linear-gradient(to top, #74C7D7 0%, white 100%)"),
  })
}
let headingStyle = {
  open Css
  style(list{paddingTop(rem(5.)), textAlign(#center)})
}

module Grid = {
  @react.component
  let make = (~chain) => {
    let allAnimals = QlHooks.useAnimalList(~chain)

    <Rimble.Flex flexWrap="wrap" justifyContent="space-around" alignItems="stretch" px=50>
      {allAnimals
      ->Array.map(animal =>
        <Rimble.Box key={animal->TokenId.toString} fontSize=4 p=3 width=[1., 1., 0.3]>
          <Rimble.Card> <Dapp.CarouselAnimal chain animal scalar=1. /> </Rimble.Card>
        </Rimble.Box>
      )
      ->React.array}
      ///* Always empty to ensure grid structure */}
      <Rimble.Box fontSize=4 p=3 width=[1., 1., 0.3]> React.null </Rimble.Box>
      <Rimble.Box fontSize=4 p=3 width=[1., 1., 0.3]> React.null </Rimble.Box>
    </Rimble.Flex>
  }
}

let indexToType = tabIndex =>
  switch tabIndex {
  | 0 => "1st-edition"
  | 1 => "2nd-edition"
  // | 2 => "coming-soon"
  | _ => "unknown"
  }

@react.component
let make = (~wildcardsEdition) => {
  let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

  let index = switch wildcardsEdition {
  | Router.Gen1 => 0
  | Router.Gen2 => 1
  }
  let selectLeaderBoard = (newIndex, _oldIndex) => {
    clearAndPush("#explorer/" ++ indexToType(newIndex))

    true
  }

  <div className=backgroundStyle>
    <div>
      <h1 className=headingStyle> {"Wildcards Kingdom"->restr} </h1>
      <ReactTabs selectedIndex=index onSelect=selectLeaderBoard>
        <ReactTabs.TabList>
          <ReactTabs.Tab> {"First edition"->React.string} </ReactTabs.Tab>
          <ReactTabs.Tab> {"Second edition"->React.string} </ReactTabs.Tab>
        </ReactTabs.TabList>
        <ReactTabs.TabPanel> <Grid chain=Client.MainnetQuery /> </ReactTabs.TabPanel>
        <ReactTabs.TabPanel> <Grid chain=Client.MaticQuery /> </ReactTabs.TabPanel>
      </ReactTabs>
    </div>
  </div>
}
