open Globals

type previousNextAnimal = option<(TokenId.t, TokenId.t)>
type animalPageState =
  | DetailView(option<TokenId.t>)
  | NormalView
type explorerTab =
  | Gen1
  | Gen2
type leaderBoard =
  | TotalContribution
  | TotalDaysHeld
  | Unknown
  | MonthlyContribution
type artistId = string
type urlState =
  | User(Web3.ethAddress)
  | Artist(artistId)
  | Org(string)
  | Explorer(explorerTab, animalPageState)
  | Team
  | Leaderboards(leaderBoard)
  // | Unknown
  | Home(animalPageState)
  | IncreaseVoteIteration
  | VotePage

let useUrlState = () => {
  let url = ReasonReactRouter.useUrl()

  React.useMemo1(() =>
    switch Js.String.split("/", url.hash) {
    | ["user", address] => User(address->Js.String.toLowerCase)
    | ["artist", id] => Artist(id)
    | ["org", orgId] => Org(orgId->Js.String.toLowerCase)
    | ["leaderboards", leaderboardType] =>
      switch leaderboardType {
      | "monthly-contribution" => Leaderboards(MonthlyContribution)
      | "days-held" => Leaderboards(TotalDaysHeld)
      | "total-contribution" => Leaderboards(TotalContribution)
      | _ => Leaderboards(Unknown)
      }
    // | [|"explorer"|] => Explorer(NormalView)
    | ["explorer", "details", animalStr]
    | ["explorer", "details", animalStr, ""] =>
      let optionAnimal = Animal.getAnimal(animalStr)
      Explorer(Gen2, DetailView(optionAnimal))
    | ["explorer", tab, "details", animalStr]
    | ["explorer", tab, "details", animalStr, ""] =>
      let optionAnimal = Animal.getAnimal(animalStr)
      Explorer(
        switch tab {
        | "2nd-edition" => Gen2
        | "1st-edition"
        | _ =>
          /* Default is Gen1 */
          Gen1
        },
        DetailView(optionAnimal),
      )
    | ["details", animalStr] =>
      let optionAnimal = Animal.getAnimal(animalStr)
      Home(DetailView(optionAnimal))
    | ["dao"] => VotePage
    | ["increase-iteration"] => IncreaseVoteIteration
    | urlArray =>
      switch \"||||"(urlArray[0], "") {
      | "explorer" =>
        Explorer(
          switch \"||||"(urlArray[1], "") {
          | "1st-edition" => Gen1
          | "2nd-edition"
          | _ =>
            /* Default is Gen1 */
            Gen2
          },
          NormalView,
        )
      | "team" => Team
      | _ => Home(NormalView)
      // | _ => Unknown
      }
    }
  , [url.hash])
}
let useIsExplorer = () => {
  let urlState = useUrlState()

  React.useMemo1(() =>
    switch urlState {
    | Explorer(_) => true
    | User(_)
    | Artist(_)
    | Leaderboards(_)
    | Home(_)
    | Org(_)
    | IncreaseVoteIteration
    | Team
    | VotePage => false
    }
  , [urlState])
}
let isDetailsAnimalPage: animalPageState => bool = animalPageState =>
  switch animalPageState {
  | DetailView(_) => true
  | NormalView => false
  }
let useIsDetails = () => {
  let urlState = useUrlState()

  React.useMemo1(() =>
    switch urlState {
    | Explorer(_, inside) => isDetailsAnimalPage(inside)
    | Home(inside) => isDetailsAnimalPage(inside)
    | User(_)
    | Artist(_)
    | Org(_)
    | Leaderboards(_)
    | IncreaseVoteIteration
    | Team
    | VotePage => false
    }
  , [urlState])
}
let useIsHome = () => {
  let urlState = useUrlState()

  React.useMemo1(() =>
    switch urlState {
    | Home(NormalView) => true
    | Home(DetailView(_))
    | User(_)
    | Artist(_)
    | Org(_)
    | Explorer(_)
    | Leaderboards(_)
    | IncreaseVoteIteration
    | Team
    | VotePage => false
    }
  , [urlState])
}
let getAnimalFormAnimalPageState: animalPageState => option<TokenId.t> = animalPageState =>
  switch animalPageState {
  | DetailView(optAnimal) => optAnimal
  | NormalView => None
  }

let useAnimalForDetails = () => {
  let urlState = useUrlState()

  React.useMemo1(() =>
    switch urlState {
    | Explorer(_, animalPageState) => getAnimalFormAnimalPageState(animalPageState)
    | Home(animalPageState) => getAnimalFormAnimalPageState(animalPageState)
    // | DetailView(_, optAnimal) => optAnimal
    | User(_)
    | Artist(_)
    | Org(_)
    | Leaderboards(_)
    | Team
    | IncreaseVoteIteration
    | VotePage =>
      None
    }
  , [urlState])
}
