open Globals
open Css

let linkToAnimal = style(list{
  color(white),
  textDecoration(underline),
  selector(":visited", list{color(hex("ffffff"))}),
  selector(":hover", list{important(color(hex("6CAD3D")))}),
})

@react.component
let make = (~nextReleasedAnimals: array<TokenId.t>, ~announcementBannerColor: string) => {
  let nextLaunchDate = Animal.nextLaunchDate

  let isLaunched = MomentRe.diff(nextLaunchDate, MomentRe.momentNow(), #seconds) < 0.

  let closeButton = style(list{position(absolute), right(px(10))})

  let (showAnnouncement, setShowAnnouncement) = React.useState(() => #block)
  let numberOfAnimalsToLaunch = nextReleasedAnimals->Array.length
  let isPlural = numberOfAnimalsToLaunch > 1
  <div className={Announcement.announcementStyle(showAnnouncement, announcementBannerColor)}>
    {("New Wildcard" ++ ((isPlural ? "s" : "") ++ " "))->restr}
    {React.array(
      nextReleasedAnimals->Array.mapWithIndex((index, animal) => {
        let name = \"||||"(QlHooks.useWildcardName(animal), "Loading")
        <span key={animal->TokenId.toString}>
          <a href={"/#details/" ++ animal->TokenId.toString} className=linkToAnimal>
            {name->restr}
          </a>
          {(
            index == numberOfAnimalsToLaunch - 1
              ? " "
              : index == numberOfAnimalsToLaunch - 2
              ? " and "
              : ", "
          )->restr}
        </span>
      }),
    )}
    {isLaunched
      ? ((isPlural ? "have" : "has") ++ " just been launched!")->restr
      : <>
          {"coming in "->restr}
          <CountDown endDateMoment=nextLaunchDate displayUnits=true />
          {"!"->restr}
        </>}
    <span className=closeButton onClick={_ => setShowAnnouncement(_ => #none)}>
      {`×`->restr}
    </span>
  </div>
}
