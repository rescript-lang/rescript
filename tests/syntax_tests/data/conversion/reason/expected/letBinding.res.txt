let deltaMode: t => Webapi__Dom__Types.deltaMode = self =>
  Webapi__Dom__Types.decodeDeltaMode(deltaMode(self))

let fromJs: ResourceIo.campaignWeeklyPlanning => t = weeklyPlanning => (
  weeklyPlanning["monday"]->dayFromJs,
  weeklyPlanning["tuesday"]->dayFromJs,
  weeklyPlanning["wednesday"]->dayFromJs,
  weeklyPlanning["thursday"]->dayFromJs,
  weeklyPlanning["friday"]->dayFromJs,
  weeklyPlanning["saturday"]->dayFromJs,
  weeklyPlanning["sunday"]->dayFromJs,
)

let newChapter: Video.chapter = {startTime: percent *. duration}
