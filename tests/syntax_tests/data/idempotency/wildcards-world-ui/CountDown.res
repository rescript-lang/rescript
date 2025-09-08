open Globals

type timeUnit = {
  decimal: int,
  unit: string,
}

@send external padStart: (string, int, int) => string = "padStart"

let culculateUnitAmount = ((remainingSeconds, timeStructure), secondsInUnit, unitStr): (
  int,
  list<timeUnit>,
) =>
  if remainingSeconds >= secondsInUnit {
    let unitCount = remainingSeconds / secondsInUnit
    let remainingSeconds = remainingSeconds - unitCount * secondsInUnit
    (remainingSeconds, timeStructure->List.add({decimal: unitCount, unit: unitStr}))
  } else {
    (remainingSeconds, timeStructure)
  }

// NOTE: this rounds off the solar year to the nearest day (ie it is off by 5 hours 48 minutes 46 seconds)
// 365 * 24 * 60 * 60
let secondsInAYear = 31536000
// (365 * 24 * 60 * 60) / 12
let secondsInAMonth = 2628000
// 24 * 60 * 60
let secondsInADay = 86400
let secondsInAHour = 3600
let secondsInAMinute = 60

let calculateTimeRemainingTillHoursFromSeconds = numSeconds =>
  (numSeconds, list{})
  ->culculateUnitAmount(secondsInAYear, "year")
  ->culculateUnitAmount(secondsInAMonth, "month")
  ->culculateUnitAmount(secondsInADay, "day")
  ->culculateUnitAmount(secondsInAHour, "hour")

let calculateTimeRemainingFromSeconds = numSeconds => {
  let (_, durationStructure) =
    numSeconds
    ->calculateTimeRemainingTillHoursFromSeconds
    ->culculateUnitAmount(secondsInAMinute, "minute")
    ->culculateUnitAmount(1, "second")
  durationStructure
}

let displayTimeLeft = durationStructure =>
  durationStructure->Belt.List.reduce("", (acc, item) => {
    let display =
      item.decimal->string_of_int ++ (" " ++ (item.unit ++ (item.decimal > 1 ? "s " : " ")))
    display ++ acc
  })

let displayTimeLeftSimple = durationStructure => {
  let (displayStr, _) = durationStructure->Belt.List.reduce(("", ""), ((acc, spacer), item) => {
    let display = item.decimal->string_of_int->padStart(0, 2)
    (display ++ (spacer ++ acc), ":")
  })
  displayStr
}

let displayTimeLeftHours = remainingTime => {
  let (_, durationStructure) = remainingTime->calculateTimeRemainingTillHoursFromSeconds
  durationStructure->displayTimeLeft
}

let calculateCountdown = endDateMoment =>
  Int.fromFloat(MomentRe.diff(endDateMoment, MomentRe.momentNow(), #seconds))

@react.component
let make = (~endDateMoment, ~displayUnits=true) => {
  let (countDown, setCountdown) = React.useState(() => 0)

  React.useEffect2(() => {
    let date = calculateCountdown(endDateMoment)
    setCountdown(_ => date)
    let interval = Js.Global.setInterval(() => {
      let date = calculateCountdown(endDateMoment)
      setCountdown(_ => date)
    }, 1000)
    Some(() => Js.Global.clearInterval(interval))
  }, (endDateMoment, setCountdown))

  <React.Fragment>
    {displayUnits
      ? countDown->calculateTimeRemainingFromSeconds->displayTimeLeft->restr
      : countDown->calculateTimeRemainingFromSeconds->displayTimeLeftSimple->restr}
  </React.Fragment>
}
