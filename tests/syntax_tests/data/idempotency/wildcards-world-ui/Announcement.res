open Globals
open Css

let announcementStyle = (displayVal, announcementBannerColor) =>
  style(list{
    display(displayVal),
    position(relative),
    padding2(~v=rem(0.4), ~h=rem(1.)),
    color(white),
    backgroundColor(hex(announcementBannerColor)),
    textAlign(#center),
    zIndex(2),
    fontSize(px(14)),
    // textTransform(uppercase),
    letterSpacing(px(2)),
  })

@react.component
let make = (~announcementBannerColor: string, ~children) => {
  let closeButton = style(list{position(absolute), right(px(10))})

  let (showAnnouncement, setShowAnnouncement) = React.useState(() => #block)

  <div className={announcementStyle(showAnnouncement, announcementBannerColor)}>
    children
    <span className=closeButton onClick={_ => setShowAnnouncement(_ => #none)}>
      {`×`->restr}
    </span>
  </div>
}
