@react.component
let make = () => {
  let (showChat, setShowChat) = React.useState(_ => false)
  <div
    className={
      open Css
      style(list{position(#fixed), bottom(px(10)), right(px(10)), zIndex(1000)})
    }>
    {showChat
      ? <div
          className={
            open Css
            style(list{position(#relative)})
          }>
          <div
            className={
              open Css
              style(list{
                height(px(50)),
                width(px(50)),
                marginLeft(#auto),
                backgroundColor(hex("CCCCCCCC")),
                borderRadius(#percent(50.)),
                marginBottom(0.2->rem),
                selector(":hover", list{important(backgroundColor(hex("6CAD3DCC")))}),
              })
            }
            onClick={_ => setShowChat(_ => false)}>
            <div
              className={
                open Css
                style(list{
                  padding(1.->rem),
                  fontSize(16->px),
                  transform(translateX(px(3))),
                  color(white),
                })
              }>
              {`✖`->React.string}
            </div>
          </div>
          <iframe
            src="https://discordapp.com/widget?id=723502058426073108&theme=dark"
            width="350"
            height="500"
            sandbox="allow-popups allow-popups-to-escape-sandbox allow-same-origin allow-scripts"
            className={
              open Css
              style(list{borderWidth(px(0))})
            }
          />
        </div>
        //Add close button above here
      : <img
          src="/img/icons/discord.svg"
          className={
            open Css
            style(list{height(px(50)), width(px(50)), marginLeft(#auto)})
          }
          onClick={_ => setShowChat(_ => true)}
        />}
  </div>
}
