module Styles = {
  open Css
  let body = style(list{
    padding2(~v=px(32), ~h=px(32)),
    width(vw(90.)),
    maxWidth(px(400)),
    boxSizing(borderBox),
    media("(max-width: 480px)", list{paddingLeft(px(16)), paddingRight(px(16))}),
  })
  let blurb = style(list{lineHeight(px(18)), paddingBottom(px(24))})
}

let process = (~code, ~isLogin, ~isRegister, ~isConnect) =>
  if isLogin || isRegister {
    %Repromise({
      let response = UserStore.loginWithDiscord(~code, ~isRegister)
      switch response {
      | Ok((user, true)) =>
        let modalKey = ref(None)
        let onClose = () =>
          ReactAtmosphere.API.removeLayer(~key=Belt.Option.getExn(modalKey.contents))
        modalKey :=
          Some(
            ReactAtmosphere.API.pushLayer(~render=_ =>
              <Modal>
                <div className=Styles.body>
                  <div>
                    <div className=LoginOverlay.Styles.registerTitle>
                      {React.string("Your account is created!")}
                    </div>
                    <div> {React.string("Add items to profile and share it with others!")} </div>
                    <div className=LoginOverlay.Styles.url>
                      <Link path={"/u/" ++ user.username}>
                        {React.string("nook.exchange/u/" ++ user.username)}
                      </Link>
                    </div>
                    <div className=Styles.blurb>
                      {React.string(
                        "Your username is " ++ (user.username ++ ". You can change it "),
                      )}
                      <Link path="/settings" onClick={_ => onClose()}>
                        {React.string("here")}
                      </Link>
                      {React.string(".")}
                    </div>
                    <div className=LoginOverlay.Styles.submitBar>
                      <button onClick={_ => onClose()} className=LoginOverlay.Styles.submitButton>
                        {React.string("Okay!")}
                      </button>
                    </div>
                  </div>
                </div>
              </Modal>
            ),
          )
      | Ok(_) => ()
      | Error(_) =>
        Error.showPopup(~message="Something went wrong. Sorry! Please reload and try again.")
      }
      Promise.resolved()
    })->ignore
  } else if isConnect {
    UserStore.connectDiscordAccount(~code)->ignore
  }
