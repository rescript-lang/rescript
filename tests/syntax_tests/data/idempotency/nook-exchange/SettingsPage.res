module Styles = {
  open Css
  let root = style(list{})
  let input = style(list{
    backgroundColor(rgba(0, 0, 0, 0.05)),
    border(px(1), solid, transparent),
    borderRadius(px(4)),
    padding2(~v=px(10), ~h=px(12)),
    boxSizing(borderBox),
    outlineStyle(none),
    fontSize(px(16)),
    width(pct(100.)),
    marginBottom(px(16)),
    transition(~duration=200, "all"),
    focus(list{
      backgroundColor(Colors.white),
      borderColor(rgba(0, 0, 0, 0.15)),
      boxShadow(Shadow.box(~spread=px(4), rgba(0, 0, 0, 0.05))),
    }),
    disabled(list{color(Colors.gray)}),
  })
  let submitBar = style(list{display(flexBox), alignItems(center), justifyContent(flexEnd)})
  let successMessage = style(list{flexGrow(1.), color(Colors.green)})
  let submitButton = style(list{
    backgroundColor(Colors.green),
    borderWidth(zero),
    borderRadius(px(4)),
    color(Colors.white),
    cursor(pointer),
    marginLeft(px(16)),
    padding2(~v=px(8), ~h=px(16)),
    fontSize(px(16)),
    transition(~duration=200, "all"),
    disabled(list{opacity(0.5)}),
  })
  let blurb = style(list{marginBottom(px(16))})
  let urlPreview = style(list{
    fontSize(px(12)),
    letterSpacing(pxFloat(0.3)),
    color(hex("b0b0b0")),
    marginBottom(px(8)),
  })
  let errorMessage = style(list{marginTop(px(-10)), marginBottom(px(16)), color(Colors.red)})
  let discordSection = style(list{
    borderBottom(px(1), dashed, Colors.lightGray),
    marginBottom(px(16)),
    paddingTop(px(8)),
    paddingBottom(px(16)),
  })
  let discordButton = style(list{marginTop(px(16))})
  let deleteSection = style(list{
    borderTop(px(1), dashed, Colors.lightGray),
    marginTop(px(16)),
    paddingTop(px(16)),
  })
  let deleteLink = style(list{
    color(Colors.red),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
  })
}

type submitStatus =
  | Success
  | Error(string)

module WithUser = {
  @react.component
  let make = (~user: User.t) => {
    let (username, setUsername) = React.useState(() => user.username)
    let (password, setPassword) = React.useState(() => "")
    let (oldPassword, setOldPassword) = React.useState(() => "")
    let hasChanges = user.username != username || password != ""
    let (isSubmitting, setIsSubmitting) = React.useState(() => false)
    let (status, setStatus) = React.useState(() => None)

    React.useEffect0(() => {
      Analytics.Amplitude.logEvent(~eventName="Settings Viewed")
      None
    })

    let onSubmit = e => {
      ReactEvent.Form.preventDefault(e)

      {
        let prevUsername = user.username
        setIsSubmitting(_ => true)
        let usernameUpdate = username == user.username ? None : Some(username)
        %Repromise({
          let result = UserStore.patchMe(
            ~username=?usernameUpdate,
            ~newPassword=?password != "" ? Some(password) : None,
            ~oldPassword=?oldPassword != "" ? Some(oldPassword) : None,
            (),
          )
          setIsSubmitting(_ => false)
          switch result {
          | Ok() =>
            setStatus(_ => Some(Success))
            setPassword(_ => "")
            setOldPassword(_ => "")
          | Error(err) => setStatus(_ => Some(Error(err)))
          }
          switch usernameUpdate {
          | Some(username) =>
            let url = ReasonReactRouter.dangerouslyGetInitialUrl()
            switch url.path {
            | list{"u", pathUsername, ..._} =>
              if Js.String.toLowerCase(pathUsername) == Js.String.toLowerCase(prevUsername) {
                ReasonReactRouter.push("/u/" ++ username)
              }
            | _ => ()
            }
          | None => ()
          }
          Promise.resolved()
        })
      }->ignore
    }

    <div className=Styles.root>
      <PageTitle title="Settings" />
      <BodyCard>
        <div className=Styles.discordSection>
          {switch user.discordId {
          | Some(_) =>
            <div>
              {React.string("Your Discord account is connected. Visit the ")}
              <a href="https://discord.gg/9sh66CX" target="_blank">
                {React.string("Discord server")}
              </a>
              {React.string(" to trade and share feedback!")}
            </div>
          | None => <>
              <div> {React.string("Join the Discord server to trade and share feedback!")} </div>
              <div>
                <button
                  onClick={_ => {
                    let state = "connect_" ++ string_of_int(Js.Math.random_int(100000, 999999))

                    {
                      open Dom.Storage
                      localStorage->setItem("discord_state", state)
                    }

                    open Webapi.Dom
                    location->Location.setHref(Constants.discordOauthRedirectUri(state))
                  }}
                  className={Cn.make(list{
                    LoginOverlay.Styles.discordButton,
                    Styles.discordButton,
                  })}>
                  <span className=LoginOverlay.Styles.discordButtonLogo />
                  {React.string("Connect and Join Discord")}
                </button>
              </div>
            </>
          }}
        </div>
        <form onSubmit>
          <div className=Styles.blurb>
            {React.string(
              user.discordId === None
                ? "You can change your username or password here!"
                : "You can change your username here!",
            )}
          </div>
          <div className=Styles.urlPreview> {React.string("nook.exchange/u/" ++ username)} </div>
          <input
            type_="text"
            placeholder="Username"
            value=username
            onChange={e => {
              let value = ReactEvent.Form.target(e)["value"]
              setUsername(_ => value)
            }}
            className=Styles.input
          />
          {if user.discordId === None {
            <>
              <input
                type_="password"
                placeholder="New Password (optional)"
                value=password
                onChange={e => {
                  let value = ReactEvent.Form.target(e)["value"]
                  setPassword(_ => value)
                }}
                className=Styles.input
              />
              <input
                type_="password"
                placeholder="Old Password (required)"
                value=oldPassword
                onChange={e => {
                  let value = ReactEvent.Form.target(e)["value"]
                  setOldPassword(_ => value)
                }}
                className=Styles.input
              />
            </>
          } else {
            React.null
          }}
          {switch status {
          | Some(Error(error)) => <div className=Styles.errorMessage> {React.string(error)} </div>
          | _ => React.null
          }}
          <div className=Styles.submitBar>
            {status == Some(Success)
              ? <div className=Styles.successMessage> {React.string("Update successful!")} </div>
              : React.null}
            <button
              type_="submit"
              disabled={!hasChanges ||
              (isSubmitting ||
              user.discordId === None && oldPassword == "")}
              className=Styles.submitButton>
              {React.string("Submit")}
            </button>
          </div>
        </form>
        <div className=Styles.deleteSection>
          <div>
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                ConfirmDialog.confirm(
                  ~bodyText="Are you sure you want to remove ALL ITEMS from your account? This includes all items in your For Trade, Can Craft, Wishlist, and Catalog.",
                  ~confirmLabel="Remove items",
                  ~cancelLabel="Never mind",
                  ~onConfirm=() => UserStore.removeAllItems()->ignore,
                  (),
                )
              }}
              className=Styles.deleteLink>
              {React.string("Remove all items")}
            </a>
          </div>
          <div>
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                ConfirmDialog.confirm(
                  ~bodyText="Are you sure you want to delete your account? THIS CANNOT BE UNDONE!",
                  ~confirmLabel="Delete account",
                  ~cancelLabel="Never mind",
                  ~onConfirm=() => UserStore.deleteAccount()->ignore,
                  (),
                )
              }}
              className=Styles.deleteLink>
              {React.string("Delete account")}
            </a>
          </div>
        </div>
      </BodyCard>
    </div>
  }
}

@react.component
let make = () => {
  let user = UserStore.useMe()
  switch user {
  | Some(user) => <WithUser user />
  | None => React.null
  }
}
