module Styles = {
  open Css
  let backdrop = style(list{
    position(absolute),
    top(zero),
    bottom(zero),
    left(zero),
    right(zero),
    backgroundColor(hex("808080a0")),
  })
  let root = style(list{
    backgroundColor(hex("ffffff")),
    borderRadius(px(4)),
    position(relative),
    maxWidth(px(448)),
    boxSizing(borderBox),
    width(pct(90.)),
    boxShadow(Shadow.box(~spread=px(12), rgba(0, 0, 0, 0.1))),
    overflow(auto),
    maxHeight(vh(100.)),
    media("(max-width: 540px)", list{paddingTop(px(24)), paddingBottom(px(24))}),
  })
  let body = style(list{
    padding2(~v=px(32), ~h=px(32)),
    boxSizing(borderBox),
    maxWidth(px(400)),
    width(vw(90.)),
    margin2(~v=zero, ~h=auto),
    media("(max-width: 480px)", list{paddingLeft(px(16)), paddingRight(px(16))}),
  })
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
      backgroundColor(hex("ffffff")),
      borderColor(rgba(0, 0, 0, 0.15)),
      boxShadow(Shadow.box(~spread=px(4), rgba(0, 0, 0, 0.05))),
    }),
  })
  let submitBar = style(list{display(flexBox), alignItems(center), justifyContent(flexEnd)})
  let submitButton = style(list{
    backgroundColor(Colors.green),
    borderWidth(zero),
    borderRadius(px(4)),
    color(Colors.white),
    cursor(pointer),
    marginLeft(px(16)),
    padding2(~v=px(10), ~h=px(16)),
    fontSize(px(16)),
    transition(~duration=200, "all"),
    disabled(list{opacity(0.5)}),
  })
  let divider = style(list{
    backgroundColor(hex("e0e0e0")),
    height(px(1)),
    width(pct(100.)),
    margin2(~v=px(24), ~h=zero),
  })
  let terms = style(list{color(hex("a0a0a0")), fontSize(px(12)), marginTop(px(32))})
  let registerTitle = style(list{fontSize(px(20)), textAlign(center), marginBottom(px(24))})
  let blurb = style(list{marginBottom(px(16)), textAlign(center)})
  let discordLoginRow = style(list{display(flexBox), justifyContent(center), marginBottom(px(8))})
  @module("./assets/discord_logo.png")
  external discordPng: string = "default"
  let discordButtonLogo = style(list{
    backgroundImage(url(discordPng)),
    display(inlineBlock),
    marginRight(px(8)),
    width(px(24)),
    height(px(24)),
    backgroundSize(cover),
  })
  let discordButton = style(list{
    backgroundColor(hex("7289da")),
    borderWidth(zero),
    borderRadius(px(4)),
    color(Colors.white),
    display(flexBox),
    alignItems(center),
    justifyContent(spaceBetween),
    fontSize(px(16)),
    padding2(~v=px(8), ~h=px(16)),
  })
  let orSection = style(list{
    padding2(~v=px(8), ~h=zero),
    display(flexBox),
    alignItems(center),
    justifyContent(center),
  })
  let orWord = style(list{color(Colors.gray), padding2(~v=zero, ~h=px(16))})
  let orLine = style(list{flexGrow(1.), height(px(1)), backgroundColor(Colors.veryLightGray)})
  let urlPreview = style(list{
    fontSize(px(12)),
    letterSpacing(pxFloat(0.3)),
    color(hex("b0b0b0")),
    marginBottom(px(8)),
  })
  let url = style(list{
    border(px(2), dashed, Colors.lightGreen),
    textAlign(center),
    borderRadius(px(4)),
    padding(px(16)),
    margin2(~v=px(16), ~h=zero),
  })
  let successMessage = style(list{flexGrow(1.), color(Colors.green)})
  let errorMessage = style(list{marginTop(px(-10)), marginBottom(px(16)), color(Colors.red)})
}

module PasswordReset = {
  type status =
    | Success
    | Error(string)

  @react.component
  let make = () => {
    let (email, setEmail) = React.useState(() => "")
    let (isSubmitting, setIsSubmitting) = React.useState(() => false)
    let (status, setStatus) = React.useState(() => None)

    let onSubmit = e => {
      ReactEvent.Form.preventDefault(e)
      setIsSubmitting(_ => true)
      %Repromise.JsExn({
        let response = Fetch.fetchWithInit(
          Constants.apiUrl ++ "/password-reset",
          Fetch.RequestInit.make(
            ~method_=Post,
            ~body=Fetch.BodyInit.make(
              Js.Json.stringify(Json.Encode.object_(list{("email", Js.Json.string(email))})),
            ),
            ~headers=Fetch.HeadersInit.make({
              "X-Client-Version": Constants.gitCommitRef,
              "Content-Type": "application/json",
            }),
            ~mode=CORS,
            (),
          ),
        )
        %Repromise({
          let _ = if Fetch.Response.status(response) < 300 {
            setStatus(_ => Some(Success))
            Analytics.Amplitude.logEventWithProperties(
              ~eventName="Reset Password Request Success",
              ~eventProperties={"email": email},
            )
            Promise.resolved()
          } else {
            %Repromise.JsExn({
              let text = Fetch.Response.text(response)
              setStatus(_ => Some(Error(text)))
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Reset Password Request Failure",
                ~eventProperties={"email": email, "error": text},
              )
              Promise.resolved()
            })
          }
          setIsSubmitting(_ => false)
          Promise.resolved()
        })
      })->ignore
    }

    <form onSubmit>
      <div className=Styles.registerTitle> {React.string("Reset your password")} </div>
      <input
        type_="text"
        placeholder="Account email"
        value=email
        onChange={e => {
          let value = ReactEvent.Form.target(e)["value"]
          setEmail(_ => value)
        }}
        className=Styles.input
      />
      {switch status {
      | Some(Error(error)) => <div className=Styles.errorMessage> {React.string(error)} </div>
      | _ => React.null
      }}
      <div className=Styles.submitBar>
        {status == Some(Success)
          ? <div className=Styles.successMessage> {React.string("Check your email!")} </div>
          : React.null}
        <button
          type_="submit"
          disabled={isSubmitting || status == Some(Success)}
          className=Styles.submitButton>
          {React.string("Send reset link")}
        </button>
      </div>
    </form>
  }
}

type registerStatus =
  | Success
  | Error(string)

type screen =
  | Login
  | Register
  | PasswordReset

@react.component
let make = (~onClose) => {
  let (screen, setScreen) = React.useState(() => Register)

  let (username, setUsername) = React.useState(() => "")
  let (email, setEmail) = React.useState(() => "")
  let (password, setPassword) = React.useState(() => "")
  let (isSubmitting, setIsSubmitting) = React.useState(() => false)

  React.useEffect0(() => {
    Analytics.Amplitude.logEvent(~eventName="Registration Viewed")
    None
  })

  let (registerStatus, setRegisterStatus) = React.useState(() => None)
  let onLoginSubmit = e => {
    ReactEvent.Form.preventDefault(e)

    {
      setIsSubmitting(_ => true)
      %Repromise({
        let result = UserStore.login(~username, ~password)
        switch result {
        | Ok(_) =>
          onClose()
          Analytics.Amplitude.logEvent(~eventName="Login Succeeded")
        | Error(_) =>
          setRegisterStatus(_ => Some(Error("Login failed. Please try again.")))
          Analytics.Amplitude.logEvent(~eventName="Login Failed")
          setIsSubmitting(_ => false)
        }
        Promise.resolved()
      })
    }->ignore
  }

  let onRegisterSubmit = e => {
    ReactEvent.Form.preventDefault(e)

    {
      setIsSubmitting(_ => true)
      %Repromise({
        let result = UserStore.register(~username, ~email, ~password)
        switch result {
        | Ok(_) =>
          setRegisterStatus(_ => Some(Success))
          Analytics.Amplitude.logEventWithProperties(
            ~eventName="Registration Succeeded",
            ~eventProperties={"username": username, "email": email},
          )
        | Error(error) =>
          setRegisterStatus(_ => Some(Error(error)))
          Analytics.Amplitude.logEventWithProperties(
            ~eventName="Registration Failed",
            ~eventProperties={"error": error},
          )
        }
        setIsSubmitting(_ => false)
        Promise.resolved()
      })
    }->ignore
  }

  let usernameRef = React.useRef(Js.Nullable.null)
  React.useEffect0(() => {
    open Webapi.Dom
    let usernameInput = Utils.getElementForDomRef(usernameRef)->Element.unsafeAsHtmlElement
    HtmlElement.focus(usernameInput)
    None
  })

  <Modal>
    <div className=Styles.body>
      {switch screen {
      | Login =>
        <div>
          <div className=Styles.registerTitle> {React.string("Welcome back!")} </div>
          <div className=Styles.discordLoginRow>
            <button
              onClick={_ => {
                let state = "login_" ++ string_of_int(Js.Math.random_int(100000, 999999))

                {
                  open Dom.Storage
                  localStorage->setItem("discord_state", state)
                }

                open Webapi.Dom
                location->Location.setHref(Constants.discordOauthRedirectUri(state))
              }}
              className=Styles.discordButton>
              <span className=Styles.discordButtonLogo /> {React.string("Login with Discord")}
            </button>
          </div>
          <div className=Styles.orSection>
            <div className=Styles.orLine />
            <div className=Styles.orWord> {React.string("or")} </div>
            <div className=Styles.orLine />
          </div>
          <form onSubmit=onLoginSubmit>
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
            <input
              type_="password"
              placeholder="Password"
              value=password
              onChange={e => {
                let value = ReactEvent.Form.target(e)["value"]
                setPassword(_ => value)
              }}
              className=Styles.input
            />
            <div className=Styles.errorMessage>
              <a
                href="#"
                onClick={e => {
                  ReactEvent.Mouse.preventDefault(e)
                  setScreen(_ => PasswordReset)
                }}>
                {React.string("Forgot password?")}
              </a>
            </div>
            {switch registerStatus {
            | Some(Error(error)) => <div className=Styles.errorMessage> {React.string(error)} </div>
            | _ => React.null
            }}
            <div className=Styles.submitBar>
              <a
                href="#"
                onClick={e => {
                  ReactEvent.Mouse.preventDefault(e)
                  setScreen(_ => Register)
                }}>
                {React.string("Need an account?")}
              </a>
              <button type_="submit" disabled=isSubmitting className=Styles.submitButton>
                {React.string("Login")}
              </button>
            </div>
          </form>
        </div>
      | Register =>
        registerStatus == Some(Success)
          ? <div>
              <div className=Styles.registerTitle> {React.string("Your account is created!")} </div>
              <div className=Styles.blurb>
                {React.string("Add items to profile and share it with others!")}
              </div>
              <div className=Styles.url>
                <Link path={"/u/" ++ username}>
                  {React.string("nook.exchange/u/" ++ username)}
                </Link>
              </div>
              <div className=Styles.submitBar>
                <button onClick={_ => onClose()} className=Styles.submitButton>
                  {React.string("Okay!")}
                </button>
              </div>
            </div>
          : <div>
              <div className=Styles.registerTitle> {React.string("Register an account")} </div>
              <div className=Styles.blurb>
                {React.string("Create and share lists of Animal Crossing items!")}
              </div>
              <div className=Styles.discordLoginRow>
                <button
                  onClick={_ => {
                    let state = "register_" ++ string_of_int(Js.Math.random_int(100000, 999999))

                    {
                      open Dom.Storage
                      localStorage->setItem("discord_state", state)
                    }

                    open Webapi.Dom
                    location->Location.setHref(Constants.discordOauthRedirectUri(state))
                  }}
                  className=Styles.discordButton>
                  <span className=Styles.discordButtonLogo />
                  {React.string("Register and Join Discord")}
                </button>
              </div>
              <div className=Styles.orSection>
                <div className=Styles.orLine />
                <div className=Styles.orWord> {React.string("or")} </div>
                <div className=Styles.orLine />
              </div>
              <div className=Styles.urlPreview>
                {React.string("nook.exchange/u/" ++ username)}
              </div>
              <div>
                <form onSubmit=onRegisterSubmit>
                  <input
                    type_="text"
                    placeholder="Username"
                    value=username
                    onChange={e => {
                      let value = ReactEvent.Form.target(e)["value"]
                      setUsername(_ => value)
                    }}
                    ref={ReactDOMRe.Ref.domRef(usernameRef)}
                    className=Styles.input
                  />
                  <input
                    type_="text"
                    placeholder="Email (Optional for account recovery)"
                    value=email
                    onChange={e => {
                      let value = ReactEvent.Form.target(e)["value"]
                      setEmail(_ => value)
                    }}
                    className=Styles.input
                  />
                  <input
                    type_="password"
                    placeholder="Password"
                    value=password
                    onChange={e => {
                      let value = ReactEvent.Form.target(e)["value"]
                      setPassword(_ => value)
                    }}
                    className=Styles.input
                  />
                  {switch registerStatus {
                  | Some(Error(error)) =>
                    <div className=Styles.errorMessage> {React.string(error)} </div>
                  | _ => React.null
                  }}
                  <div className=Styles.submitBar>
                    <a
                      href="#"
                      onClick={e => {
                        ReactEvent.Mouse.preventDefault(e)
                        setScreen(_ => Login)
                      }}>
                      {React.string("Login instead?")}
                    </a>
                    <button
                      type_="submit"
                      className=Styles.submitButton
                      disabled={Js.String.length(password) < 4 || isSubmitting}>
                      {React.string("Register")}
                    </button>
                  </div>
                  <div className=Styles.terms>
                    {React.string("By registering, you agree to Nook Exchange's ")}
                    <a href="/terms" target="_blank"> {React.string("Terms of Service")} </a>
                    {React.string(" and ")}
                    <a href="/privacy" target="_blank"> {React.string("Privacy Policy")} </a>
                    {React.string(".")}
                  </div>
                </form>
              </div>
            </div>
      | PasswordReset => <PasswordReset />
      }}
    </div>
    <Modal.CloseButton onClose />
  </Modal>
}
