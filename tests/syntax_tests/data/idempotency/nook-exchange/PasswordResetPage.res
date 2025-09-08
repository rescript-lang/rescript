module Styles = {
  open Css
  let root = style(list{
    backgroundColor(Colors.white),
    width(vw(90.)),
    boxSizing(borderBox),
    margin2(~v=px(32), ~h=auto),
    padding2(~v=px(32), ~h=px(32)),
    borderRadius(px(16)),
    maxWidth(px(512)),
  })
}

type status =
  | Success(string)
  | Error(string)

@react.component
let make = (~url: ReasonReactRouter.url) => {
  let (password, setPassword) = React.useState(() => "")
  let (isSubmitting, setIsSubmitting) = React.useState(() => false)
  let (status, setStatus) = React.useState(() => None)
  let token = React.useMemo1(() => {
    open Webapi.Url.URLSearchParams
    let searchParams = make(url.search)
    searchParams->get("token")
  }, [url.search])

  let onSubmit = e => {
    ReactEvent.Form.preventDefault(e)
    switch token {
    | Some(token) =>
      setIsSubmitting(_ => true)
      %Repromise.JsExn({
        let response = Fetch.fetchWithInit(
          Constants.apiUrl ++ "/password-reset/consume",
          Fetch.RequestInit.make(
            ~method_=Post,
            ~body=Fetch.BodyInit.make(
              Js.Json.stringify(
                Json.Encode.object_(list{
                  ("token", Js.Json.string(token)),
                  ("password", Js.Json.string(password)),
                }),
              ),
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
            %Repromise.JsExn({
              let json = Fetch.Response.json(response)
              let username = {
                open Json.Decode
                json->field("username", string)
              }
              setStatus(_ => Some(Success(username)))
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Reset Password Changed Success",
                ~eventProperties={"token": token, "username": username},
              )
              Promise.resolved()
            })
          } else {
            %Repromise.JsExn({
              let text = Fetch.Response.text(response)
              setStatus(_ => Some(Error(text)))
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Reset Password Changed Failure",
                ~eventProperties={"token": token, "error": text},
              )
              Promise.resolved()
            })
          }
          setIsSubmitting(_ => false)
          Promise.resolved()
        })
      })->ignore
    | None => setStatus(_ => Some(Error("Missing token")))
    }
  }

  <div className=Styles.root>
    <form onSubmit>
      <div className=LoginOverlay.Styles.registerTitle>
        {React.string("Change your password")}
      </div>
      <input
        type_="password"
        placeholder="New password"
        value=password
        onChange={e => {
          let value = ReactEvent.Form.target(e)["value"]
          setPassword(_ => value)
        }}
        className=LoginOverlay.Styles.input
      />
      {switch status {
      | Some(Error(error)) =>
        <div className=LoginOverlay.Styles.errorMessage> {React.string(error)} </div>
      | _ => React.null
      }}
      <div className=LoginOverlay.Styles.submitBar>
        {switch status {
        | Some(Success(username)) =>
          <div className=LoginOverlay.Styles.successMessage>
            {React.string(username ++ "'s password is changed!")}
          </div>
        | _ => React.null
        }}
        <button
          type_="submit"
          disabled={isSubmitting ||
          switch status {
          | Some(Success(_)) => true
          | _ => false
          }}
          className=LoginOverlay.Styles.submitButton>
          {React.string("Change password")}
        </button>
      </div>
    </form>
  </div>
}
