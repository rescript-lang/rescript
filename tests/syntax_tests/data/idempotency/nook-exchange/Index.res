@module("@sentry/browser")
external initSentry: {"dsn": string, "ignoreErrors": array<string>} => unit = "init"

@set
external setOnError: (
  Webapi.Dom.Window.t,
  (string, string, int, int, {"stack": string}) => unit,
) => unit = "onerror"

initSentry({
  "dsn": Constants.sentryId,
  "ignoreErrors": ["Failed to execute 'insertBefore' on 'Node'", "Loading chunk"],
})
Analytics.Amplitude.init()
Webapi.Dom.window->setOnError((message, url, _line, _column, error) =>
  Analytics.Amplitude.logEventWithProperties(
    ~eventName="Error Raised",
    ~eventProperties={
      "message": message,
      "url": url,
      "errorStack": error["stack"],
    },
  )
)
UserStore.init()

ReactDOMRe.renderToElementWithId(<App />, "container")
