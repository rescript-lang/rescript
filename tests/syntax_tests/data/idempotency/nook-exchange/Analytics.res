module Amplitude = {
  module API = {
    type t

    @module("amplitude-js")
    external getInstance: unit => t = "getInstance"

    @send external init: (t, string) => unit = "init"
    @send external logEvent: (t, string) => unit = "logEvent"
    @send
    external logEventWithProperties: (t, string, 'a) => unit = "logEvent"
    @send external setUserId: (t, option<string>) => unit = "setUserId"
    @send external setVersionName: (t, string) => unit = "setVersionName"

    type identify
    @module("amplitude-js") @new
    external getIdentify: unit => identify = "Identify"
    @send
    external identifySet: (identify, string, string) => identify = "set"
    @send
    external identifySetInt: (identify, string, int) => identify = "set"
    @send
    external identifyAppend: (identify, string, string) => identify = "append"

    @send external identify: (t, identify) => unit = "identify"
  }

  let instanceRef = ref(None)
  let getInstance = () =>
    switch instanceRef.contents {
    | Some(instance) => instance
    | None =>
      let instance = API.getInstance()
      instance->API.init(Constants.amplitudeApiKey)
      instanceRef := Some(instance)
      instance
    }

  let init = () => getInstance()->API.setVersionName(Constants.gitCommitRef)

  let logEvent = (~eventName) => getInstance()->API.logEvent(eventName)

  let logEventWithProperties = (~eventName, ~eventProperties) =>
    getInstance()->API.logEventWithProperties(eventName, eventProperties)

  let setUserId = (~userId) => getInstance()->API.setUserId(userId)

  let setUsername = (~username, ~email) => {
    let identifyInstance =
      API.getIdentify()->API.identifySet("username", username)->API.identifySet("email", email)
    getInstance()->API.identify(identifyInstance)
  }

  let setLanguage = (~language) => {
    let identifyInstance = API.getIdentify()->API.identifySet("language", language)
    getInstance()->API.identify(identifyInstance)
  }

  let setItemCount = (~itemCount) => {
    let identifyInstance = API.getIdentify()->API.identifySetInt("itemCount", itemCount)
    getInstance()->API.identify(identifyInstance)
  }

  let addExperimentBucket = (~experimentId, ~bucketId) => {
    let identifyInstance =
      API.getIdentify()->API.identifyAppend("experiments", experimentId ++ ("__" ++ bucketId))
    getInstance()->API.identify(identifyInstance)
  }
}
