let key = "bucket_hash"
let bucketHash = ref(
  (Dom.Storage.localStorage->Dom.Storage.getItem(key))->Belt.Option.flatMap(int_of_string_opt),
)

let triggerKey = "triggered_experiments"
let triggeredMap = ref(
  (Dom.Storage.localStorage->Dom.Storage.getItem(triggerKey))
  ->Belt.Option.map(value => {
    let json = Js.Json.parseExn(value)
    open Json.Decode
    dict(string, json)
  })
  ->Belt.Option.getWithDefault(Js.Dict.empty()),
)
let addTrigger = (key, value) => {
  triggeredMap.contents->Js.Dict.set(key, value)
  open Dom.Storage
  localStorage->setItem(
    triggerKey,
    Js.Json.stringify({
      open Json.Encode
      dict(string, triggeredMap.contents)
    }),
  )
}

exception UnexpectedExperimentId(string)
let getBucketHash = () =>
  switch bucketHash.contents {
  | Some(bucketHash) => bucketHash
  | None =>
    let value = Js.Math.random_int(0, max_int)
    bucketHash := Some(value)
    Dom.Storage.localStorage->Dom.Storage.setItem(key, string_of_int(value))
    value
  }

module ExperimentIds = {
  let matchListNotice = "524"
  let quicklistOverlay = "927"
}

let getBucketIdForExperiment = (~experimentId) =>
  if experimentId == ExperimentIds.matchListNotice {
    string_of_int(
      // skip two low bits for now
      // Use the 3rd bit (50%)
      land(lsr(getBucketHash(), 2), 1),
    )
  } else if experimentId == ExperimentIds.quicklistOverlay {
    string_of_int(land(lsr(getBucketHash(), 3), 1))
  } else {
    throw(UnexpectedExperimentId(experimentId))
  }

let trigger = (~experimentId, ~bucketId) =>
  if triggeredMap.contents->Js.Dict.get(experimentId) != Some(bucketId) {
    addTrigger(experimentId, bucketId)
    Analytics.Amplitude.addExperimentBucket(~experimentId, ~bucketId)
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Experiment Triggered",
      ~eventProperties={"experimentId": experimentId, "bucketId": bucketId},
    )
  }
