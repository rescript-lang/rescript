type t = Dom.storageEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "StorageEvent"
@new external makeWithOptions: (string, {..}) => t = "StorageEvent"

@get external key: t => string = ""
@get external newValue: t => Js.Nullable.t<string> = ""
@get external oldValue: t => Js.Nullable.t<string> = ""
@get external storageArea: t => Dom.Storage.t = ""
@get external url: t => string = ""
