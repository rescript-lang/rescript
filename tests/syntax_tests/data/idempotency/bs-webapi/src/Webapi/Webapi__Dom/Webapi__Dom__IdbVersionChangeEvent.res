type t = Dom.idbVersionChangeEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "IDBVersionChangeEvent"
@new external makeWithOptions: (string, {..}) => t = "IDBVersionChangeEvent"

@get external oldVersion: t => int = ""
@get external newVersion: t => int = ""
