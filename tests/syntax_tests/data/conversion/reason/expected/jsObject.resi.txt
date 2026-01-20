type propField<'a> = {.}
type propField<'a> = {..} as 'a
type propField<'a> = {..} as 'a
type propField<'a> = Js.nullable<{..} as 'a>

type propField<'a> = {"a": b}
type propField<'a> = {.."a": b}
type propField<'a> = {"a": {"b": c}}
