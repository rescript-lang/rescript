type propField<'a> = {.}
type propField<'a> = {..} as 'a
type propField<'a> = {..} as 'a
type propField<'a> = nullable<{..} as 'a>

type propField<'a> = {"a": b}
type propField<'a> = {.."a": b}
type propField<'a> = {"a": {"b": c}}
