type t = Dom.history
type state /* TODO: should be "anything that can be serializable" apparently */

@get external length: t => int = ""
@get external scrollRestoration: t => bool = "" /* experimental */
@set external setScrollRestoration: (t, bool) => unit = "scrollRestoration" /* experimental */
@get external state: t => state = ""
