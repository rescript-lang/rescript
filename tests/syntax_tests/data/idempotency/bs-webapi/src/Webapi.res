type rafId

@val external requestAnimationFrame: (float => unit) => unit = ""
@val external requestCancellableAnimationFrame: (float => unit) => rafId = "requestAnimationFrame"
@val external cancelAnimationFrame: rafId => unit = ""

module Base64 = Webapi__Base64
module Canvas = Webapi__Canvas
module Dom = Webapi__Dom
module File = Webapi__File
module Performance = Webapi__Performance
module Url = Webapi__Url
module ResizeObserver = Webapi__ResizeObserver
