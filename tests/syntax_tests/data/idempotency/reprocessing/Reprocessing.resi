@@ocaml.doc(" # Reprocessing
 *
 * ## The Run Function
 *
 * This is how all of your programs start. At minimum you need a setup function, but it also doesn't make much sense to not have a draw function.
 *
 * @doc run
 *
 * ## Hot reloading
 *
 * Put the following in an `indexhot.re`
 *
 * ```reason;no-run
 * Reprocessing.hotreload(\"./index.re\")
 * ```
 *
 * @doc hotreload
 *
 * ## Misc helpers
 *
 * The following modules are included here as a psuedo namespacing function. It is common to `open Reprocessing` and then access them as `Draw.rect`, etc.
 *
 * @doc Utils, Constants, Draw, Env, Events
 *
 * ## Handling Multiple Canvases
 *
 * (only supported on web target)
 *
 * @doc setScreenId, clearScreenId, playPause
 *
 * @includes
")

/* ** Contains functions having to do with drawing to the screen */
module Draw = Reprocessing_Draw

/* ** Contains functions having to do with the environment:
 * ie window properties, user input
 */
module Env = Reprocessing_Env

/* ** Contains types for events. */
module Events = Reprocessing_Events

/* ** Contains utility functions */
module Utils = Reprocessing_Utils

/* ** Contains useful constants */
module Constants = Reprocessing_Constants

include Reprocessing_Types.TypesT

let hotreload: (~screen: string=?, string) => bool

@ocaml.doc(" Set the ID that will be used by subsequent calls to `run()` that don't have an explicitly-passed `~screen`.
 *
 * If a canvas exists on the document with the given ID, then that canvas will be used. Otherwise a canvas will be created & appended to the body.
 *
 * ```no-run
 * # let _ = (setup, draw) => {
 * Reprocessing.setScreenId(\"my-fancy-id\");
 * /* This will render to the canvas with id \"my-fancy-id\" */
 * Reprocessing.run(~setup, ~draw, ());
 * # }
 * ```
")
let setScreenId: string => unit
let clearScreenId: unit => unit

@ocaml.doc(" Play/pause the screen specified by the given ID. If you pass true, it will try to play it, otherwise pause.
 *
 * The return value indicates the status:
 * - None: no screen found
 * - Some(true): the screen is (now/still) playing
 * - Some(false): the screen is (now/still) paused
 *
 * Calling this function will not necessarily change the state. Inspect the result to determine success.
 ")
let playPause: (string, bool) => option<bool>

/* ** Entrypoint to the graphics library. The system
 * is designed for you to return a self-defined 'state'
 * object from setup, which will then be passed to every
 * callback you choose to implement. Updating the state
 * is done by returning a different value from the callback.
 */
let run: (
  ~setup: glEnvT => 'a,
  ~screen: string=?,
  ~draw: ('a, glEnvT) => 'a=?,
  ~mouseMove: ('a, glEnvT) => 'a=?,
  ~mouseDragged: ('a, glEnvT) => 'a=?,
  ~mouseDown: ('a, glEnvT) => 'a=?,
  ~mouseUp: ('a, glEnvT) => 'a=?,
  ~keyPressed: ('a, glEnvT) => 'a=?,
  ~keyReleased: ('a, glEnvT) => 'a=?,
  ~keyTyped: ('a, glEnvT) => 'a=?,
  unit,
) => unit
