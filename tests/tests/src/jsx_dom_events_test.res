@@config({flags: ["-bs-jsx", "4"]})

// Check the new handlers and capture variants through JSX.
let handlers =
  <div
    onFullscreenChange={_ => ()}
    onFullscreenChangeCapture={_ => ()}
    onFullscreenError={_ => ()}
    onFullscreenErrorCapture={_ => ()}
    onCancel={_ => ()}
    onCancelCapture={_ => ()}
    onClose={_ => ()}
    onCloseCapture={_ => ()}
    onBeforeToggle={_ => ()}
    onBeforeToggleCapture={_ => ()}
    onToggle={_ => ()}
    onToggleCapture={_ => ()}
    onCopyCapture={_ => ()}
    onCutCapture={_ => ()}
    onPasteCapture={_ => ()}
    onCompositionEndCapture={_ => ()}
    onCompositionStartCapture={_ => ()}
    onCompositionUpdateCapture={_ => ()}
    onKeyDownCapture={_ => ()}
    onKeyPressCapture={_ => ()}
    onKeyUpCapture={_ => ()}
    onFocusCapture={_ => ()}
    onBlurCapture={_ => ()}
    onBeforeInputCapture={_ => ()}
    onChangeCapture={_ => ()}
    onInputCapture={_ => ()}
    onResetCapture={_ => ()}
    onSubmitCapture={_ => ()}
    onInvalidCapture={_ => ()}
    onAuxClick={_ => ()}
    onAuxClickCapture={_ => ()}
    onClickCapture={_ => ()}
    onContextMenuCapture={_ => ()}
    onDoubleClickCapture={_ => ()}
    onDragCapture={_ => ()}
    onDragEndCapture={_ => ()}
    onDragEnterCapture={_ => ()}
    onDragExitCapture={_ => ()}
    onDragLeaveCapture={_ => ()}
    onDragOverCapture={_ => ()}
    onDragStartCapture={_ => ()}
    onDropCapture={_ => ()}
    onMouseDownCapture={_ => ()}
    onMouseMoveCapture={_ => ()}
    onMouseOutCapture={_ => ()}
    onMouseOverCapture={_ => ()}
    onMouseUpCapture={_ => ()}
    onSelectCapture={_ => ()}
    onTouchCancelCapture={_ => ()}
    onTouchEndCapture={_ => ()}
    onTouchMoveCapture={_ => ()}
    onTouchStartCapture={_ => ()}
    onPointerOverCapture={_ => ()}
    onPointerDownCapture={_ => ()}
    onPointerMoveCapture={_ => ()}
    onPointerUpCapture={_ => ()}
    onPointerCancelCapture={_ => ()}
    onPointerOutCapture={_ => ()}
    onGotPointerCapture={_ => ()}
    onGotPointerCaptureCapture={_ => ()}
    onLostPointerCapture={_ => ()}
    onLostPointerCaptureCapture={_ => ()}
    onScrollCapture={_ => ()}
    onScrollEnd={_ => ()}
    onScrollEndCapture={_ => ()}
    onWheelCapture={_ => ()}
    onAbortCapture={_ => ()}
    onCanPlayCapture={_ => ()}
    onCanPlayThroughCapture={_ => ()}
    onDurationChangeCapture={_ => ()}
    onEmptiedCapture={_ => ()}
    onEncryptedCapture={_ => ()}
    onEndedCapture={_ => ()}
    onErrorCapture={_ => ()}
    onLoadedDataCapture={_ => ()}
    onLoadedMetadataCapture={_ => ()}
    onLoadStartCapture={_ => ()}
    onPauseCapture={_ => ()}
    onPlayCapture={_ => ()}
    onPlayingCapture={_ => ()}
    onProgressCapture={_ => ()}
    onRateChangeCapture={_ => ()}
    onResize={_ => ()}
    onResizeCapture={_ => ()}
    onSeekedCapture={_ => ()}
    onSeekingCapture={_ => ()}
    onStalledCapture={_ => ()}
    onSuspendCapture={_ => ()}
    onTimeUpdateCapture={_ => ()}
    onVolumeChangeCapture={_ => ()}
    onWaitingCapture={_ => ()}
    onLoadCapture={_ => ()}
    onAnimationStartCapture={_ => ()}
    onAnimationEndCapture={_ => ()}
    onAnimationIterationCapture={_ => ()}
    onTransitionCancel={_ => ()}
    onTransitionCancelCapture={_ => ()}
    onTransitionRun={_ => ()}
    onTransitionRunCapture={_ => ()}
    onTransitionStart={_ => ()}
    onTransitionStartCapture={_ => ()}
    onTransitionEndCapture={_ => ()}
  />

let onToggle = (event: JsxEvent.Toggle.t) => {
  let oldState = event->JsxEvent.Toggle.oldState
  let newState = event->JsxEvent.Toggle.newState
  switch (oldState, newState) {
  | (#closed, #"open") => event->JsxEvent.Toggle.preventDefault
  | (#"open", #closed) => event->JsxEvent.Synthetic.stopPropagation
  | (#closed, #closed) | (#"open", #"open") => ()
  }
}

let dialog = <dialog onToggle onBeforeToggle=onToggle />
let details = <details onToggle />
let popover = <div popover=Auto onToggle />

let toggleSource = (event: JsxEvent.Toggle.t): option<Dom.element> => event->JsxEvent.Toggle.source
