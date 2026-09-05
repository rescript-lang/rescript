type orientation =
  | Columns
  | Rows

type state = {
  columnRatio: float,
  rowRatio: float,
  orientation: orientation,
  dragging: bool,
}

type drag = {
  pointerId: int,
  orientation: orientation,
  size: float,
  minFirst: float,
  minSecond: float,
}

type t = {
  state: Signal.t<state>,
  drag: ref<option<drag>>,
  containerId: string,
}

let separatorSize = 8.0
let defaultRatio = 0.5
let keyboardStep = 0.025
let narrowWidth = 900.0

let nextInstanceId = ref(0)

let nextContainerId = () => {
  nextInstanceId := nextInstanceId.contents + 1
  `pane-layout-${nextInstanceId.contents->Int.toString}`
}

let make = () => {
  state: Signal.make({
    columnRatio: defaultRatio,
    rowRatio: defaultRatio,
    orientation: Columns,
    dragging: false,
  }),
  drag: ref(None),
  containerId: nextContainerId(),
}

let current = layout => Signal.peek(layout.state)

let orientationForWidth = width => width <= narrowWidth ? Rows : Columns

let clamp = (value, minimum, maximum) =>
  if value < minimum {
    minimum
  } else if value > maximum {
    maximum
  } else {
    value
  }

let ratioBounds = (~size, ~minFirst, ~minSecond) => {
  let available = size - separatorSize
  if available <= 0.0 {
    (defaultRatio, defaultRatio)
  } else {
    let minimum = minFirst /. available
    let maximum = 1.0 -. minSecond /. available
    minimum > maximum ? (defaultRatio, defaultRatio) : (minimum, maximum)
  }
}

let clampRatio = (ratio, ~size, ~minFirst, ~minSecond) => {
  let (minimum, maximum) = ratioBounds(~size, ~minFirst, ~minSecond)
  clamp(ratio, minimum, maximum)
}

let ratioForPosition = (position, ~size, ~minFirst, ~minSecond) => {
  let available = size - separatorSize
  let ratio = available <= 0.0 ? defaultRatio : (position -. separatorSize /. 2.0) /. available
  clampRatio(ratio, ~size, ~minFirst, ~minSecond)
}

let setRatio = (layout, orientation, ratio) =>
  Signal.update(layout.state, state =>
    switch orientation {
    | Columns => {...state, columnRatio: ratio}
    | Rows => {...state, rowRatio: ratio}
    }
  )

let setOrientation = (layout, orientation) => {
  let state = current(layout)
  if state.orientation !== orientation {
    layout.drag := None
    Signal.set(layout.state, {...state, orientation, dragging: false})
  }
}

let startDrag = (layout, ~pointerId, ~position, ~size, ~minFirst, ~minSecond) => {
  let orientation = current(layout).orientation
  layout.drag := Some({pointerId, orientation, size, minFirst, minSecond})
  setRatio(layout, orientation, ratioForPosition(position, ~size, ~minFirst, ~minSecond))
  Signal.update(layout.state, state => {...state, dragging: true})
}

let moveDrag = (layout, ~pointerId, ~position) =>
  switch layout.drag.contents {
  | Some(drag) if drag.pointerId === pointerId =>
    setRatio(
      layout,
      drag.orientation,
      ratioForPosition(
        position,
        ~size=drag.size,
        ~minFirst=drag.minFirst,
        ~minSecond=drag.minSecond,
      ),
    )
  | Some(_) | None => ()
  }

let finishDrag = (layout, ~pointerId) =>
  switch layout.drag.contents {
  | Some(drag) if drag.pointerId === pointerId => {
      layout.drag := None
      Signal.update(layout.state, state => {...state, dragging: false})
    }
  | Some(_) | None => ()
  }

let reset = layout => {
  let state = current(layout)
  setRatio(layout, state.orientation, defaultRatio)
}

let nudge = (layout, delta, ~size, ~minFirst, ~minSecond) => {
  let state = current(layout)
  let ratio = switch state.orientation {
  | Columns => state.columnRatio
  | Rows => state.rowRatio
  }
  setRatio(layout, state.orientation, clampRatio(ratio +. delta, ~size, ~minFirst, ~minSecond))
}

let activeRatio = (state: state) =>
  switch state.orientation {
  | Columns => state.columnRatio
  | Rows => state.rowRatio
  }

let workspaceClass = (state: state) => {
  let orientationClass = switch state.orientation {
  | Columns => "workspace-columns"
  | Rows => "workspace-rows"
  }
  state.dragging ? `workspace ${orientationClass} is-resizing` : `workspace ${orientationClass}`
}

let workspaceStyle = (state: state) => {
  let firstColumn = state.columnRatio->Float.toString
  let secondColumn = (1.0 -. state.columnRatio)->Float.toString
  let firstRow = state.rowRatio->Float.toString
  let secondRow = (1.0 -. state.rowRatio)->Float.toString
  `--pane-first-column: ${firstColumn}fr; --pane-second-column: ${secondColumn}fr; --pane-first-row: ${firstRow}fr; --pane-second-row: ${secondRow}fr;`
}

let ariaOrientation = (state: state) =>
  switch state.orientation {
  | Columns => "vertical"
  | Rows => "horizontal"
  }

let ariaValueNow = (state: state) =>
  (activeRatio(state) *. 100.0)->Math.round->Float.toInt->Int.toString
