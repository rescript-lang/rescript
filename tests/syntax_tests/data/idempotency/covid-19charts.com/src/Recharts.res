module PctOrPx = {
  type t
  external px: float => t = "%identity"
  external pct: string => t = "%identity"
  let pct = float => pct(Js.Float.toString(float) ++ "%")
}

let px = PctOrPx.px
let pct = PctOrPx.pct

module Dot = {
  type t

  external bool: bool => t = "%identity"
  external element: React.element => t = "%identity"
  external obj: _ => t = "%identity"
  external render: (_ => React.element) => t = "%identity"
}

module Make = (
  Config: {
    type dataItem
  },
) => {
  module ResponsiveContainer = {
    @module("recharts") @react.component
    external make: (
      ~minHeight: float=?,
      ~height: PctOrPx.t=?,
      ~width: PctOrPx.t=?,
      ~children: React.element=?,
    ) => React.element = "ResponsiveContainer"
  }
  module LineChart = {
    @module("recharts") @react.component
    external make: (
      ~margin: _=?,
      ~data: array<Config.dataItem>=?,
      ~children: React.element=?,
    ) => React.element = "LineChart"
  }
  module Line = {
    type yValue
    external int: int => yValue = "%identity"
    external float: float => yValue = "%identity"
    external toInt: yValue => int = "%identity"
    external toFloat: yValue => float = "%identity"
    @module("recharts") @react.component
    external make: (
      ~_type: @string
      [
        | #basis
        | #basisClosed
        | #basisOpen
        | #linear
        | #linearClosed
        | #natural
        | #monotoneX
        | #monotoneY
        | #monotone
        | #step
        | #stepBefore
        | #stepAfter
      ]=?,
      ~dataKey: Config.dataItem => Js.null<yValue>,
      ~stroke: string=?,
      ~strokeWidth: float=?,
      ~strokeDasharray: string=?,
      ~children: React.element=?,
      ~dot: Dot.t=?,
      ~activeDot: Dot.t=?,
      ~label: string=?,
      ~name: string=?,
      ~connectNulls: bool=?,
    ) => React.element = "Line"
  }
  module XAxis = {
    type value
    external int: int => value = "%identity"
    external float: float => value = "%identity"
    external string: string => value = "%identity"

    type label
    external obj: _ => label = "%identity"
    external bool: bool => label = "%identity"

    @module("recharts") @react.component
    external make: (
      ~_type: @string [#number | #category]=?,
      ~padding: _=?,
      ~dataKey: Config.dataItem => value,
      ~axisLine: bool=?,
      ~tickLine: bool=?,
      ~interval: string=?,
      ~minTickGap: float=?,
      ~label: label=?,
      ~tick: bool=?,
      ~children: React.element=?,
    ) => React.element = "XAxis"
  }
  module YAxis = {
    type domain
    external int: int => domain = "%identity"
    external string: string => domain = "%identity"
    @module("recharts") @react.component
    external make: (
      ~_type: @string [#number]=?,
      ~scale: @string [#log | #linear]=?,
      ~domain: (domain, domain)=?,
      ~axisLine: bool=?,
      ~tickLine: bool=?,
      ~tick: bool=?,
      ~tickFormatter: Line.yValue => string=?,
    ) => React.element = "YAxis"
  }
  module Tooltip = {
    type payload = {
      stroke: string,
      name: string,
      value: Line.yValue,
      dataKey: string,
      payload: Config.dataItem,
    }

    type data = {
      payload: Js.null<array<payload>>,
      label: string,
      separator: string,
    }
    @module("recharts") @react.component
    external make: (~content: data => React.element=?) => React.element = "Tooltip"
  }
  module CartesianGrid = {
    @module("recharts") @react.component
    external make: (~strokeDasharray: string=?) => React.element = "CartesianGrid"
  }
  module Label = {
    @module("recharts") @react.component
    external make: (
      ~style: _=?,
      ~value: string=?,
      ~position: string=?,
      ~offset: float=?,
    ) => React.element = "Label"
  }
}
