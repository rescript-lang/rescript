type standard
type daylight

type timezone<_> =
  | EST: timezone<standard>
  | EDT: timezone<daylight>
  | CST: timezone<standard>
  | CDT: timezone<daylight>
