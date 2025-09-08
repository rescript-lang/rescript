@@config({jsx: 3})

@deriving({jsConverter: newType})
type source<'a> = {
  subscribe: (unit => unit, unit) => unit,
  getCurrentValue: unit => 'a,
}

@module("use-subscription")
external useSubscriptionJs: abs_source<'a> => 'a = "useSubscription"

let useSubscription = source => {
  let sourceJs = React.useMemo1(() => sourceToJs(source), [source])
  useSubscriptionJs(sourceJs)
}
