module NotificationProvider = {
  let dispatchNotificationContext = React.createContext((
    ~action as _: option<string>=?,
    _message: string,
  ) => ())

  module Provider = {
    let make = React.Context.provider(dispatchNotificationContext)
  }

  @react.component
  let make = (~children) => {
    let dispatchNotification = (~action=?, message) =>
      switch action {
      | Some(action) => Console.log2(message, action)
      | None => Console.log(message)
      }

    <Provider value=dispatchNotification> {children} </Provider>
  }

  let useNotification = () => React.useContext(dispatchNotificationContext)
}

module ComponentUsingAction = {
  @react.component
  let make = () => {
    let dispatchNotification = NotificationProvider.useNotification()

    React.useEffect(() => {
      dispatchNotification(~action="Some action", "This is a notification message")
      None
    }, [])

    React.null
  }
}

module ComponentNotUsingAction = {
  @react.component
  let make = () => {
    let dispatchNotification = NotificationProvider.useNotification()

    React.useEffect(() => {
      dispatchNotification("This is a notification message")
      None
    }, [])

    React.null
  }
}

@live
@react.component
let make = () => {
  <NotificationProvider>
    <ComponentUsingAction />
    <ComponentNotUsingAction />
  </NotificationProvider>
}
