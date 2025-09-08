@react.component
let make = (~username, ~urlRest, ~url, ~showLogin) => {
  let me = UserStore.useMe()
  switch me {
  | Some(me) =>
    if Js.String.toLowerCase(me.username) == Js.String.toLowerCase(username) {
      <MyPage user=me urlRest url />
    } else {
      <UserViewingPage username urlRest url showLogin />
    }
  | None => <UserViewingPage username urlRest url showLogin />
  }
}
