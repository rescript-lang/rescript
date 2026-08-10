@react.component
let make = (~username, ~urlRest, ~url, ~showLogin) => {
  let me = UserStore.useMe()
  switch me {
  | Some(me) =>
    if String.toLowerCase(me.username) == String.toLowerCase(username) {
      <MyPage user=me urlRest url />
    } else {
      <UserViewingPage username urlRest url showLogin />
    }
  | None => <UserViewingPage username urlRest url showLogin />
  }
}
