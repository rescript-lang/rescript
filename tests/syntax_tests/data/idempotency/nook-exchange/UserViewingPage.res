module Styles = {
  open Css
  let root = style(list{})
  let emptyProfile = style(list{
    maxWidth(px(768)),
    margin3(~top=px(32), ~bottom=px(128), ~h=auto),
    textAlign(center),
  })
  let username = style(list{fontSize(px(32)), textAlign(center), marginBottom(px(24))})
  let usernameLink = style(list{
    color(Colors.charcoal),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
  })
  let userBodySpacer = style(list{height(px(16))})
  let userBody = style(list{
    backgroundColor(hex("ffffffc0")),
    boxSizing(borderBox),
    lineHeight(px(20)),
    margin3(~top=zero, ~bottom=px(48), ~h=auto),
    maxWidth(px(512)),
    padding2(~v=px(16), ~h=px(24)),
    borderRadius(px(8)),
    whiteSpace(#preLine),
    position(relative),
    media(
      "(max-width: 512px)",
      list{
        borderRadius(zero),
        padding(px(16)),
        marginBottom(zero),
        borderBottom(px(1), solid, Colors.faintGray),
      },
    ),
  })
  let bodyText = style(list{fontSize(px(18))})
  let followBlock = style(list{marginTop(px(16)), firstChild(list{marginTop(zero)})})
  let followLink = style(list{textDecoration(none)})
  let followLinkText = style(list{
    media(
      "(hover: hover)",
      list{selector("." ++ (followLink ++ ":hover &"), list{textDecoration(underline)})},
    ),
  })
}

module FollowLink = {
  type status =
    | Success
    | Error(string)

  @react.component
  let make = (~user: User.t, ~showLogin) => {
    let (status, setStatus) = React.useState(() => None)

    <div className=Styles.followBlock>
      {switch status {
      | Some(Success) =>
        <div>
          {React.string(j`🙌` ++ " Yay! You can find them on your ")}
          <Link path="/friends"> {React.string("friends page")} </Link>
          {React.string(".")}
        </div>
      | _ => <>
          <a
            href="#"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Friend Link Clicked",
                ~eventProperties={
                  "isLoggedIn": UserStore.isLoggedIn(),
                  "followeeId": user.id,
                },
              )
              if UserStore.isLoggedIn() {
                %Repromise({
                  let response = UserStore.followUser(~userId=user.id)
                  switch response {
                  | Ok() =>
                    setStatus(_ => Some(Success))
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Friend Follow Success",
                      ~eventProperties={"followeeId": user.id},
                    )
                  | Error(error) =>
                    setStatus(_ => Some(Error(error)))
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Friend Follow Failed",
                      ~eventProperties={
                        "followeeId": user.id,
                        "error": error,
                      },
                    )
                  }
                  Promise.resolved()
                })->ignore
              } else {
                showLogin()
              }
            }}
            className=Styles.followLink>
            {React.string(j`😊 `)}
            <span className=Styles.followLinkText>
              {React.string("Add " ++ (user.username ++ " to my friends"))}
            </span>
          </a>
          {switch status {
          | Some(Error("")) => <div> {React.string(j`Oh no! Something went wrong 🙁`)} </div>
          | Some(Error(error)) => <div> {React.string(error)} </div>
          | _ => React.null
          }}
        </>
      }}
    </div>
  }
}

@react.component
let make = (~username, ~urlRest, ~url: ReasonReactRouter.url, ~showLogin) => {
  let list = switch urlRest {
  | list{url} => ViewingList.urlToViewingList(url)
  | _ => None
  }
  let (user, setUser) = React.useState(() => None)
  let isMountedRef = React.useRef(true)
  let me = UserStore.useMe()
  let isLoggedIn = me != None
  let wasFollowing = React.useMemo2(() =>
    switch (me, user) {
    | (Some(me), Some(user: User.t)) =>
      switch me.followeeIds {
      | Some(followeeIds) => followeeIds->Js.Array.includes(user.id)
      | None => false
      }
    | _ => false
    }
  , (isLoggedIn, user))
  React.useEffect0(() => {
    open Webapi.Dom
    window->Window.scrollTo(0., 0.)
    Some(() => React.Ref.setCurrent(isMountedRef, false))
  })
  React.useEffect1(() => {
    %Repromise.JsExn({
      let response = Fetch.fetchWithInit(
        Constants.apiUrl ++ ("/users/" ++ username),
        Fetch.RequestInit.make(
          ~headers=Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
          }),
          (),
        ),
      )
      switch Fetch.Response.status(response) {
      | 200 =>
        %Repromise.JsExn({
          let json = Fetch.Response.json(response)
          if React.Ref.current(isMountedRef) {
            setUser(_ => Some(User.fromAPI(json)))
          }
          Promise.resolved()
        })
      | _ => Promise.resolved()
      }
    })->ignore
    None
  }, [username])
  React.useEffect0(() => {
    switch url.hash {
    | "for-trade"
    | "can-craft"
    | "wishlist" =>
      if urlRest == list{} {
        ReasonReactRouter.replace(
          "/" ++ (Js.Array.joinWith("/", Belt.List.toArray(url.path)) ++ ("/" ++ url.hash)),
        )
      }
    | _ => ()
    }
    None
  })
  <div className=Styles.root>
    <div className=Styles.username>
      {switch list {
      | Some(_) =>
        <Link path={"/u/" ++ username} className=Styles.usernameLink>
          {React.string(username)}
        </Link>
      | None => React.string(username)
      }}
    </div>
    {switch user {
    | Some(user) =>
      <div>
        {switch (user.profileText, wasFollowing) {
        | ("", true) => React.null
        | _ =>
          <div className=Styles.userBody>
            {switch user.profileText {
            | "" => React.null
            | profileText => <div> {Emoji.parseText(profileText)} </div>
            }}
            {!wasFollowing ? <FollowLink user showLogin /> : React.null}
          </div>
        }}
        {switch list {
        | Some(list) => <UserListBrowser user list url />
        | None =>
          if user.items->Js.Dict.keys->Js.Array.length > 0 {
            <UserProfileBrowser
              username
              userItems={user.items
              ->Js.Dict.entries
              ->Belt.Array.keepMapU((. (itemKey, item)) =>
                User.fromItemKey(~key=itemKey)->Belt.Option.map(x => (x, item))
              )}
              editable=false
            />
          } else {
            <div className=Styles.emptyProfile>
              <div className=Styles.bodyText>
                {React.string(j`I have no items on my profile 😞`)}
              </div>
            </div>
          }
        }}
      </div>
    | None => React.null
    }}
  </div>
}
