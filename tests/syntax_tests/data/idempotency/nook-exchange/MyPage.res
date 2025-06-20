module Styles = {
  open Css
  let root = style(list{})
  let emptyProfile = style(list{
    maxWidth(px(768)),
    margin3(~top=px(32), ~bottom=px(128), ~h=auto),
    textAlign(center),
  })
  let username = style(list{fontSize(px(32)), textAlign(center), marginBottom(px(16))})
  let url = style(list{})
  let bodyText = style(list{fontSize(px(18))})
}

module ProfileTextarea = {
  module Styles = {
    open Css
    let root = style(list{display(flexBox), flexDirection(column), marginBottom(px(8))})
    let textarea = style(list{
      backgroundColor(hex("f6f6f6")),
      border(px(1), solid, transparent),
      borderRadius(px(4)),
      padding(px(7)),
      height(px(64)),
      boxSizing(borderBox),
      outlineStyle(none),
      fontSize(px(14)),
      lineHeight(px(18)),
      width(pct(100.)),
      transition(~duration=200, "all"),
      width(pct(100.)),
      placeholder(list{opacity(0.7)}),
      marginBottom(px(6)),
      focus(list{backgroundColor(Colors.white), borderColor(rgba(0, 0, 0, 0.1))}),
    })
    let profileTextPreview = style(list{
      backgroundColor(hex("f6f6f6")),
      borderRadius(px(4)),
      padding4(~top=px(8), ~bottom=px(8), ~left=px(8), ~right=px(32)),
      fontSize(px(14)),
      lineHeight(px(18)),
      marginBottom(px(6)),
      cursor(pointer),
      whiteSpace(#preLine),
      transition(~duration=200, "all"),
    })
    let updateBar = style(list{justifyContent(flexEnd)})
    let previewWrapper = style(list{
      position(relative),
      hover(list{selector("& ." ++ profileTextPreview, list{backgroundColor(hex("f0f0f0"))})}),
    })
    let editLink = style(list{
      color(hex("a0a0a0")),
      fontSize(px(12)),
      opacity(0.8),
      textDecoration(none),
      position(absolute),
      top(px(8)),
      right(px(8)),
      hover(list{opacity(1.), textDecoration(underline)}),
    })
  }

  @react.component
  let make = (~user: User.t) => {
    let (isEditing, setIsEditing) = React.useState(() => false)
    let (profileText, setProfileText) = React.useState(() => user.profileText)

    let textareaRef = React.useRef(Js.Nullable.null)
    React.useEffect1(() => {
      if isEditing {
        open Webapi.Dom
        let textareaElement = Utils.getElementForDomRef(textareaRef)
        textareaElement->Element.unsafeAsHtmlElement->HtmlElement.focus
      }
      None
    }, [isEditing])

    <div className=Styles.root>
      {isEditing || user.profileText == ""
        ? <textarea
            value=profileText
            placeholder="Add a welcome message and your Discord username!\nYou can use emoji :nmt: and :bell:"
            className=Styles.textarea
            onChange={e => {
              let value = ReactEvent.Form.target(e)["value"]
              setProfileText(_ => value)
            }}
            ref={ReactDOMRe.Ref.domRef(textareaRef)}
          />
        : <div className=Styles.previewWrapper>
            <div onClick={_ => setIsEditing(_ => true)} className=Styles.profileTextPreview>
              {Emoji.parseText(user.profileText)}
            </div>
            <a
              href="#"
              onClick={e => {
                setIsEditing(_ => true)
                ReactEvent.Mouse.preventDefault(e)
              }}
              className=Styles.editLink>
              {React.string("Edit")}
            </a>
          </div>}
      {isEditing || profileText != user.profileText
        ? <div className={Cn.make(list{UserItemNote.Styles.updateBar, Styles.updateBar})}>
            <Button
              small=true
              onClick={_ => {
                UserStore.updateProfileText(~profileText)
                setIsEditing(_ => false)
              }}>
              {React.string("Save")}
            </Button>
            <a
              href="#"
              onClick={e => {
                setProfileText(_ => user.profileText)
                setIsEditing(_ => false)
                ReactEvent.Mouse.preventDefault(e)
              }}
              className=UserItemNote.Styles.cancelLink>
              {React.string("Cancel")}
            </a>
          </div>
        : React.null}
    </div>
  }
}

@react.component
let make = (~user: User.t, ~urlRest, ~url) => {
  React.useEffect0(() => {
    open Webapi.Dom
    window->Window.scrollTo(0., 0.)
    None
  })
  let list = switch urlRest {
  | list{url} => ViewingList.urlToViewingList(url)
  | _ => None
  }
  <div className=Styles.root>
    <div className=Styles.username> {React.string(user.username)} </div>
    <BodyCard>
      <ProfileTextarea user />
      <div className=Styles.url>
        {React.string("Share your profile! ")}
        <Link path={"/u/" ++ user.username}>
          {React.string("https://nook.exchange/u/" ++ user.username)}
        </Link>
      </div>
      <div>
        {React.string("Have a lot of items to add? Try the new ")}
        <Link path="/import"> {React.string("item importer")} </Link>
        {React.string(".")}
      </div>
    </BodyCard>
    {switch list {
    | Some(list) => <UserListBrowser user list url me=true />
    | None =>
      if user.items->Js.Dict.keys->Js.Array.length > 0 {
        <UserProfileBrowser
          username=user.username
          userItems={user.items
          ->Js.Dict.entries
          ->Belt.Array.mapU((. (itemKey, item)) => (
            Belt.Option.getExn(User.fromItemKey(~key=itemKey)),
            item,
          ))}
          editable=true
        />
      } else {
        <>
          <div className=Styles.emptyProfile>
            <div className=Styles.bodyText>
              <div> {React.string("Your profile is empty!")} </div>
              <div>
                {React.string("Add items from the ")}
                <Link path="/"> {React.string("Item Browser")} </Link>
              </div>
            </div>
          </div>
        </>
      }
    }}
  </div>
}
