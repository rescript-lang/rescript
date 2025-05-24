let viewportThreshold = 450

module Styles = {
  open Css
  let mobileOverlay = style(list{
    media(
      "(max-width: 450px)",
      list{
        position(fixed),
        backgroundColor(hex("ffffff80")),
        bottom(zero),
        top(zero),
        left(zero),
        right(zero),
      },
    ),
  })
  let root = style(list{
    position(fixed),
    bottom(zero),
    left(px(32)),
    width(px(288)),
    borderTopLeftRadius(px(8)),
    borderTopRightRadius(px(8)),
    overflow(hidden),
    backgroundColor(Colors.green),
    color(Colors.white),
    transition(~duration=250, "all"),
    transform(translate3d(zero, pct(100.), zero)),
    media(
      "(max-width: 450px)",
      list{backgroundColor(hex("fffffff0")), left(zero), right(zero), width(auto)},
    ),
  })
  let rootWithQuicklist = style(list{Colors.darkLayerShadow})
  let introBar = style(list{
    display(flexBox),
    justifyContent(spaceBetween),
    alignItems(center),
    padding2(~v=px(8), ~h=px(8)),
    height(px(33)),
    media(
      "(max-width: 450px)",
      list{
        paddingTop(px(16)),
        paddingBottom(px(16)),
        selector("@supports (-webkit-touch-callout: none)", list{paddingBottom(px(20))}),
      },
    ),
    transition(~duration=200, "all"),
  })
  let shown = style(list{transform(translate3d(zero, px(384), zero))})
  let button = style(list{
    backgroundColor(Colors.white),
    color(Colors.green),
    borderWidth(zero),
    borderRadius(px(4)),
    padding2(~v=px(8), ~h=px(12)),
    cursor(pointer),
    disabled(list{opacity(0.8)}),
    media("(min-width: 451px)", list{hover(list{backgroundColor(Colors.white)})}),
    media("(max-width: 450px)", list{backgroundColor(Colors.green), color(Colors.white)}),
  })
  let barLink = style(list{
    color(Colors.white),
    display(inlineBlock),
    padding2(~v=px(4), ~h=px(8)),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
    media("(max-width: 450px)", list{color(Colors.green)}),
  })
  @module("./assets/down_caret.png")
  external downCaretIcon: string = "default"
  let hidePanelLink = style(list{
    padding2(~v=zero, ~h=px(8)),
    textDecoration(none),
    display(flexBox),
    alignItems(center),
    unsafe("alignSelf", "stretch"),
    media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
  })
  let hidePanelLinkCaret = style(list{
    backgroundImage(url(downCaretIcon)),
    backgroundSize(cover),
    display(inlineBlock),
    height(px(16)),
    width(px(16)),
    verticalAlign(#top),
  })
  let shownPanel = style(list{
    boxShadow(Shadow.box(~blur=px(24), hex("00000040"))),
    transform(translate3d(zero, zero, zero)),
    backgroundColor(Colors.white),
    color(Colors.charcoal),
    selector("& ." ++ barLink, list{color(Colors.linkGreen)}),
    selector("& ." ++ introBar, list{borderBottom(px(1), solid, Colors.faintGray)}),
    media(
      "(max-width: 450px)",
      list{
        boxShadow(Shadow.box(~blur=px(16), hex("00000040"))),
        selector("& ." ++ introBar, list{paddingTop(px(8)), paddingBottom(px(8))}),
      },
    ),
  })
  let body = style(list{
    height(px(384)),
    boxSizing(borderBox),
    display(flexBox),
    flexDirection(column),
  })
  let bodyListWrapper = style(list{
    flexGrow(1.),
    overflow(hidden),
    position(relative),
    display(flexBox),
    flexDirection(column),
  })
  let bodyList = style(list{height(pct(100.)), overflow(auto)})
  let listImages = style(list{
    display(flexBox),
    flexWrap(wrap),
    padding(px(16)),
    position(relative),
  })
  let listImagesScrollFade = style(list{
    backgroundImage(
      linearGradient(deg(0.), list{(zero, hex("ffffffff")), (pct(100.), hex("ffffff00"))}),
    ),
    position(absolute),
    bottom(zero),
    left(zero),
    right(zero),
    height(px(16)),
  })
  let listImage = style(list{
    display(block),
    width(px(64)),
    height(px(64)),
    cursor(pointer),
    media("(hover: hover)", list{hover(list{backgroundColor(Colors.faintGray)})}),
  })
  let emptyList = style(list{paddingTop(px(32)), textAlign(center), fontSize(px(20))})
  let saveRow = style(list{
    backgroundColor(Colors.white),
    display(flexBox),
    flexDirection(column),
    flexShrink(0.),
    padding3(~top=px(8), ~h=px(16), ~bottom=px(16)),
  })
  let mobileRemoveMessage = style(list{
    display(none),
    media(
      "(max-width: 450px)",
      list{display(block), paddingBottom(px(8)), color(Colors.gray), textAlign(center)},
    ),
  })
  let saveButton = style(list{
    padding2(~v=px(12), ~h=zero),
    backgroundColor(Colors.green),
    borderWidth(zero),
    fontSize(px(16)),
    color(Colors.white),
    borderRadius(px(10)),
    width(pct(100.)),
  })
}

type visibility =
  | Hidden
  | Bar
  | Panel

module CreateDialog = {
  module Styles = {
    open Css
    let body = style(list{
      padding(px(16)),
      width(px(384)),
      boxSizing(borderBox),
      minHeight(px(96)),
      maxWidth(vw(90.)),
    })
    let title = style(list{fontSize(px(24)), marginBottom(px(16))})
    let listUrl = style(list{
      width(pct(100.)),
      padding2(~v=px(12), ~h=px(8)),
      backgroundColor(transparent),
      border(px(2), dashed, hex("bae8cc")),
      borderRadius(px(4)),
      textAlign(center),
      cursor(pointer),
      outlineStyle(none),
      margin2(~v=px(16), ~h=zero),
      transition(~duration=300, "all"),
    })
    let listUrlCopied = style(list{borderColor(hex("3cb56c"))})
  }

  @module external copyToClipboard: string => unit = "copy-to-clipboard"

  module ConfirmModal = {
    @react.component
    let make = (~listId, ~onClose) => {
      let (hasCopied, setHasCopied) = React.useState(() => false)
      <Modal>
        <div className=Styles.body>
          <div className=Styles.title> {React.string("List created!")} </div>
          {
            let url =
              {
                open Webapi.Dom
                location->Location.origin
              } ++
              ("/l/" ++
              listId)
            <>
              <div>
                {React.string("Tap to copy or ")}
                <a
                  href=url
                  target="_blank"
                  onClick={_ =>
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Item List Opened in New Tab",
                      ~eventProperties={"listId": listId},
                    )}>
                  {React.string("open in new tab")}
                </a>
                {React.string(".")}
              </div>
              {if UserStore.isLoggedIn() {
                <div>
                  {React.string(" You can find this on your ")}
                  <Link path="/lists" onClick={() => onClose()}>
                    {React.string("custom lists page")}
                  </Link>
                  {React.string(".")}
                </div>
              } else {
                React.null
              }}
              <button
                className={Cn.make(list{
                  Styles.listUrl,
                  Cn.ifTrue(Styles.listUrlCopied, hasCopied),
                })}
                onClick={_ => {
                  copyToClipboard(url)
                  Analytics.Amplitude.logEventWithProperties(
                    ~eventName="Item List Copied to Clipboard",
                    ~eventProperties={"listId": listId},
                  )
                  setHasCopied(_ => true)
                }}>
                {React.string(url)}
              </button>
              <div>
                {React.string("We are working on this feature. Please ")}
                <a href="https://twitter.com/nookexchange" target="_blank">
                  {React.string("share your feedback")}
                </a>
                {React.string("!")}
              </div>
            </>
          }
        </div>
        // {if (UserStore.isLoggedIn()) {
        //    <div>
        //      {React.string(
        //         {j|Lists are saved to your account. The page is coming soon 🙃|j},
        //       )}
        //    </div>;
        //  } else {
        //    <div>
        //      {React.string("Create an account to save your lists!")}
        //    </div>;
        //  }}
        <Modal.FooterBar>
          <Button onClick={_ => onClose()}> {React.string("Okay")} </Button>
        </Modal.FooterBar>
      </Modal>
    }
  }

  let show = (~listId) => {
    let modalKey = ref(None)
    let onClose = () => ReactAtmosphere.API.removeLayer(~key=Belt.Option.getExn(modalKey.contents))
    modalKey := Some(ReactAtmosphere.API.pushLayer(~render=_ => <ConfirmModal listId onClose />))
  }
}

@react.component
let make = () => {
  let me = UserStore.useMe()
  let quicklist = QuicklistStore.useQuicklist()
  let (visibility, setVisibility) = React.useState(() => Bar)
  let (isSubmitting, setIsSubmitting) = React.useState(() => false)

  if visibility == Panel && quicklist == None {
    setVisibility(_ => Bar)
  }
  let listId = quicklist->Belt.Option.flatMap(quicklist => quicklist.id)
  React.useEffect1(() => {
    if listId != None {
      setVisibility(_ => Panel)
    }
    None
  }, [listId])

  let url = ReasonReactRouter.useUrl()
  let viewportWidth = Utils.useViewportWidth()
  let isShown =
    quicklist != None ||
      (visibility != Hidden &&
        switch url.path {
        | list{"u", username, ..._} =>
          switch me {
          | Some(me) => me.username != username || Js.Dict.keys(me.items)->Js.Array.length >= 8
          | None => true
          }
        | _ => false
        })
  <>
    {visibility == Panel
      ? <div onClick={_ => setVisibility(_ => Bar)} className=Styles.mobileOverlay />
      : React.null}
    <div
      className={Cn.make(list{
        Styles.root,
        Cn.ifTrue(Styles.shown, isShown),
        Cn.ifTrue(Styles.shownPanel, visibility == Panel),
        Cn.ifTrue(Styles.rootWithQuicklist, quicklist != None),
      })}>
      <div className=Styles.introBar>
        {visibility == Panel
          ? <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                Analytics.Amplitude.logEvent(~eventName="Item List Overlay Closed")
                setVisibility(_ => Bar)
              }}
              className=Styles.hidePanelLink>
              <span className=Styles.hidePanelLinkCaret /> {React.string("Close")}
            </a>
          : switch quicklist {
            | Some(quicklist) =>
              let numItems = quicklist.itemIds->Js.Array.length
              <button
                onClick={_ => {
                  setVisibility(_ => Panel)
                  Analytics.Amplitude.logEventWithProperties(
                    ~eventName="Item List Overlay Shown",
                    ~eventProperties={"numItems": numItems},
                  )
                }}
                disabled={Js.Array.length(quicklist.itemIds) == 0}
                className=Styles.button>
                {React.string(
                  numItems > 0
                    ? "See " ++ (string_of_int(numItems) ++ (" item" ++ (numItems == 1 ? "" : "s")))
                    : "Select items",
                )}
              </button>
            | None =>
              <button onClick={_ => QuicklistStore.startList()} className=Styles.button>
                {React.string("Start a list")}
              </button>
            }}
        {switch quicklist {
        | Some(quicklist) =>
          switch quicklist.id {
          | Some(listId) =>
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                QuicklistStore.removeList()
                setVisibility(_ => Bar)
              }}
              className=Styles.barLink>
              {React.string("Stop editing list")}
            </a>
          | None =>
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                let numItems = Js.Array.length(quicklist.itemIds)
                if numItems > 1 {
                  ConfirmDialog.confirm(
                    ~bodyText="You have " ++
                    (string_of_int(numItems) ++
                    " items that will be discarded. Are you sure?"),
                    ~confirmLabel="Yes, discard list",
                    ~cancelLabel="Not yet",
                    ~onConfirm=() => {
                      QuicklistStore.removeList()
                      Analytics.Amplitude.logEventWithProperties(
                        ~eventName="Item List Removed",
                        ~eventProperties={"numItems": numItems},
                      )
                    },
                    (),
                  )
                } else {
                  QuicklistStore.removeList()
                  Analytics.Amplitude.logEventWithProperties(
                    ~eventName="Item List Removed",
                    ~eventProperties={"numItems": numItems},
                  )
                }
                setVisibility(_ => Bar)
              }}
              className=Styles.barLink>
              {React.string("Discard list")}
            </a>
          }
        | None =>
          <a
            href="#"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e)
              setVisibility(_ => Hidden)
              Analytics.Amplitude.logEvent(~eventName="Item List Overlay Hidden")
            }}
            className=Styles.barLink>
            {React.string("Hide")}
          </a>
        }}
      </div>
      <div className=Styles.body>
        <div className=Styles.bodyListWrapper>
          <div className=Styles.bodyList>
            {switch quicklist {
            | Some(quicklist) =>
              if Js.Array.length(quicklist.itemIds) > 0 {
                <div className=Styles.listImages>
                  {quicklist.itemIds
                  ->Belt.Array.map(((itemId, variant)) => {
                    let item = Item.getItem(~itemId)
                    if viewportWidth > viewportThreshold {
                      <ReactAtmosphere.Tooltip
                        text={React.string(
                          Item.getName(item) ++
                          ((
                            Item.getNumVariations(~item) > 1
                              ? " (" ++
                                (Item.getVariantName(
                                  ~item,
                                  ~variant,
                                  ~hidePattern=true,
                                  (),
                                )->Belt.Option.getExn ++
                                ")")
                              : ""
                          ) ++
                          "\nTap to remove"),
                        )}
                        options={
                          placement: Some("top"),
                          modifiers: Some([
                            {
                              "name": "offset",
                              "options": {
                                "offset": [0, 4],
                              },
                            },
                          ]),
                        }
                        key={string_of_int(itemId) ++ string_of_int(variant)}>
                        {({onMouseEnter, onMouseLeave, ref}) =>
                          <img
                            src={Item.getImageUrl(~item, ~variant)}
                            className=Styles.listImage
                            onMouseEnter
                            onMouseLeave
                            onClick={_ => QuicklistStore.removeItem(~itemId, ~variant)}
                            ref={ReactDOMRe.Ref.domRef(ref)}
                          />}
                      </ReactAtmosphere.Tooltip>
                    } else {
                      <img
                        src={Item.getImageUrl(~item, ~variant)}
                        className=Styles.listImage
                        onClick={_ => QuicklistStore.removeItem(~itemId, ~variant)}
                        key={string_of_int(itemId) ++ string_of_int(variant)}
                      />
                    }
                  })
                  ->React.array}
                </div>
              } else {
                <div className=Styles.emptyList> {React.string("Your list is empty!")} </div>
              }
            | None => React.null
            }}
          </div>
          <div className=Styles.listImagesScrollFade />
        </div>
        <div className=Styles.saveRow>
          <div className=Styles.mobileRemoveMessage> {React.string("Tap item to remove")} </div>
          {switch quicklist->Belt.Option.flatMap(quicklist => quicklist.id) {
          | None =>
            <button
              onClick={_ =>
                {
                  setIsSubmitting(_ => true)
                  %Repromise({
                    let listId = QuicklistStore.saveList()
                    setIsSubmitting(_ => false)
                    setVisibility(_ => Bar)
                    CreateDialog.show(~listId)
                    QuicklistStore.removeList()
                    Promise.resolved()
                  })
                }->ignore}
              disabled={switch quicklist {
              | Some(quicklist) => Js.Array.length(quicklist.itemIds) == 0 || isSubmitting
              | None => true
              }}
              className=Styles.saveButton>
              {React.string("Save and share this list!")}
            </button>
          | Some(listId) =>
            <button
              onClick={_ => {
                ReasonReactRouter.push("/l/" ++ listId)
                setVisibility(_ => Hidden)
                QuicklistStore.removeList()
              }}
              disabled=isSubmitting
              className=Styles.saveButton>
              {React.string("See list")}
            </button>
          }}
        </div>
      </div>
    </div>
  </>
}
