module WithViewer = {
  module Styles = {
    open Css
    let root = style(list{
      display(flexBox),
      flexDirection(column),
      margin2(~v=zero, ~h=auto),
      maxWidth(px(560)),
      media("(max-width: 630px)", list{marginLeft(px(16)), marginRight(px(16))}),
    })
    let bodyCard = style(list{
      maxWidth(none),
      media("(max-width: 512px)", list{marginLeft(px(-16)), marginRight(px(-16))}),
    })
    let body = style(list{
      backgroundColor(Colors.white),
      borderRadius(px(8)),
      border(px(1), solid, Colors.lightGreen),
      overflow(hidden),
    })
    let listItem = style(list{
      display(block),
      padding2(~v=px(16), ~h=px(16)),
      textDecoration(none),
      borderTop(px(1), solid, Colors.faintGray),
      firstChild(list{borderTopWidth(zero)}),
      media(
        "(hover: hover)",
        list{hover(list{backgroundColor(Colors.faintGreen), borderTopColor(Colors.lightGreen)})},
      ),
    })
    let listTitleRow = style(list{
      display(flexBox),
      alignItems(center),
      justifyContent(spaceBetween),
      marginBottom(px(16)),
    })
    let listTitle = style(list{fontSize(px(16)), color(Colors.charcoal)})
    let listNumberItems = style(list{color(Colors.lightGray)})
    let listItemImages = style(list{display(flexBox)})
    let listItemImage = style(list{width(px(64)), height(px(64)), flexShrink(0.)})
    let noLists = style(list{padding2(~v=px(16), ~h=px(16))})
    let startListFooter = style(list{
      paddingTop(px(32)),
      selector("& > a", list{textDecoration(none), hover(list{textDecoration(underline)})}),
    })
  }

  type listInfo = {
    id: string,
    createTime: Js.Date.t,
    itemIds: array<(int, int)>,
    title: option<string>,
  }

  @react.component
  let make = (~me: User.t) => {
    let (lists, setLists) = React.useState(() => None)
    React.useEffect0(() => {
      %Repromise({
        let response = API.getUserLists(~sessionId=Belt.Option.getExn(UserStore.sessionId.contents))
        %Repromise.JsExn({
          let json = Fetch.Response.json(Belt.Result.getExn(response))
          let lists = {
            open Json.Decode
            json->array(json => {
              id: json->field("id", string),
              createTime: json->field("createTime", date),
              itemIds: json->field("itemIds", array(tuple2(int, int))),
              title: (json->optional(field("title", string)))
                ->Belt.Option.flatMap(title => title == "" ? None : Some(title)),
            })
          }->Js.Array.sortInPlaceWith((a, b) =>
            int_of_float({
              open Js.Date
              getTime(b.createTime) -. getTime(a.createTime)
            })
          )
          setLists(_ => Some(lists))
          Analytics.Amplitude.logEventWithProperties(
            ~eventName="My Lists Page Viewed",
            ~eventProperties={"numLists": Js.Array.length(lists)},
          )
          Promise.resolved()
        })
      })->ignore
      None
    })
    <div className=Styles.root>
      <PageTitle title="My Custom Lists" />
      <BodyCard className=Styles.bodyCard>
        <p> {React.string("You can create and manage your custom lists here.")} </p>
        <p>
          {React.string(
            "Be sure to also try the more powerful default lists (For Trade, Wishlist, Catalog)! You can share filtered views of your profile such as ",
          )}
          <Link path={"/u/" ++ (me.username ++ "/for-trade?c=recipes")}>
            {React.string("DIY for Trade")}
          </Link>
          {React.string(", ")}
          <Link path={"/u/" ++ (me.username ++ "/wishlist?c=recipes")}>
            {React.string("DIY I want")}
          </Link>
          {React.string(", ")}
          <Link path={"/u/" ++ (me.username ++ "/catalog?orderable=&c=furniture")}>
            {React.string("Furniture I can Order")}
          </Link>
          {React.string(", ")}
          <Link path={"/u/" ++ (me.username ++ "/wishlist?q=saharah")}>
            {React.string("Saharah items I want")}
          </Link>
          {React.string(". Just use the URL!")}
        </p>
      </BodyCard>
      {switch lists {
      | Some(lists) =>
        <div>
          <div className=Styles.body>
            {lists
            ->Js.Array.map(list =>
              <Link path={"/l/" ++ list.id} className=Styles.listItem key=list.id>
                <div className=Styles.listTitleRow>
                  <div className=Styles.listTitle>
                    {switch list.title {
                    | Some(title) => Emoji.parseText(title)
                    | None => React.string("Unnamed list")
                    }}
                  </div>
                  <div className=Styles.listNumberItems>
                    {
                      let numItems = Js.Array.length(list.itemIds)
                      React.string(
                        string_of_int(numItems) ++ (" item" ++ (numItems == 1 ? "" : "s")),
                      )
                    }
                  </div>
                </div>
                <div className=Styles.listItemImages>
                  {list.itemIds
                  ->Js.Array.slice(~start=0, ~end_=8)
                  ->Js.Array.mapi(((itemId, variant), i) => {
                    let item = Item.getItem(~itemId)
                    <img
                      src={Item.getImageUrl(~item, ~variant)}
                      className=Styles.listItemImage
                      key={string_of_int(i)}
                    />
                  })
                  ->React.array}
                </div>
              </Link>
            )
            ->React.array}
            {if Js.Array.length(lists) == 0 {
              <div className=Styles.noLists>
                {React.string("You have no custom lists. ")}
                <Link path="/" onClick={() => QuicklistStore.startList()}>
                  {React.string("Start one!")}
                </Link>
              </div>
            } else {
              React.null
            }}
          </div>
          {if Js.Array.length(lists) > 0 {
            <div className=Styles.startListFooter>
              <Link path="/" onClick={() => QuicklistStore.startList()}>
                {React.string("Create new custom list")}
              </Link>
            </div>
          } else {
            React.null
          }}
        </div>
      | None => React.null
      }}
    </div>
  }
}

@react.component
let make = () => {
  let me = UserStore.useMe()

  switch me {
  | Some(me) => <WithViewer me />
  | None => React.null
  }
}
