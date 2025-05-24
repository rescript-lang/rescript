let smallThreshold = 500
let smallThresholdMediaQuery = styles =>
  Css.media("(max-width: " ++ (string_of_int(smallThreshold) ++ "px)"), styles)

module Styles = {
  open Css
  let backdrop = style(list{
    position(absolute),
    top(zero),
    bottom(zero),
    left(zero),
    right(zero),
    opacity(0.),
    backgroundColor(hex("80808080")),
    transition(~duration=200, "all"),
  })
  let root100VH = style(list{
    display(flexBox),
    alignItems(center),
    justifyContent(center),
    width({
      open Calc
      vw(100.) - px(16)
    }),
  })
  let root = style(list{
    backgroundColor(hex("ffffff")),
    borderRadius(px(8)),
    position(relative),
    maxWidth(px(640)),
    boxSizing(borderBox),
    boxShadow(Shadow.box(~blur=px(32), rgba(0, 0, 0, 0.2))),
    overflow(auto),
    maxHeight(pct(95.)),
    minHeight(px(256)),
    opacity(0.),
    transforms(list{scale(0.85, 0.85), translate3d(zero, zero, zero)}),
    media("(max-width: 400px)", list{maxHeight(pct(90.)), minWidth(zero)}),
    transition(~duration=200, ~timingFunction=cubicBezier(0.48, 1.38, 0.71, 0.93), "all"),
    smallThresholdMediaQuery(list{width(pct(100.))}),
  })
  let body = style(list{
    display(flexBox),
    justifyContent(center),
    padding2(~v=px(32), ~h=px(32)),
    smallThresholdMediaQuery(list{justifyContent(flexStart)}),
  })
  let bodyContent = style(list{
    display(flexBox),
    flexDirection(column),
    smallThresholdMediaQuery(list{width(pct(100.))}),
  })
  let itemBodyTop = style(list{fontSize(px(16)), paddingRight(px(8))})
  let itemImageUnit = style(list{
    width(px(128)),
    marginRight(px(48)),
    smallThresholdMediaQuery(list{
      display(flexBox),
      marginTop(px(16)),
      marginRight(zero),
      width(auto),
      justifyContent(center),
    }),
  })
  let itemImageUnitWithRecipe = style(list{
    smallThresholdMediaQuery(list{
      display(flexBox),
      marginTop(px(16)),
      marginRight(zero),
      width(auto),
      justifyContent(spaceBetween),
    }),
  })
  let itemImage = style(list{display(block), width(px(128)), height(px(128))})
  let itemImageUnitBody = style(list{display(flexBox), flexDirection(column), alignItems(center)})
  let itemImageLabel = style(list{fontSize(px(16)), textAlign(center), marginTop(px(8))})
  let itemRecipe = style(list{
    marginTop(px(16)),
    smallThresholdMediaQuery(list{width(pct(50.)), marginTop(zero)}),
  })
  let itemRecipeTitle = style(list{marginBottom(px(2))})
  @module("./assets/recipe_icon.png")
  external recipeIcon: string = "default"
  let recipeIcon = style(list{
    display(inlineBlock),
    backgroundImage(url(recipeIcon)),
    width(px(16)),
    height(px(16)),
    backgroundSize(cover),
    marginLeft(px(4)),
    verticalAlign(#top),
  })
  let recipeRow = style(list{
    color(Colors.gray),
    display(flexBox),
    borderTop(px(1), dashed, Colors.faintGray),
    padding2(~v=px(2), ~h=zero),
  })
  let recipeRowMaterial = style(list{flexGrow(1.)})
  let recipeRowQuantity = style(list{width(px(24)), color(Colors.lightGray), textAlign(#right)})
  let itemName = style(list{
    fontSize(px(24)),
    marginBottom(px(8)),
    paddingRight(px(16)),
    smallThresholdMediaQuery(list{paddingRight(px(8))}),
  })
  let itemCategory = style(list{marginBottom(px(4))})
  let itemCategoryLink = style(list{
    color(Colors.gray),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
  })
  let itemSourceFrom = style(list{color(Colors.lightGray)})
  let itemTags = style(list{marginBottom(px(4))})
  @module("./assets/tag.png") external tagIcon: string = "default"
  let itemTagIcon = style(list{
    display(inlineBlock),
    backgroundImage(url(tagIcon)),
    width(px(16)),
    height(px(16)),
    backgroundSize(cover),
    marginRight(px(6)),
    verticalAlign(#top),
    position(relative),
    top(px(1)),
  })
  let itemTag = style(list{
    color(Colors.gray),
    display(inlineBlock),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
    marginRight(px(8)),
  })
  let itemPriceIcon = style(list{marginRight(px(6)), verticalAlign(#top), top(px(1))})
  let itemPrices = style(list{display(flexBox), marginBottom(px(4))})
  let itemPrice = style(list{marginRight(px(16)), whiteSpace(nowrap)})
  let itemPriceLabel = style(list{color(Colors.gray), marginRight(px(4))})
  let itemPriceValue = style(list{fontWeight(#num(700))})
  let itemCustomizeCost = style(list{marginBottom(px(4))})
  @module("./assets/remake.png") external remakeIcon: string = "default"
  let itemRemakeIcon = style(list{
    display(inlineBlock),
    backgroundImage(url(remakeIcon)),
    width(px(16)),
    height(px(16)),
    backgroundSize(cover),
    marginRight(px(6)),
    verticalAlign(#top),
    position(relative),
    top(px(1)),
  })
  let variantSection = style(list{
    marginTop(px(24)),
    marginLeft(px(-16)),
    marginRight(px(-16)),
    paddingLeft(px(16)),
    paddingRight(px(32)),
    overflowX(auto),
    overflowY(hidden),
    smallThresholdMediaQuery(list{marginLeft(px(-32)), marginRight(px(-32))}),
  })
  let variantTable = style(list{display(flexBox), flexWrap(wrap), maxWidth(px(288))})
  let variantRow = style(list{
    display(flexBox),
    alignItems(center),
    marginRight(px(8)),
    marginBottom(px(4)),
  })
  let variantRowLabel = style(list{alignItems(flexStart)})
  let variantLabel = style(list{width(px(86)), paddingRight(px(6)), flexShrink(0.)})
  let variantCell = style(list{fontSize(zero)})
  let variantImage = style(list{
    borderRadius(px(4)),
    border(px(1), solid, transparent),
    display(inlineBlock),
    width(px(32)),
    height(px(32)),
    flexShrink(0.),
    cursor(pointer),
  })
  let variantImageCell = style(list{
    marginRight(px(4)),
    hover(list{borderColor(Colors.veryLightGray)}),
  })
  let variantImageWrap = style(list{marginRight(px(8))})
  let variantImageSelected = style(list{important(borderColor(Colors.lightGray))})
  let singleVariantUnit = style(list{
    borderRadius(px(4)),
    border(px(1), solid, transparent),
    cursor(pointer),
    hover(list{selector("& ." ++ variantImage, list{borderColor(Colors.veryLightGray)})}),
  })
  let singleVariantUnitSelected = style(list{
    selector("& ." ++ variantImage, list{important(borderColor(Colors.lightGray))}),
  })
  let patternCell = style(list{marginRight(px(4))})
  let patternLabel = style(list{
    width(px(34)),
    transforms(list{translateX(px(22)), rotate(deg(45.))}),
    transformOrigin(zero, zero),
    whiteSpace(nowrap),
  })
  let transitionIn = style(list{
    selector("& ." ++ backdrop, list{opacity(1.)}),
    selector(
      "& ." ++ root,
      list{opacity(1.), transforms(list{scale(1., 1.), translate3d(zero, zero, zero)})},
    ),
  })
}

module VariantWithLabel = {
  @react.component
  let make = (~item: Item.t, ~variant, ~selected) =>
    <div
      onClick={_ => {
        open Webapi.Dom
        location->Location.setHash(
          "i" ++ (string_of_int(item.id) ++ (":" ++ string_of_int(variant))),
        )
      }}
      className={Cn.make(list{
        Styles.variantRow,
        Styles.singleVariantUnit,
        Cn.ifTrue(Styles.singleVariantUnitSelected, selected),
      })}>
      <img
        src={Item.getImageUrl(~item, ~variant)}
        className={Cn.make(list{Styles.variantImage, Styles.variantImageWrap})}
      />
      <div className=Styles.variantLabel>
        {switch Item.getVariantName(~item, ~variant, ()) {
        | Some(bodyName) => React.string(bodyName)
        | None => React.null
        }}
      </div>
    </div>
}

module OneDimensionVariants = {
  @react.component
  let make = (~item, ~a, ~variant) =>
    <div className={Cn.make(list{Styles.variantSection, Styles.variantTable})}>
      {Belt.Array.make(a, None)
      ->Belt.Array.mapWithIndex((i, _) =>
        <VariantWithLabel item variant=i selected={i == variant} key={string_of_int(i)} />
      )
      ->React.array}
    </div>
}

module TwoDimensionVariants = {
  @react.component
  let make = (~item, ~a, ~b, ~variant) =>
    <div className={Cn.make(list{Styles.variantSection, Cn.ifTrue(Styles.variantTable, b == 1)})}>
      {Belt.Array.make(a, None)
      ->Belt.Array.mapWithIndex((i, _) =>
        if b > 1 {
          <div className=Styles.variantRow key={string_of_int(i)}>
            {a > 1
              ? <div className=Styles.variantLabel>
                  {switch Item.getVariantName(~item, ~variant=i * b, ~hidePattern=true, ()) {
                  | Some(bodyName) => React.string(bodyName)
                  | None => React.null
                  }}
                </div>
              : React.null}
            {Belt.Array.make(b, None)
            ->Belt.Array.mapWithIndex((j, _) => {
              let v = i * b + j
              <img
                src={Item.getImageUrl(~item, ~variant=v)}
                onClick={_ => {
                  open Webapi.Dom
                  location->Location.setHash(
                    "i" ++ (string_of_int(item.id) ++ (":" ++ string_of_int(v))),
                  )
                }}
                className={Cn.make(list{
                  Styles.variantImage,
                  Styles.variantImageCell,
                  Cn.ifTrue(Styles.variantImageSelected, variant == v),
                })}
                key={string_of_int(j)}
              />
            })
            ->React.array}
          </div>
        } else {
          <VariantWithLabel item variant=i selected={i == variant} key={string_of_int(i)} />
        }
      )
      ->React.array}
      {b > 1
        ? <div className={Cn.make(list{Styles.variantRow, Styles.variantRowLabel})}>
            {a > 1 ? <div className=Styles.variantLabel /> : React.null}
            {Belt.Array.make(b, None)
            ->Belt.Array.mapWithIndex((j, _) =>
              <div className=Styles.patternCell key={string_of_int(j)}>
                {switch Item.getVariantName(~item, ~variant=j, ~hideBody=true, ()) {
                | Some(patternName) =>
                  <div
                    className=Styles.patternLabel
                    style={ReactDOMRe.Style.make(
                      ~height=string_of_int(Js.String.length(patternName) * 6) ++ "px",
                      (),
                    )}>
                    {React.string(patternName)}
                  </div>
                | None => React.null
                }}
              </div>
            )
            ->React.array}
          </div>
        : React.null}
    </div>
}

module ItemRecipe = {
  @react.component
  let make = (~recipe) =>
    <div className=Styles.itemRecipe>
      <div className=Styles.itemRecipeTitle>
        {React.string("DIY")} <span className=Styles.recipeIcon />
      </div>
      {recipe
      ->Belt.Array.map(((itemId, quantity)) =>
        <div className=Styles.recipeRow key=itemId>
          <div className=Styles.recipeRowMaterial>
            {React.string(Item.getMaterialName(itemId))}
          </div>
          <div className=Styles.recipeRowQuantity>
            {React.string(j`×` ++ string_of_int(quantity))}
          </div>
        </div>
      )
      ->React.array}
    </div>
}

module MyStatusSection = {
  module Styles = {
    open Css
    let root = style(list{
      marginTop(px(16)),
      borderTop(px(1), dashed, Colors.veryLightGray),
      paddingTop(px(8)),
      position(relative),
      selector(
        "& ." ++ ItemCard.Styles.statusButton,
        list{
          fontSize(px(14)),
          backgroundColor(Colors.faintGreen),
          paddingLeft(px(6)),
          paddingRight(px(6)),
        },
      ),
      selector(
        "& ." ++ ItemCard.Styles.catalogCheckbox,
        list{borderColor(Colors.lightGray), opacity(1.)},
      ),
      smallThresholdMediaQuery(list{marginLeft(px(-16)), marginRight(px(-16))}),
    })
    let ellipsisButton = style(list{})
    let currentStatusRow = style(list{
      display(flexBox),
      alignItems(center),
      justifyContent(spaceBetween),
    })
    let currentStatus = style(list{})
    let currentStatusEmoji = style(list{display(inlineBlock), marginRight(px(8))})
    let itemNote = style(list{marginTop(px(8))})
    let catalogCheckbox = style(list{marginLeft(px(12))})
  }

  @react.component
  let make = (~item: Item.t, ~variant) => {
    let userItem = UserStore.useItem(~itemId=item.id, ~variation=variant)
    <div className=Styles.root>
      {switch userItem {
      | Some(userItem) => <>
          {switch userItem.status {
          | Wishlist
          | ForTrade
          | CanCraft => <>
              <div className=Styles.currentStatusRow>
                <div className=Styles.currentStatus>
                  <span className=Styles.currentStatusEmoji>
                    {React.string(User.itemStatusToEmoji(userItem.status))}
                  </span>
                  {React.string(
                    switch userItem.status {
                    | Wishlist => `In your Wishlist`
                    | ForTrade => `In your For Trade list`
                    | CanCraft => `In your Can Craft list`
                    | _ => raise(Constants.Uhoh)
                    },
                  )}
                </div>
                <UserItemEllipsisButton
                  item userItem variation=variant className=Styles.ellipsisButton
                />
              </div>
            </>
          | CatalogOnly =>
            <div className=Styles.currentStatusRow>
              <ItemCard.StatusButtons item variant />
              <ItemCard.CatalogCheckbox
                item variant userItem=Some(userItem) className=Styles.catalogCheckbox
              />
            </div>
          }}
          <UserItemNote itemId=item.id variation=variant userItem className=Styles.itemNote />
        </>
      | None =>
        <div className=Styles.currentStatusRow>
          <ItemCard.StatusButtons item variant />
          <ItemCard.CatalogCheckbox item variant userItem=None className=Styles.catalogCheckbox />
        </div>
      }}
    </div>
  }
}

module FriendsSection = {
  module Styles = {
    open Css
    let friendSection = style(list{
      borderTop(px(1), dashed, Colors.lightGreen),
      backgroundColor(Colors.faintGreen),
    })
    let friendItemList = style(list{
      display(flexBox),
      flexWrap(wrap),
      padding(px(16)),
      maxWidth(px(512)),
    })
    let friendItem = style(list{
      display(flexBox),
      alignItems(center),
      boxSizing(borderBox),
      paddingRight(px(8)),
      marginBottom(px(2)),
      width(pct(50.)),
      whiteSpace(nowrap),
      smallThresholdMediaQuery(list{width(pct(100.))}),
    })
    let image = style(list{display(block), width(px(32)), height(px(32)), marginRight(px(8))})
    let hasIn = style(list{color(Colors.gray)})
    let listLink = style(list{
      color(Colors.charcoal),
      textDecoration(none),
      hover(list{textDecoration(underline)}),
    })
    let showAllRow = style(list{width(pct(100.)), paddingTop(px(8))})
    let showAllButton = style(list{
      backgroundColor(transparent),
      border(px(1), solid, Colors.lightGreen),
      borderRadius(px(4)),
      color(Colors.green),
      padding2(~v=px(10), ~h=zero),
      width(pct(100.)),
      transition(~duration=200, "all"),
      hover(list{borderColor(Colors.green)}),
    })
  }

  type friendItem = {
    userId: string,
    username: string,
    variant: int,
    status: User.itemStatus,
  }

  @react.component
  let make = (~item: Item.t) => {
    let (friendItems, setFriendItems) = React.useState(() => None)
    let (showLimit, setShowLimit) = React.useState(() => 12)
    React.useEffect0(() => {
      %Repromise({
        let response = API.getFolloweesItem(
          ~sessionId=Belt.Option.getExn(UserStore.sessionId.contents),
          ~itemId=item.id,
        )
        %Repromise.JsExn({
          let json = Fetch.Response.json(response)
          open Json.Decode
          let friendItems = json->array(json => {
            userId: json->field("userId", string),
            username: json->field("username", string),
            variant: json->field("variant", int),
            status: json->field("status", int)->User.itemStatusFromJs->Belt.Option.getExn,
          })
          setFriendItems(_ => Some(friendItems))
          Promise.resolved()
        })
      })->ignore
      None
    })

    switch friendItems {
    | Some(friendItems) =>
      if Js.Array.length(friendItems) > 0 {
        <div className=Styles.friendSection>
          <div className=Styles.friendItemList>
            {friendItems
            ->Js.Array.slice(~start=0, ~end_=showLimit)
            ->Js.Array.map(friendItem =>
              <div
                className=Styles.friendItem
                key={friendItem.userId ++ string_of_int(friendItem.variant)}>
                <img
                  src={Item.getImageUrl(~item, ~variant=friendItem.variant)} className=Styles.image
                />
                <span>
                  <Link path={"/u/" ++ friendItem.username}>
                    {React.string(friendItem.username)}
                  </Link>
                  <span className=Styles.hasIn> {React.string(" has in ")} </span>
                  <Link
                    path={"/u/" ++
                    (friendItem.username ++
                    ("/" ++
                    ViewingList.viewingListToUrl(
                      switch friendItem.status {
                      | ForTrade => ForTrade
                      | CanCraft => CanCraft
                      | Wishlist => Wishlist
                      | CatalogOnly => Catalog
                      },
                    )))}
                    className=Styles.listLink>
                    {React.string(User.itemStatusToString(friendItem.status))}
                  </Link>
                </span>
              </div>
            )
            ->React.array}
            {Js.Array.length(friendItems) > showLimit
              ? <div className=Styles.showAllRow>
                  <button
                    onClick={_ => setShowLimit(showLimit => showLimit + 12)}
                    className=Styles.showAllButton>
                    {React.string("Show more")}
                  </button>
                </div>
              : React.null}
          </div>
        </div>
      } else {
        React.null
      }
    | None => React.null
    }
  }
}

let hasLoggedDetailOverlay = ref(false)

@react.component
let make = (~item: Item.t, ~variant, ~isInitialLoad) => {
  let me = UserStore.useMe()
  let onClose = () => {
    let url = ReasonReactRouter.dangerouslyGetInitialUrl()
    ReasonReactRouter.push(
      "/" ++
      (Js.Array.joinWith("/", Belt.List.toArray(url.path)) ++
      switch url.search {
      | "" => ""
      | search => "?" ++ search
      }),
    )
  }
  let variant = variant->Belt.Option.getWithDefault(0)
  let (transitionIn, setTransitionIn) = React.useState(() => isInitialLoad)
  React.useEffect0(() => {
    Js.Global.setTimeout(() => setTransitionIn(_ => true), 20)->ignore
    if !hasLoggedDetailOverlay.contents {
      Analytics.Amplitude.logEventWithProperties(
        ~eventName="Item Detail Overlay Shown",
        ~eventProperties={
          "isInitialLoad": isInitialLoad,
          "itemId": item.id,
          "variant": variant,
          "pathname": {
            open Webapi.Dom
            location->Location.pathname
          },
        },
      )
      hasLoggedDetailOverlay := true
    }
    None
  })

  let viewportWidth = Utils.useViewportWidth()
  let itemImage =
    <div
      className={Cn.make(list{
        Styles.itemImageUnit,
        Cn.ifTrue(Styles.itemImageUnitWithRecipe, item.recipe != None),
      })}>
      <div className=Styles.itemImageUnitBody>
        <img src={Item.getImageUrl(~item, ~variant)} className=Styles.itemImage />
        {if Item.getNumVariations(~item) > 1 {
          switch Item.getVariantName(~item, ~variant, ()) {
          | Some(variantName) =>
            <div className=Styles.itemImageLabel> {React.string(variantName)} </div>
          | None => React.null
          }
        } else {
          React.null
        }}
      </div>
      {switch item.recipe {
      | Some(recipe) => <ItemRecipe recipe />
      | None => React.null
      }}
    </div>

  <div
    className={Cn.make(list{Modal.Styles.overlay, Cn.ifTrue(Styles.transitionIn, transitionIn)})}>
    <div className=Styles.backdrop onClick={_ => onClose()} />
    <Modal.Div100VH className=Styles.root100VH>
      <div className=Styles.root>
        <div className=Styles.body>
          {viewportWidth > 500 ? itemImage : React.null}
          <div className=Styles.bodyContent>
            <div className=Styles.itemBodyTop>
              <div className=Styles.itemName> {React.string(Item.getName(item))} </div>
              <div className=Styles.itemCategory>
                <Link path={"/?c=" ++ item.category} className=Styles.itemCategoryLink>
                  {React.string(Utils.capitalizeFirstLetter(item.category))}
                </Link>
                {switch item.source {
                | Some(itemSource) => <>
                    <span className=Styles.itemSourceFrom> {React.string(" from ")} </span>
                    {React.string(itemSource)}
                  </>
                | None => React.null
                }}
              </div>
              {if item.tags->Js.Array.length > 0 {
                <div className=Styles.itemTags>
                  <span className=Styles.itemTagIcon />
                  {item.tags
                  ->Belt.Array.map(tag =>
                    <Link path={"/?q=" ++ tag} className=Styles.itemTag key=tag>
                      {React.string(Utils.capitalizeFirstLetter(tag))}
                    </Link>
                  )
                  ->React.array}
                </div>
              } else {
                React.null
              }}
              {if item.buyPrice != None || item.sellPrice != None {
                <div className=Styles.itemPrices>
                  {switch item.buyPrice {
                  | Some(buyPrice) =>
                    <div className=Styles.itemPrice>
                      <span className={Cn.make(list{Emoji.Styles.bell, Styles.itemPriceIcon})} />
                      <label className=Styles.itemPriceLabel> {React.string("Buy")} </label>
                      <span className=Styles.itemPriceValue>
                        {React.string(string_of_int(buyPrice))}
                      </span>
                    </div>
                  | None => React.null
                  }}
                  {switch item.sellPrice {
                  | Some(sellPrice) =>
                    <div className=Styles.itemPrice>
                      <span className={Cn.make(list{Emoji.Styles.bell, Styles.itemPriceIcon})} />
                      <label className=Styles.itemPriceLabel> {React.string("Sell")} </label>
                      <span className=Styles.itemPriceValue>
                        {React.string(string_of_int(sellPrice))}
                      </span>
                    </div>
                  | None => React.null
                  }}
                </div>
              } else {
                React.null
              }}
              {switch item.customizeCost {
              | Some(customizeCost) =>
                <div className=Styles.itemCustomizeCost>
                  <span className=Styles.itemRemakeIcon />
                  {React.string(
                    switch (item.bodyCustomizable, item.patternCustomizable) {
                    | (true, true) => "Body and Pattern Customizable "
                    | (false, true) => "Pattern Customizable "
                    | (true, false) => "Body Customizable "
                    | _ => ""
                    },
                  )}
                  {React.string(j`(×` ++ (string_of_int(customizeCost) ++ ")"))}
                </div>
              | None => React.null
              }}
            </div>
            {viewportWidth <= 500 ? itemImage : React.null}
            {switch (item.type_, item.variations) {
            | (Recipe(_), _) => React.null
            | (_, Single) => React.null
            | (_, OneDimension(a)) => <OneDimensionVariants item a variant />
            | (_, TwoDimensions(a, b)) => <TwoDimensionVariants item a b variant />
            }}
            {switch me {
            | Some(me) =>
              let canonicalVariant = Item.getCanonicalVariant(~item, ~variant)
              <MyStatusSection
                item variant=canonicalVariant key={string_of_int(canonicalVariant)}
              />
            | None => React.null
            }}
          </div>
        </div>
        {switch me->Belt.Option.flatMap(me => me.followeeIds) {
        | Some(followeeIds) =>
          if Js.Array.length(followeeIds) > 0 {
            <FriendsSection item />
          } else {
            React.null
          }
        | None => React.null
        }}
        <Modal.CloseButton onClose />
      </div>
    </Modal.Div100VH>
  </div>
}
