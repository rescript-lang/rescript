open Belt

module Styles = {
  open Css
  let root = style(list{margin3(~top=zero, ~bottom=px(32), ~h=auto)})
  let body = style(list{maxWidth(px(512)), margin2(~v=zero, ~h=auto), padding2(~v=zero, ~h=px(16))})
  let fetchingCatalogScanner = style(list{marginBottom(px(16))})
  let textarea = style(list{
    borderRadius(px(4)),
    borderColor(Colors.lightGray),
    width(pct(100.)),
    minHeight(px(256)),
    padding(px(8)),
    boxSizing(borderBox),
    marginBottom(px(8)),
  })
  let searchButtonRow = style(list{display(flexBox), justifyContent(flexEnd)})

  let sectionTitle = style(list{fontSize(px(24)), marginBottom(px(8))})
  let results = style(list{maxWidth(px(400)), margin2(~v=zero, ~h=auto)})
  let missingRows = style(list{marginBottom(px(64))})
  let missingRow = style(list{color(Colors.red)})
  let matchRows = style(list{marginBottom(px(64))})
  let itemRowName = style(list{
    fontSize(px(16)),
    marginBottom(px(4)),
    display(flexBox),
    justifyContent(spaceBetween),
    alignItems(center),
  })
  let itemRowNameLink = style(list{
    color(Colors.charcoal),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
  })
  let itemRowVariants = style(list{width(pct(100.))})

  let variantRowImageLink = style(list{marginRight(px(8))})
  let variantRowImage = style(list{display(block), width(px(32)), height(px(32))})
  let variantRowName = style(list{flexGrow(1.)})
  let variantRow = style(list{
    display(flexBox),
    alignItems(center),
    media(
      "(hover: hover)",
      list{hover(list{selector("& ." ++ variantRowName, list{fontWeight(#num(800))})})},
    ),
  })
  let variantRowUserStatus = style(list{width(px(32))})
  let radioButtons = style(list{display(flexBox)})
  let radioButtonsBatchLabel = style(list{marginRight(px(8)), fontSize(px(12)), color(Colors.gray)})
  let radioButtonsBatch = style(list{
    display(flexBox),
    alignItems(center),
    opacity(0.1),
    transition(~duration=200, "all"),
    media("(hover: none)", list{opacity(0.5)}),
  })
  let itemRow = style(list{
    marginBottom(px(16)),
    media(
      "(hover: hover)",
      list{hover(list{selector("& ." ++ radioButtonsBatch, list{opacity(1.)})})},
    ),
  })
  let radioButton = style(list{
    backgroundColor(Colors.white),
    borderStyle(solid),
    borderColor(Colors.lightGray),
    borderTopWidth(px(1)),
    borderBottomWidth(px(1)),
    borderLeftWidth(px(1)),
    borderRightWidth(zero),
    height(px(27)),
    boxSizing(borderBox),
    margin(zero),
    padding2(~v=px(1), ~h=px(6)),
    unsafe("touchAction", "manipulation"),
    firstChild(list{borderTopLeftRadius(px(4)), borderBottomLeftRadius(px(4))}),
    lastChild(list{
      borderRightWidth(px(1)),
      borderTopRightRadius(px(4)),
      borderBottomRightRadius(px(4)),
    }),
  })
  let radioButtonDisabled = style(list{opacity(0.1)})
  let radioButtonSelected = style(list{
    backgroundColor(hex("3aa56380")),
    borderColor(Colors.green),
    selector("& + ." ++ radioButton, list{borderLeftColor(Colors.green)}),
  })

  let resultsOverlay = style(list{
    position(fixed),
    bottom(zero),
    backgroundColor(Colors.white),
    padding(px(16)),
    borderTopLeftRadius(px(8)),
    borderTopRightRadius(px(8)),
    left(pct(50.)),
    width(px(512)),
    boxSizing(borderBox),
    marginLeft(px(-256)),
    display(flexBox),
    alignItems(center),
    justifyContent(spaceBetween),
    Colors.darkLayerShadow,
    selector("& a", list{textDecoration(none), hover(list{textDecoration(underline)})}),
    media("(max-width: 550px)", list{width(auto), left(zero), right(zero), marginLeft(zero)}),
  })
  let bulkActionsLink = style(list{})
  let bulkActionsButtons = style(list{
    display(flexBox),
    flexDirection(column),
    padding(px(16)),
    backgroundColor(Colors.white),
    borderRadius(px(8)),
    Colors.darkLayerShadow,
    selector(
      "& > a",
      list{
        display(inlineBlock),
        marginBottom(px(4)),
        textDecoration(none),
        hover(list{textDecoration(underline)}),
      },
    ),
  })
  let successBlock = style(list{
    backgroundColor(Colors.white),
    padding2(~v=px(16), ~h=px(16)),
    borderRadius(px(8)),
    marginBottom(px(32)),
  })
  let successMessage = style(list{flexGrow(1.), textAlign(#right), color(Colors.green)})
  let errorMessage = style(list{flexGrow(1.), textAlign(#right), color(Colors.red)})
  let saveButton = style(list{marginLeft(px(16))})
}

@deriving(jsConverter)
type itemDestination = [
  | @as("For Trade") #ForTrade
  | @as("Can Craft") #CanCraft
  | @as("Catalog Only") #CatalogOnly
  | @as("Wishlist") #Wishlist
  | @as("Ignore") #Ignore
]

let itemDestinationToEmoji = destination =>
  switch destination {
  | #ForTrade => `🤝`
  | #CanCraft => `🔨`
  | #CatalogOnly => `📖`
  | #Wishlist => `🙏`
  | #Ignore => `🤝`
  }

module VariantRow = {
  @react.component
  let make = (~item: Item.t, ~variant, ~itemsState, ~onChange) => {
    let renderDestinationOption = destination => {
      let disabled = destination == #CanCraft && item.recipe == None
      <button
        onClick={_ => onChange(item.id, variant, destination)}
        disabled
        title={switch destination {
        | #CanCraft => "Can Craft"
        | #ForTrade => "For Trade"
        | #Wishlist => "Wishlist"
        | #CatalogOnly => "Catalog"
        | #Ignore => "Skip"
        }}
        className={Cn.make(list{
          Styles.radioButton,
          Cn.ifTrue(
            Styles.radioButtonSelected,
            Js.Dict.get(itemsState, User.getItemKey(~itemId=item.id, ~variation=variant)) ==
              Some(destination),
          ),
        })}>
        <span className={Cn.ifTrue(Styles.radioButtonDisabled, disabled)}>
          {React.string(destination == #Ignore ? "Skip" : itemDestinationToEmoji(destination))}
        </span>
      </button>
    }

    <div className=Styles.variantRow>
      <Link
        path={Utils.getItemDetailUrl(~itemId=item.id, ~variant=Some(variant))}
        className=Styles.variantRowImageLink>
        <img src={Item.getImageUrl(~item, ~variant)} className=Styles.variantRowImage />
      </Link>
      <div className=Styles.variantRowName>
        {switch Item.getVariantName(~item, ~variant, ~hidePattern=true, ()) {
        | Some(variantName) => React.string(variantName)
        | None => React.null
        }}
      </div>
      <div className=Styles.variantRowUserStatus>
        {switch UserStore.getItem(~itemId=item.id, ~variation=variant) {
        | Some(userItem) => <div> {React.string(User.itemStatusToEmoji(userItem.status))} </div>
        | None => React.null
        }}
      </div>
      <div className=Styles.radioButtons>
        {renderDestinationOption(#ForTrade)}
        {renderDestinationOption(#CanCraft)}
        {renderDestinationOption(#CatalogOnly)}
        {renderDestinationOption(#Wishlist)}
        {renderDestinationOption(#Ignore)}
      </div>
    </div>
  }
}

module ResultRowWithItem = {
  @react.component
  let make = (
    ~item: Item.t,
    ~variants,
    ~itemsState,
    ~onChange: (int, int, itemDestination) => unit,
  ) => {
    let renderDestinationOption = destination => {
      let disabled = destination == #CanCraft && item.recipe == None
      <button
        onClick={_ =>
          variants->Belt.Array.forEach(variant => onChange(item.id, variant, destination))}
        disabled
        className=Styles.radioButton>
        <span className={Cn.ifTrue(Styles.radioButtonDisabled, disabled)}>
          {React.string(destination == #Ignore ? "Skip" : itemDestinationToEmoji(destination))}
        </span>
      </button>
    }

    <div className=Styles.itemRow>
      <div className=Styles.itemRowName>
        <Link
          path={Utils.getItemDetailUrl(~itemId=item.id, ~variant=None)}
          className=Styles.itemRowNameLink>
          {React.string(Item.getName(item))}
        </Link>
        {Js.Array.length(variants) > 1
          ? <div className=Styles.radioButtonsBatch>
              <span className=Styles.radioButtonsBatchLabel> {React.string("Quick")} </span>
              <div className=Styles.radioButtons>
                {renderDestinationOption(#ForTrade)}
                {renderDestinationOption(#CanCraft)}
                {renderDestinationOption(#CatalogOnly)}
                {renderDestinationOption(#Wishlist)}
                {renderDestinationOption(#Ignore)}
              </div>
            </div>
          : React.null}
      </div>
      <div className=Styles.itemRowVariants>
        {variants
        ->Js.Array.map(variant =>
          <VariantRow item variant itemsState onChange key={string_of_int(variant)} />
        )
        ->React.array}
      </div>
    </div>
  }
}

module BulkActions = {
  @react.component
  let make = (~setItemStates) => {
    let (showPopup, setShowPopup) = React.useState(() => false)
    let reference = React.useRef(Js.Nullable.null)

    <>
      <a
        href="#"
        onClick={e => {
          ReactEvent.Mouse.preventDefault(e)
          setShowPopup(show => !show)
        }}
        className=Styles.bulkActionsLink
        ref={ReactDOMRe.Ref.domRef(reference)}>
        {React.string("Bulk actions")}
      </a>
      {showPopup
        ? <ReactAtmosphere.PopperLayer
            reference
            onOutsideClick={() => setShowPopup(_ => false)}
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
            render={_ =>
              <div className=Styles.bulkActionsButtons>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    setItemStates(itemStates =>
                      itemStates
                      ->Js.Dict.entries
                      ->Js.Array.map(((key, _value)) => (key, #Ignore))
                      ->Js.Dict.fromArray
                    )
                    setShowPopup(_ => false)
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Item Import Bulk Set",
                      ~eventProperties={"destination": "skip"},
                    )
                  }}>
                  {React.string("Set every item to Skip")}
                </a>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    setItemStates(itemStates =>
                      itemStates
                      ->Js.Dict.entries
                      ->Js.Array.map(((key, _value)) => (key, #ForTrade))
                      ->Js.Dict.fromArray
                    )
                    setShowPopup(_ => false)
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Item Import Bulk Set",
                      ~eventProperties={"destination": "for-trade"},
                    )
                  }}>
                  {React.string("Set every item to For Trade")}
                </a>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    setItemStates(itemStates =>
                      itemStates
                      ->Js.Dict.entries
                      ->Js.Array.map(((key, _value)) => (key, #CatalogOnly))
                      ->Js.Dict.fromArray
                    )
                    setShowPopup(_ => false)
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Item Import Bulk Set",
                      ~eventProperties={"destination": "catalog"},
                    )
                  }}>
                  {React.string("Set every item to Catalog")}
                </a>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    setItemStates(itemStates =>
                      itemStates
                      ->Js.Dict.entries
                      ->Js.Array.map(((key, value)) => {
                        let (itemId, _variant) = User.fromItemKey(~key)->Option.getExn
                        let item = Item.getItem(~itemId)
                        (
                          key,
                          if item.recipe != None {
                            #CanCraft
                          } else {
                            value
                          },
                        )
                      })
                      ->Js.Dict.fromArray
                    )
                    setShowPopup(_ => false)
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Item Import Bulk Set",
                      ~eventProperties={"destination": "can-craft"},
                    )
                  }}>
                  {React.string("Set every craftable item to Can Craft")}
                </a>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    setItemStates(itemStates =>
                      itemStates
                      ->Js.Dict.entries
                      ->Js.Array.map(((key, _value)) => (key, #Wishlist))
                      ->Js.Dict.fromArray
                    )
                    setShowPopup(_ => false)
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Item Import Bulk Set",
                      ~eventProperties={"destination": "wishlist"},
                    )
                  }}>
                  {React.string("Set every item to Wishlist")}
                </a>
              </div>}
          />
        : React.null}
    </>
  }
}

module Results = {
  type submitState =
    | Submitting
    | Success
    | Error(string)

  @react.component
  let make = (
    ~me: User.t,
    ~matches: array<(Item.t, array<int>)>,
    ~misses: array<string>,
    ~onReset,
  ) => {
    let (itemsState, setItemStates) = React.useState(() =>
      Js.Dict.fromArray(
        Array.concatMany(
          matches->Array.map(((item, variants)) =>
            variants->Belt.Array.map(variant => (
              User.getItemKey(~itemId=item.id, ~variation=variant),
              switch UserStore.getItem(~itemId=item.id, ~variation=variant) {
              | Some(userItem) =>
                switch userItem.status {
                | CanCraft => #CanCraft
                | ForTrade => #ForTrade
                | CatalogOnly => #CatalogOnly
                | Wishlist => #Ignore
                }
              | None => #Ignore
              },
            ))
          ),
        ),
      )
    )
    let (submitStatus, setSubmitState) = React.useState(() => None)

    let numMissingRows = Array.length(misses)
    let numMatchingRows = Array.length(matches)
    React.useEffect0(() => {
      Analytics.Amplitude.logEventWithProperties(
        ~eventName="Import Page List Processed",
        ~eventProperties={
          "numMismatch": numMissingRows,
          "numMatch": numMatchingRows,
        },
      )
      None
    })

    <div className=Styles.results>
      {submitStatus == Some(Success)
        ? <div className=Styles.successBlock>
            {React.string("Your import was successful! Go to ")}
            <Link path={"/u/" ++ me.username}> {React.string("your profile")} </Link>
            {React.string("!")}
          </div>
        : React.null}
      {Js.Array.length(misses) > 0
        ? <div className=Styles.missingRows>
            <div className=Styles.sectionTitle> {React.string("Items without matches")} </div>
            {misses
            ->Array.mapWithIndex((i, query) =>
              <div className=Styles.missingRow key={string_of_int(i)}> {React.string(query)} </div>
            )
            ->React.array}
          </div>
        : React.null}
      {Js.Array.length(matches) > 0
        ? <div className=Styles.matchRows>
            {matches
            ->Belt.Array.map(((item, variants)) =>
              <ResultRowWithItem
                item
                variants
                itemsState
                onChange={(itemId, variant, destination) =>
                  setItemStates(itemsState => {
                    let clone = Utils.cloneJsDict(itemsState)
                    clone->Js.Dict.set(User.getItemKey(~itemId, ~variation=variant), destination)
                    clone
                  })}
                key={string_of_int(item.id)}
              />
            )
            ->React.array}
          </div>
        : <div>
            {React.string("No items were matched. ")}
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                onReset()
              }}>
              {React.string("Start over")}
            </a>
          </div>}
      <div className=Styles.resultsOverlay>
        <BulkActions setItemStates />
        {switch submitStatus {
        | Some(Success) => <div className=Styles.successMessage> {React.string("Success!")} </div>
        | Some(Error(error)) => <div className=Styles.errorMessage> {React.string(error)} </div>
        | _ =>
          <div className=Styles.successMessage>
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                onReset()
              }}>
              {React.string("Start over")}
            </a>
          </div>
        }}
        <Button
          onClick={_ => {
            let numForTrade = ref(0)
            let numCanCraft = ref(0)
            let numCatalog = ref(0)
            let numWishlist = ref(0)
            itemsState
            ->Js.Dict.entries
            ->Array.forEach(((_itemKey, destination)) =>
              switch destination {
              | #ForTrade => numForTrade := numForTrade.contents + 1
              | #CanCraft => numCanCraft := numCanCraft.contents + 1
              | #CatalogOnly => numCatalog := numCatalog.contents + 1
              | #Wishlist => numWishlist := numWishlist.contents + 1
              | #Ignore => ()
              }
            )
            if (
              numForTrade.contents +
              numCanCraft.contents +
              numCatalog.contents +
              numWishlist.contents != 0
            ) {
              ConfirmDialog.confirm(
                ~bodyText="This will add " ++
                (Js.Array.joinWith(
                  ", ",
                  Array.keepMap(
                    [
                      switch numForTrade.contents {
                      | 0 => None
                      | numForTrade => Some(string_of_int(numForTrade) ++ " For Trade")
                      },
                      switch numCanCraft.contents {
                      | 0 => None
                      | numCanCraft => Some(string_of_int(numCanCraft) ++ " Can Craft")
                      },
                      switch numWishlist.contents {
                      | 0 => None
                      | numWishlist => Some(string_of_int(numWishlist) ++ " Wishlist")
                      },
                      switch numCatalog.contents {
                      | 0 => None
                      | numCatalog => Some(string_of_int(numCatalog) ++ " Catalog")
                      },
                    ],
                    x => x,
                  ),
                ) ++
                " items. Are you sure you want to continue?"),
                ~confirmLabel="Do it!",
                ~cancelLabel="Never mind",
                ~onConfirm=() => {
                  setSubmitState(_ => Some(Submitting))
                  let updates =
                    itemsState
                    ->Js.Dict.entries
                    ->Array.keepMap(((itemKey, value)) =>
                      switch value {
                      | #Ignore => None
                      | #CanCraft =>
                        Some((Option.getExn(User.fromItemKey(~key=itemKey)), User.CanCraft))
                      | #ForTrade =>
                        Some((Option.getExn(User.fromItemKey(~key=itemKey)), User.ForTrade))
                      | #Wishlist =>
                        Some((Option.getExn(User.fromItemKey(~key=itemKey)), User.Wishlist))
                      | #CatalogOnly =>
                        Some((Option.getExn(User.fromItemKey(~key=itemKey)), User.CatalogOnly))
                      }
                    )
                  %Repromise({
                    let responseResult = API.importItems(
                      ~sessionId=UserStore.sessionId.contents,
                      ~updates,
                    )
                    switch responseResult {
                    | Ok(response) =>
                      if Fetch.Response.status(response) < 400 {
                        UserStore.init()
                        setSubmitState(_ => Some(Success))
                        Analytics.Amplitude.logEventWithProperties(
                          ~eventName="Item Import Success",
                          ~eventProperties={
                            "numMismatch": numMissingRows,
                            "numMatch": numMatchingRows,
                            "numUpdates": Js.Array.length(updates),
                          },
                        )
                        Promise.resolved()
                      } else {
                        %Repromise.JsExn({
                          let text = Fetch.Response.text(response)
                          Analytics.Amplitude.logEventWithProperties(
                            ~eventName="Item Import Error",
                            ~eventProperties={
                              "error": text,
                              "numMismatch": numMissingRows,
                              "numMatch": numMatchingRows,
                              "numUpdates": Js.Array.length(updates),
                            },
                          )
                          setSubmitState(_ => Some(Error(text)))
                          Promise.resolved()
                        })
                      }
                    | Error(_error) =>
                      setSubmitState(_ => Some(Error("Something went wrong. Sorry!")))
                      Promise.resolved()
                    }
                  })->ignore
                },
                (),
              )
            }
          }}
          disabled={submitStatus == Some(Submitting)}
          className=Styles.saveButton>
          {React.string("Save changes")}
        </Button>
      </div>
    </div>
  }
}

let process = value => {
  let rows =
    value
    ->Js.String.split("\n")
    ->Js.Array.map(str => str->Js.String.trim)
    ->Js.Array.filter(x => x != "")
  let resultMap = Js.Dict.empty()
  let missingQueries = []
  rows->Array.forEach(row => {
    let result = row->Js.Re.exec_(/(.*?) \[(.*?)\]$/g)
    let itemWithVariant = switch result {
    | Some(match_) =>
      let captures = Js.Re.captures(match_)
      let itemName = Array.getUnsafe(captures, 1)->Js.Nullable.toOption
      let variantName = Array.getUnsafe(captures, 2)->Js.Nullable.toOption
      switch (itemName, variantName) {
      | (Some(itemName), Some(variantName)) =>
        Item.getByName(~name=itemName)->Option.flatMap(item =>
          Item.getVariantByName(~item, ~variantName)->Option.map(variant => (item, variant))
        )
      | _ => None
      }
    | None => None
    }
    let itemWithVariants = switch itemWithVariant {
    | Some((item, variant)) => Some((item, [variant]))
    | None =>
      Item.getByName(~name=row)->Option.map(item => (item, Item.getCollapsedVariants(~item)))
    }
    switch itemWithVariants {
    | Some((item, variants)) =>
      let resultMapVariants = switch resultMap->Js.Dict.get(string_of_int(item.id)) {
      | Some(arr) => arr
      | None =>
        let arr = []
        resultMap->Js.Dict.set(string_of_int(item.id), arr)
        arr
      }
      variants->Js.Array.forEach(variant =>
        if !(resultMapVariants->Js.Array.includes(variant)) {
          resultMapVariants->Js.Array.push(variant)->ignore
        }
      )
    | None => missingQueries->Js.Array.push(row)->ignore
    }
  })
  (
    resultMap
    ->Js.Dict.entries
    ->Array.map(((itemId, variants)) => (Item.getItem(~itemId=int_of_string(itemId)), variants))
      ->Js.Array.sortInPlaceWith(((aItem, _), (bItem, _)) =>
        ItemFilters.compareItemsABC(aItem, bItem)
      ),
    missingQueries,
  )
}

@react.component
let make = (~showLogin, ~url: ReasonReactRouter.url) => {
  let userState = UserStore.useStore()
  let (value, setValue) = React.useState(() => "")
  let (results, setResults) = React.useState(() => None)
  let (isFetchingFromCatalogScanner, setIsFetchingFromCatalogScanner) = React.useState(() => false)
  let onSubmit = e => {
    ReactEvent.Form.preventDefault(e)
    setResults(_ => Some(process(value)))
  }

  let needsLogin = userState == NotLoggedIn
  React.useEffect1(() => {
    if needsLogin {
      showLogin()
    }
    None
  }, [needsLogin])
  React.useEffect0(() => {
    open Webapi.Url.URLSearchParams
    let searchParams = make(url.search)
    let catalogScannerId = searchParams->get("cs")
    switch catalogScannerId {
    | Some(catalogScannerId) =>
      setIsFetchingFromCatalogScanner(_ => true)
      %Repromise.Js({
        let response = Fetch.fetchWithInit(
          `https://ehsan.lol/$catalogScannerId/raw`,
          Fetch.RequestInit.make(~method_=Get, ~mode=CORS, ()),
        )
        setIsFetchingFromCatalogScanner(_ => false)
        switch response {
        | Ok(response) =>
          if Fetch.Response.status(response) < 400 {
            %Repromise.JsExn({
              let text = Fetch.Response.text(response)
              setResults(_ => Some(process(text)))
              Promise.resolved()
            })
          } else {
            Promise.resolved()
          }
        | Error(_) => Promise.resolved()
        }
      })
    | None => Promise.resolved()
    }->ignore
    Analytics.Amplitude.logEvent(~eventName="Import Page Viewed")
    None
  })

  <div className=Styles.root>
    <PageTitle title="Import items" />
    {switch userState {
    | LoggedIn(me) =>
      switch results {
      | Some((matches, misses)) =>
        <div>
          <BodyCard>
            <p>
              {React.string(j`For each item, choose For Trade 🤝, Can Craft 🔨, Catalog 📖 or skip. If the item is already in your profile, you will see the status next to the selector.`)}
            </p>
            <p>
              {React.string(j`On Nook Exchange, customizable variations are hidden. That is why you do not see all colors and patterns here.`)}
            </p>
            <p>
              {React.string(
                "When you are done, press the Save button in the bottom bar. There are bulk actions to help as well!",
              )}
            </p>
          </BodyCard>
          <div className=Styles.body>
            <Results
              me
              matches
              misses
              onReset={() => {
                setResults(_ => None)
                setValue(_ => "")
              }}
            />
          </div>
        </div>
      | None =>
        <div>
          <BodyCard>
            <p>
              {React.string(
                "Have a big collection? This tool can help you add many items at once. Start by pasting a list of item names in the textbox. ",
              )}
              <a href="https://twitter.com/nookexchange" target="_blank">
                {React.string("Let us know")}
              </a>
              {React.string(" if you have issues.")}
            </p>
            <p>
              {React.string("If you don't want to type each item, check out ")}
              <a href="https://twitter.com/CatalogScanner" target="_blank">
                {React.string("Catalog Scanner")}
              </a>
              {React.string(" by ")}
              <a href="https://twitter.com/@ehsankia_" target="_blank">
                {React.string("Ehsan Kia")}
              </a>
              {React.string("!")}
            </p>
          </BodyCard>
          <div className=Styles.body>
            {isFetchingFromCatalogScanner
              ? <div className=Styles.fetchingCatalogScanner>
                  {React.string("Fetching from Catalog Scanner...")}
                </div>
              : React.null}
            <form onSubmit>
              <textarea
                value
                placeholder="Enter each item name on its own line"
                onChange={e => {
                  let value = ReactEvent.Form.target(e)["value"]
                  setValue(_ => value)
                }}
                className=Styles.textarea
              />
              <div className=Styles.searchButtonRow>
                <Button> {React.string("Search for items")} </Button>
              </div>
            </form>
          </div>
        </div>
      }
    | _ => React.null
    }}
  </div>
}
