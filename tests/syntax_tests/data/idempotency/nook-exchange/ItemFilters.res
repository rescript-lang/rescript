module Styles = {
  open Css
  let select = style(list{
    borderColor(hex("00000020")),
    fontSize(px(16)),
    height(px(37)),
    marginRight(px(8)),
    marginBottom(px(8)),
  })
  let selectSort = style(list{marginRight(px(16))})
  let textInputWrapper = style(list{display(inlineBlock), position(relative), marginRight(px(8))})
  let textInput = style(list{
    backgroundColor(Colors.white),
    border(px(1), solid, hex("00000020")),
    fontSize(px(16)),
    marginBottom(px(8)),
    padding2(~v=zero, ~h=px(12)),
    borderRadius(px(4)),
    height(px(35)),
    width(px(180)),
  })
  @module("./assets/close.png") external closePng: string = "default"
  let textInputClear = style(list{
    position(absolute),
    right(px(8)),
    top(px(10)),
    width(px(16)),
    height(px(16)),
    backgroundColor(transparent),
    backgroundImage(url(closePng)),
    backgroundSize(#size(px(16), px(16))),
    backgroundRepeat(noRepeat),
    backgroundPosition(center),
    borderStyle(none),
    opacity(0.5),
    padding(zero),
  })
  let inputWithValue = style(list{boxShadow(Shadow.box(~spread=px(2), hex("3aa56380")))})
  let clearFilters = style(list{
    alignSelf(flexStart),
    fontSize(px(16)),
    lineHeight(px(37)),
    paddingRight(px(16)),
    whiteSpace(#nowrap),
    textDecoration(none),
    hover(list{textDecoration(underline)}),
  })
  let root = style(list{display(flexBox), flexWrap(wrap)})
  let rootScrollerSpacer = style(list{
    media("(max-width: 600px)", list{flexShrink(0.), width(px(1)), height(px(1))}),
  })
  let pager = style(list{fontSize(px(16)), lineHeight(px(32))})
  let pagerArrow = style(list{
    fontSize(px(24)),
    lineHeight(px(24)),
    textDecoration(none),
    opacity(0.8),
    padding2(~v=zero, ~h=px(8)),
    transition(~duration=200, "all"),
    hover(list{opacity(1.)}),
  })
}

type sort =
  | ABC
  | SellPriceDesc
  | SellPriceAsc
  | UserDefault
  | UserTimeUpdated
  | UserNote
  | ListTimeAdded

type mask =
  | Orderable
  | Craftable
  | NotOrderable

type excludables =
  | Catalog
  | CanCraft
  | Wishlist

type t = {
  text: string,
  mask: option<mask>,
  category: option<string>,
  exclude: array<excludables>,
  sort: sort,
}

let serializeSort = (~sort, ~defaultSort) =>
  if sort != defaultSort && sort != UserDefault {
    Some((
      "s",
      switch sort {
      | ABC => "abc"
      | SellPriceDesc => "pd"
      | SellPriceAsc => "pa"
      | UserDefault => ""
      | UserTimeUpdated => "tu"
      | UserNote => "note"
      | ListTimeAdded => ""
      },
    ))
  } else {
    None
  }

let serialize = (~filters, ~defaultSort, ~pageOffset) => {
  let p = []
  switch serializeSort(~sort=filters.sort, ~defaultSort) {
  | Some(param) => p->Js.Array.push(param)->ignore
  | None => ()
  }
  if filters.text != "" {
    p->Js.Array.push(("q", filters.text))->ignore
  }
  switch filters.mask {
  | Some(Orderable) => p->Js.Array.push(("orderable", ""))->ignore
  | Some(NotOrderable) => p->Js.Array.push(("not-orderable", ""))->ignore
  | Some(Craftable) => p->Js.Array.push(("craftable", ""))->ignore
  | None => ()
  }
  switch filters.category {
  | Some(category) => p->Js.Array.push(("c", category))->ignore
  | None => ()
  }
  if Js.Array.length(filters.exclude) > 0 {
    p
    ->Js.Array.push((
      "e",
      filters.exclude->Belt.Array.map(exclude =>
        switch exclude {
        | Catalog => "catalog"
        | CanCraft => "can-craft"
        | Wishlist => "wishlist"
        }
      )->Js.Array.joinWith(","),
    ))
    ->ignore
  }
  if pageOffset != 0 {
    p->Js.Array.push(("p", string_of_int(pageOffset + 1)))->ignore
  }
  p
}

let sortFromUrlSearch = (~searchParams, ~defaultSort) => {
  open Webapi.Url.URLSearchParams
  switch searchParams->get("s") {
  | Some("abc") => ABC
  | Some("pd") => SellPriceDesc
  | Some("pa") => SellPriceAsc
  | Some("tu") => UserTimeUpdated
  | Some("note") => UserNote
  | _ => defaultSort
  }
}

let fromUrlSearch = (~urlSearch, ~defaultSort) => {
  open Belt
  open Webapi.Url.URLSearchParams
  let searchParams = make(urlSearch)
  (
    {
      text: (searchParams->get("q"))->Option.getWithDefault(""),
      mask: searchParams->has("orderable")
        ? Some(Orderable)
        : searchParams->has("craftable")
        ? Some(Craftable)
        : searchParams->has("not-orderable")
        ? Some(NotOrderable)
        : None,
      category: Option.flatMap(searchParams->get("c"), category =>
        if Item.validCategoryStrings->Js.Array.includes(category) {
          Some(category)
        } else {
          None
        }
      ),
      exclude: switch searchParams->get("e") {
      | Some(e) =>
        (e->Js.String.split(","))
          ->Belt.Array.keepMap(fragment =>
            switch fragment {
            | "wishlist" => Some(Wishlist)
            | "can-craft" => Some(CanCraft)
            | "catalog" => Some(Catalog)
            | _ => None
            }
          )
      | None => []
      },
      sort: sortFromUrlSearch(~searchParams, ~defaultSort),
    },
    Option.map(searchParams->get("p"), s => int_of_string(s) - 1)->Option.getWithDefault(0),
  )
}

let doesItemMatchCategory = (~item: Item.t, ~category: string) =>
  switch category {
  | "furniture" => Item.furnitureCategories->Js.Array.includes(item.category)
  | "clothing" => Item.clothingCategories->Js.Array.includes(item.category)
  | "recipes" => Item.isRecipe(~item)
  | category => item.category == category
  }

let removeAccents = str =>
  str->Js.String.normalizeByForm("NFD")->Js.String.replaceByRe(/[\u0300-\u036f]/g, "")

let doesItemMatchFilters = (~item: Item.t, ~filters: t) =>
  switch filters.text {
  | "" => true
  | text =>
    let textLower = Js.String.toLowerCase(text)
    switch item.source {
    | Some(source) => textLower == Js.String.toLowerCase(source)
    | None => false
    } || {
      let fragments =
        (textLower->Js.String.splitByRe(/[\s-]+/))->Belt.Array.keepMap(x => x)
      fragments->Belt.Array.every(fragment =>
        Js.String.toLowerCase(Item.getName(item))
        ->removeAccents
        ->Js.String.includes(removeAccents(fragment)) || item.tags->Js.Array.includes(fragment)
      )
    }
  } &&
  (switch filters.mask {
  | Some(Orderable) => item.orderable
  | Some(Craftable) => item.recipe !== None
  | Some(NotOrderable) => !item.orderable && item.recipe === None
  | None => true
  } &&
  switch filters.category {
  | Some(category) => doesItemMatchCategory(~item, ~category)
  | None => true
  })

let compareArrays = (a, b) => {
  let rv = ref(None)
  let i = ref(0)
  while i.contents < Js.Array.length(a) && rv.contents === None {
    if a[i.contents] < b[i.contents] {
      rv := Some(-1)
    } else if a[i.contents] > b[i.contents] {
      rv := Some(1)
    }
    i := i.contents + 1
  }
  Belt.Option.getWithDefault(rv.contents, 0)
}

let compareItemsABC = (a: Item.t, b: Item.t) => {
  // hack to sort "wooden-" before "wooden "
  let aName = Item.getName(a)->Js.String.replaceByRe(/-/g, " ")
  let bName = Item.getName(b)->Js.String.replaceByRe(/-/g, " ")
  int_of_float(Js.String.localeCompare(bName, aName))
}
let compareItemsSellPriceDesc = (a: Item.t, b: Item.t) =>
  compareArrays(
    [Belt.Option.getWithDefault(b.sellPrice, 0), compareItemsABC(a, b)],
    [Belt.Option.getWithDefault(a.sellPrice, 0), 0],
  )
let compareItemsSellPriceAsc = (a: Item.t, b: Item.t) =>
  compareArrays(
    [Belt.Option.getWithDefault(a.sellPrice, 0), compareItemsABC(a, b)],
    [Belt.Option.getWithDefault(b.sellPrice, 0), 0],
  )

exception UnexpectedSort(sort)
let getSort = (~sort) =>
  switch sort {
  | ABC => compareItemsABC
  | SellPriceDesc => compareItemsSellPriceDesc
  | SellPriceAsc => compareItemsSellPriceAsc
  | ListTimeAdded
  | UserTimeUpdated
  | UserNote
  | UserDefault =>
    raise(UnexpectedSort(sort))
  }

let wrapWithVariantSort = (sort, a, b) => {
  let order = sort(a, b)
  if order != 0 {
    order
  } else {
    let ((_, aVariant), _) = a
    let ((_, bVariant), _) = b
    aVariant - bVariant
  }
}
let getUserItemSort = (~prioritizeViewerStatuses: array<User.itemStatus>=[], ~sort) =>
  switch sort {
  | ABC =>
    wrapWithVariantSort((((aId, _), _), ((bId, _), _)) =>
      compareItemsABC(Item.getItem(~itemId=aId), Item.getItem(~itemId=bId))
    )
  | SellPriceDesc =>
    wrapWithVariantSort((((aId, _), _), ((bId, _), _)) =>
      compareItemsSellPriceDesc(Item.getItem(~itemId=aId), Item.getItem(~itemId=bId))
    )
  | SellPriceAsc =>
    wrapWithVariantSort((((aId, _), _), ((bId, _), _)) =>
      compareItemsSellPriceAsc(Item.getItem(~itemId=aId), Item.getItem(~itemId=bId))
    )
  | UserDefault =>
    wrapWithVariantSort((
      ((aId, aVariant), {priorityTimestamp: aPriorityTimestamp}: User.item),
      ((bId, bVariant), {priorityTimestamp: bPriorityTimestamp}: User.item),
    ) => {
      let aItem = Item.getItem(~itemId=aId)
      let bItem = Item.getItem(~itemId=bId)
      compareArrays(
        [
          switch UserStore.getItem(~itemId=aId, ~variation=aVariant) {
          | Some(aUserItem) =>
            prioritizeViewerStatuses->Js.Array.includes(aUserItem.status) ? -1. : 0.
          | None => 0.
          },
          -.Belt.Option.getWithDefault(aPriorityTimestamp, 0.),
          Item.categories->Js.Array.indexOf(aItem.category)->float_of_int,
          float_of_int(compareItemsABC(aItem, bItem)),
        ],
        [
          switch UserStore.getItem(~itemId=bId, ~variation=bVariant) {
          | Some(bUserItem) =>
            prioritizeViewerStatuses->Js.Array.includes(bUserItem.status) ? -1. : 0.
          | None => 0.
          },
          -.Belt.Option.getWithDefault(bPriorityTimestamp, 0.),
          Item.categories->Js.Array.indexOf(bItem.category)->float_of_int,
          0.,
        ],
      )
    })
  | UserTimeUpdated =>
    wrapWithVariantSort((((aId, _), aUserItem: User.item), ((bId, _), bUserItem: User.item)) => {
      let aItem = Item.getItem(~itemId=aId)
      let bItem = Item.getItem(~itemId=bId)
      compareArrays(
        [
          -.(aUserItem.timeUpdated->Belt.Option.getWithDefault(0.)),
          float_of_int(compareItemsABC(aItem, bItem)),
        ],
        [-.(bUserItem.timeUpdated->Belt.Option.getWithDefault(0.)), 0.],
      )
    })
  | UserNote =>
    wrapWithVariantSort((((aId, _), aUserItem: User.item), ((bId, _), bUserItem: User.item)) => {
      let aItem = Item.getItem(~itemId=aId)
      let bItem = Item.getItem(~itemId=bId)
      compareArrays(
        [
          switch (aUserItem.note, bUserItem.note) {
          | ("", _) => 1
          | (_, "") => -1
          | (a, b) => int_of_float(a->Js.String.localeCompare(b))
          },
          compareItemsABC(aItem, bItem),
        ],
        [0, 0],
      )
    })
  | ListTimeAdded => raise(UnexpectedSort(sort))
  }

module Pager = {
  @react.component
  let make = (~numResults, ~pageOffset, ~numResultsPerPage, ~setPageOffset, ~className=?, ()) =>
    if numResults > 8 {
      <div className={Cn.make(list{Styles.pager, Cn.unpack(className)})}>
        {pageOffset > 0
          ? <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                setPageOffset(pageOffset => pageOffset - 1)
              }}
              className=Styles.pagerArrow>
              {React.string("<")}
            </a>
          : React.null}
        {React.string(
          "Showing " ++
          (string_of_int(pageOffset * numResultsPerPage + 1) ++
          (" - " ++
          (string_of_int(Js.Math.min_int((pageOffset + 1) * numResultsPerPage, numResults)) ++
          (" of " ++ string_of_int(numResults))))),
        )}
        {pageOffset < (numResults - 1) / numResultsPerPage
          ? <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                setPageOffset(pageOffset => pageOffset + 1)
              }}
              className=Styles.pagerArrow>
              {React.string(">")}
            </a>
          : React.null}
      </div>
    } else {
      React.null
    }
}

external unsafeAsHtmlInputElement: 'a => Webapi.Dom.HtmlInputElement.t = "%identity"

let getCategoryLabel = category =>
  switch category {
  | "recipes" => "Recipes"
  | "furniture" => "Furniture"
  | "clothing" => "Clothing"
  | category => Utils.capitalizeFirstLetter(category)
  }

module CategoryButtons = {
  module CategoryStyles = {
    open Css
    let button = style(list{marginRight(px(8)), transition(~duration=200, "all")})
    let buttonNotSelected = style(list{
      backgroundColor(hex("8fcaa3e0")),
      hover(list{backgroundColor(hex("3aa563e0"))}),
    })
    let select = style(list{
      height(px(37)),
      opacity(0.7),
      marginRight(px(16)),
      marginBottom(zero),
      hover(list{opacity(1.)}),
    })
    let selectSelected = style(list{height(px(37)), opacity(1.)})
    let root = style(list{paddingBottom(px(8)), whiteSpace(#nowrap)})
  }

  @react.component
  let make = (~filters: t, ~onChange, ~userItemIds: option<array<int>>=?, ()) => {
    let shouldRenderCategory = category =>
      switch userItemIds {
      | Some(userItemIds) =>
        userItemIds->Belt.Array.some(itemId =>
          doesItemMatchCategory(~item=Item.getItem(~itemId), ~category)
        )
      | None => true
      }
    let renderButton = category =>
      if shouldRenderCategory(category) {
        let isSelected = filters.category == Some(category)
        <Button
          onClick={_ =>
            onChange({
              ...filters,
              text: "",
              category: isSelected ? None : Some(category),
            })}
          className={Cn.make(list{
            CategoryStyles.button,
            Cn.ifTrue(CategoryStyles.buttonNotSelected, !isSelected),
          })}
          key=category>
          {React.string(getCategoryLabel(category))}
        </Button>
      } else {
        React.null
      }

    let selectCategories = Belt.Array.concatMany([
      ["housewares", "miscellaneous", "wall-mounted", "wallpapers", "floors", "rugs", "other"],
      Item.clothingCategories,
      ["fossils", "photos", "posters", "fencing", "tools", "music"],
    ])

    <div className=CategoryStyles.root>
      <Button
        onClick={_ => onChange({...filters, category: None})}
        className={Cn.make(list{
          CategoryStyles.button,
          Cn.ifTrue(CategoryStyles.buttonNotSelected, filters.category != None),
        })}>
        {React.string("Everything!")}
      </Button>
      {renderButton("furniture")}
      {renderButton("recipes")}
      {renderButton("clothing")}
      <select
        value={switch filters.category {
        | Some(category) => selectCategories->Js.Array.includes(category) ? category : ""
        | None => ""
        }}
        onChange={e => {
          let value = ReactEvent.Form.target(e)["value"]
          onChange({
            ...filters,
            text: "",
            category: switch value {
            | "" => None
            | category => Some(category)
            },
          })
        }}
        className={Cn.make(list{
          Styles.select,
          CategoryStyles.select,
          Cn.ifTrue(
            CategoryStyles.selectSelected,
            switch filters.category {
            | Some(category) => selectCategories->Js.Array.includes(category)
            | None => false
            },
          ),
        })}>
        <option value=""> {React.string("-- Categories")} </option>
        {selectCategories
        ->Belt.Array.mapU((. category) =>
          shouldRenderCategory(category)
            ? <option value=category key=category>
                {React.string(getCategoryLabel(category))}
              </option>
            : React.null
        )
        ->React.array}
      </select>
    </div>
  }
}

module AdvancedFilter = {
  module Styles = {
    open Css
    let link = style(list{
      fontSize(px(16)),
      textDecoration(none),
      lineHeight(px(32)),
      marginBottom(px(8)),
      hover(list{textDecoration(underline)}),
      media("(min-width: 940px)", list{lineHeight(px(37))}),
    })
    let linkSelected = style(list{fontWeight(#num(800))})
    let root = style(list{
      padding2(~v=px(24), ~h=px(32)),
      width(vw(90.)),
      boxSizing(borderBox),
      maxWidth(px(360)),
    })
    let sectionTitle = style(list{fontSize(px(20)), marginBottom(px(8))})
    let option = style(list{
      display(flexBox),
      justifyContent(spaceBetween),
      alignItems(center),
      selector("& > input", list{fontSize(px(16)), margin(zero)}),
    })
    let optionLabel = style(list{fontSize(px(16)), lineHeight(px(24))})
  }

  module AdvancedFilterModal = {
    @react.component
    let make = (~filters: t, ~onChange, ~onClose) => {
      let onChangeCheckbox = (excludable, e) => {
        let checked = ReactEvent.Form.target(e)["checked"]
        if checked {
          onChange({
            ...filters,
            exclude: if filters.exclude->Js.Array.includes(excludable) {
              filters.exclude
            } else {
              filters.exclude->Belt.Array.concat([excludable])
            },
          })
        } else {
          onChange({
            ...filters,
            exclude: filters.exclude->Belt.Array.keep(a => a !== excludable),
          })
        }
      }

      <Modal onBackdropClick={_ => onClose()}>
        <div className=Styles.root>
          <div className=Styles.sectionTitle> {React.string("Hide items from")} </div>
          <div>
            <label className=Styles.option>
              <span className=Styles.optionLabel> {React.string("My Catalog")} </span>
              <input
                type_="checkbox"
                value="catalog"
                checked={filters.exclude->Js.Array.includes(Catalog)}
                onChange={onChangeCheckbox(Catalog)}
              />
            </label>
          </div>
          <div>
            <label className=Styles.option>
              <span className=Styles.optionLabel> {React.string("Can Craft")} </span>
              <input
                type_="checkbox"
                value="can-craft"
                checked={filters.exclude->Js.Array.includes(CanCraft)}
                onChange={onChangeCheckbox(CanCraft)}
              />
            </label>
          </div>
          <div>
            <label className=Styles.option>
              <span className=Styles.optionLabel> {React.string("My Wishlist")} </span>
              <input
                type_="checkbox"
                value="wishlist"
                checked={filters.exclude->Js.Array.includes(Wishlist)}
                onChange={onChangeCheckbox(Wishlist)}
              />
            </label>
          </div>
        </div>
        <Modal.CloseButton onClose />
      </Modal>
    }
  }

  @react.component
  let make = (~filters, ~onChange, ~className=?, ()) => {
    let (showModal, setShowModal) = React.useState(() => false)
    <>
      <a
        href="#"
        onClick={e => {
          ReactEvent.Mouse.preventDefault(e)
          setShowModal(_ => true)
        }}
        className={Cn.make(list{
          Styles.link,
          Cn.ifTrue(Styles.linkSelected, filters.exclude != []),
          Cn.unpack(className),
        })}>
        {React.string("Advanced")}
      </a>
      {showModal
        ? <AdvancedFilterModal filters onChange onClose={() => setShowModal(_ => false)} />
        : React.null}
    </>
  }
}

module UserCategorySelector = {
  module CategoryStyles = {
    open Css
    let select = style(list{height(px(37))})
  }

  @react.component
  let make = (~filters: t, ~onChange, ~userItemIds: array<int>) => {
    let shouldRenderCategory = category =>
      userItemIds->Belt.Array.some(itemId =>
        doesItemMatchCategory(~item=Item.getItem(~itemId), ~category)
      )

    <select
      value={Belt.Option.getWithDefault(filters.category, "")}
      onChange={e => {
        let value = ReactEvent.Form.target(e)["value"]
        if value == "" {
          onChange({...filters, category: None})
        } else {
          onChange({...filters, text: "", category: Some(value)})
        }
      }}
      className={Cn.make(list{
        Styles.select,
        CategoryStyles.select,
        Cn.ifTrue(Styles.inputWithValue, filters.category != None),
      })}>
      <option value=""> {React.string("All categories")} </option>
      {Item.validCategoryStrings
      ->Belt.Array.mapU((. category) =>
        shouldRenderCategory(category)
          ? <option value=category key=category>
              {React.string(Utils.capitalizeFirstLetter(category))}
            </option>
          : React.null
      )
      ->React.array}
    </select>
  }
}

module SortSelector = {
  @react.component
  let make = (
    ~sort,
    ~onChange,
    ~onListPage=false,
    ~userItemIds,
    ~isViewingSelf,
    ~className=?,
    (),
  ) =>
    <select
      value={switch sort {
      | ABC => "abc"
      | SellPriceDesc => "sell-desc"
      | SellPriceAsc => "sell-asc"
      | UserDefault => "user-default"
      | UserTimeUpdated => "time-updated"
      | UserNote => "note"
      | ListTimeAdded => "list-added"
      }}
      onChange={e => {
        let value = ReactEvent.Form.target(e)["value"]
        onChange(
          switch value {
          | "abc" => ABC
          | "sell-desc" => SellPriceDesc
          | "sell-asc" => SellPriceAsc
          | "user-default" => UserDefault
          | "time-updated" => UserTimeUpdated
          | "note" => UserNote
          | "list-added" => ListTimeAdded
          | _ => SellPriceDesc
          },
        )
      }}
      className={Cn.unpack(className)}>
      {if onListPage {
        <option value="list-added"> {React.string(j`Time Added ↓`)} </option>
      } else {
        React.null
      }}
      {if userItemIds !== None {
        <>
          <option value="user-default">
            {React.string(isViewingSelf ? "Sort: Category" : "Sort: Default")}
          </option>
          <option value="time-updated"> {React.string(j`Time Updated ↓`)} </option>
        </>
      } else {
        React.null
      }}
      <option value="sell-desc"> {React.string(j`Sell Price ↓`)} </option>
      <option value="sell-asc"> {React.string(j`Sell Price ↑`)} </option>
      <option value="abc"> {React.string("A - Z")} </option>
      {if userItemIds !== None {
        <option value="note"> {React.string(j`Item Note`)} </option>
      } else {
        React.null
      }}
    </select>
}

@get
external activeElementForDocument: Webapi.Dom.Document.t => Webapi.Dom.HtmlElement.t =
  "activeElement"

let isInputActiveElement = () => {
  open Webapi.Dom

  let activeElementTagName = activeElementForDocument(document)->HtmlElement.tagName
  activeElementTagName == "INPUT" ||
    (activeElementTagName == "SELECT" ||
    activeElementTagName == "TEXTAREA")
}

@react.component
let make = (
  ~filters,
  ~onChange,
  ~userItemIds: option<array<int>>=?,
  ~isViewingSelf=false,
  ~className=?,
  (),
) => {
  let inputTextRef = React.useRef(Js.Nullable.null)
  let updateTextTimeoutRef = React.useRef(None)
  React.useEffect1(() => {
    {
      open Webapi.Dom
      Utils.getElementForDomRef(inputTextRef)
      ->unsafeAsHtmlInputElement
      ->HtmlInputElement.setValue(filters.text)
    }

    Some(
      () => {
        switch React.Ref.current(updateTextTimeoutRef) {
        | Some(updateTextTimeout) => Js.Global.clearTimeout(updateTextTimeout)
        | None => ()
        }
        React.Ref.setCurrent(updateTextTimeoutRef, None)
      },
    )
  }, [filters])

  React.useEffect0(() => {
    open Webapi.Dom
    let onKeyDown = e =>
      switch KeyboardEvent.key(e) {
      | "Esc"
      | "Escape" =>
        let url = ReasonReactRouter.dangerouslyGetInitialUrl()
        // don't trigger if ItemDetailOverlay is shown
        if !(url.hash->Js.Re.test_(/i(-?\d+)(:(\d+))?/g)) {
          onChange({...filters, text: ""})
        }
      | "/" =>
        if !isInputActiveElement() {
          Js.Global.setTimeout(
            () =>
              Utils.getElementForDomRef(inputTextRef)
              ->unsafeAsHtmlInputElement
              ->HtmlInputElement.focus,
            20,
          )->ignore
        }
      | _ => ()
      }
    window->Window.addKeyDownEventListener(onKeyDown)
    Some(() => window->Window.removeKeyDownEventListener(onKeyDown))
  })

  <div className={Cn.make(list{Styles.root, Cn.unpack(className)})}>
    <div className=Styles.textInputWrapper>
      <input
        type_="text"
        ref={ReactDOMRe.Ref.domRef(inputTextRef)}
        placeholder="Search.. Esc to clear"
        defaultValue=filters.text
        onChange={e => {
          let value = ReactEvent.Form.target(e)["value"]
          switch React.Ref.current(updateTextTimeoutRef) {
          | Some(updateTextTimeout) => Js.Global.clearTimeout(updateTextTimeout)
          | None => ()
          }
          React.Ref.setCurrent(updateTextTimeoutRef, Some(Js.Global.setTimeout(() => {
                React.Ref.setCurrent(updateTextTimeoutRef, None)
                onChange({...filters, text: value})
              }, 500)))
        }}
        className={Cn.make(list{
          Styles.textInput,
          Cn.ifTrue(Styles.inputWithValue, filters.text != ""),
        })}
      />
      {filters.text != ""
        ? <button onClick={e => onChange({...filters, text: ""})} className=Styles.textInputClear />
        : React.null}
    </div>
    {switch userItemIds {
    | Some(userItemIds) => <UserCategorySelector userItemIds filters onChange />
    | None => React.null
    }}
    <select
      value={switch filters.mask {
      | Some(Orderable) => "orderable"
      | Some(Craftable) => "craftable"
      | Some(NotOrderable) => "not-orderable"
      | None => "none"
      }}
      onChange={e => {
        let value = ReactEvent.Form.target(e)["value"]
        onChange({
          ...filters,
          mask: switch value {
          | "orderable" => Some(Orderable)
          | "craftable" => Some(Craftable)
          | "not-orderable" => Some(NotOrderable)
          | "none" => None
          | _ => None
          },
        })
      }}
      className={Cn.make(list{
        Styles.select,
        Cn.ifTrue(Styles.inputWithValue, filters.mask != None),
      })}>
      <option value="none"> {React.string("No Filter")} </option>
      <option value="orderable"> {React.string("Orderable")} </option>
      <option value="craftable"> {React.string("Craftable")} </option>
      <option value="not-orderable"> {React.string("Not Orderable")} </option>
    </select>
    <SortSelector
      sort=filters.sort
      onChange={sort => onChange({...filters, sort: sort})}
      userItemIds
      isViewingSelf
      className={Cn.make(list{Styles.select, Styles.selectSort})}
    />
    {if (
      filters.text != "" ||
        (filters.mask != None ||
        (filters.exclude != [] || filters.category != None))
    ) {
      <a
        href="#"
        onClick={e => {
          ReactEvent.Mouse.preventDefault(e)
          onChange({
            text: "",
            mask: None,
            category: None,
            exclude: [],
            sort: filters.sort,
          })
        }}
        className=Styles.clearFilters>
        {React.string("Clear filters")}
      </a>
    } else {
      React.null
    }}
    <div className=Styles.rootScrollerSpacer />
  </div>
}
