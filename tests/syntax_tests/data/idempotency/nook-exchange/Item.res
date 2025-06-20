type recipeMaterial = (string, int)
type recipe = array<recipeMaterial>

type variations =
  | Single
  | OneDimension(int)
  | TwoDimensions(int, int)

type image =
  | Base(string)
  | Array(array<string>)
type type_ =
  | Item(option<int>)
  | Recipe(int)
type t = {
  id: int,
  type_: type_,
  name: string,
  image: image,
  variations: variations,
  sellPrice: option<int>,
  buyPrice: option<int>,
  recipe: option<recipe>,
  orderable: bool,
  source: option<string>,
  bodyCustomizable: bool,
  patternCustomizable: bool,
  customizeCost: option<int>,
  category: string,
  version: option<string>,
  tags: array<string>,
}

let categories = [
  "housewares",
  "miscellaneous",
  "wall-mounted",
  "wallpapers",
  "floors",
  "rugs",
  "tops",
  "bottoms",
  "dresses",
  "headwear",
  "accessories",
  "socks",
  "shoes",
  "bags",
  "umbrellas",
  "wetsuits",
  "fossils",
  "photos",
  "posters",
  "fencing",
  "tools",
  "music",
  "other",
]
let validCategoryStrings = ["furniture", "clothing", "recipes"]->Belt.Array.concat(categories)

let furnitureCategories = ["housewares", "miscellaneous", "wall-mounted"]

let clothingCategories = [
  "tops",
  "bottoms",
  "dresses",
  "headwear",
  "accessories",
  "socks",
  "shoes",
  "bags",
  "umbrellas",
  "wetsuits",
]

@val @scope("window") external itemsJson: Js.Json.t = "items"
@val @scope("window") external variantsJson: Js.Json.t = "variants"

let loadTranslation: (string, Js.Json.t => unit) => unit = %raw(`function(language, callback) {
    import(/* webpackChunkName */ './translations/' + language + '.json').then(j => callback(j.default))
  }`)

exception UnexpectedType(string)

let spaceRegex = /\s/g

exception Unexpected
let jsonToItems = (json: Js.Json.t) => {
  open Json.Decode
  let flags = json->field("flags", int)
  let recipeInfo = json->optional(
    field("recipe", json => {
      let jsonArray = Js.Json.decodeArray(json)->Belt.Option.getExn
      (
        -int(jsonArray[0]),
        string(jsonArray[1]),
        jsonArray
        ->Js.Array.sliceFrom(2)
        ->Js.Array.map(json => {
          let (quantity, itemName) = json->tuple2(int, string)
          (itemName, quantity)
        }),
      )
    }),
  )
  let item = {
    id: json->field("id", int),
    type_: Item(recipeInfo->Belt.Option.map(((recipeId, _, _)) => recipeId)),
    name: json->field("name", string),
    image: json->field(
      "image",
      oneOf(list{json => Base(string(json)), json => Array(array(string, json))}),
    ),
    variations: switch json->field("variants", array(int)) {
    | [] => Single
    | [a] => OneDimension(a)
    | [a, b] => TwoDimensions(a, b)
    | _ => throw(Unexpected)
    },
    sellPrice: json->optional(field("sell", int)),
    buyPrice: json->optional(field("buy", int)),
    recipe: recipeInfo->Belt.Option.map(((_, _, recipe)) => recipe),
    orderable: land(flags, 2) !== 0,
    source: json->optional(field("source", string)),
    bodyCustomizable: land(flags, 4) != 0,
    patternCustomizable: land(flags, 8) != 0,
    customizeCost: json->optional(field("kitCost", int)),
    category: json->field("category", string),
    version: json->optional(field("v", string)),
    tags: (json->optional(field("tags", array(string))))->Belt.Option.getWithDefault([]),
  }
  let items = switch recipeInfo {
  | Some((recipeId, recipeSource, _)) => [
      item,
      {
        ...item,
        id: recipeId,
        type_: Recipe(item.id),
        name: item.name ++ " DIY",
        sellPrice: None,
        buyPrice: None,
        source: Some(recipeSource),
        customizeCost: None,
        orderable: false,
        bodyCustomizable: false,
        patternCustomizable: false,
      },
    ]
  | None => [item]
  }
  items->Js.Array.map((item: t) => {
    let extraTags = []
    switch item.source {
    | Some(source) =>
      if source == "Jolly Redd's Treasure Trawler" {
        extraTags->Js.Array.push("redd")->ignore
      }
    | None => ()
    }
    {...item, tags: item.tags->Js.Array.concat(extraTags)}
  })
}

let all = itemsJson->Json.Decode.array(jsonToItems)->Belt.Array.concatMany

let itemMap = {
  let itemMap = Js.Dict.empty()
  all->Belt.Array.forEach(item => itemMap->Js.Dict.set(string_of_int(item.id), item))
  itemMap
}
let getItem = (~itemId) => itemMap->Js.Dict.unsafeGet(string_of_int(itemId))

exception UnexpectedVersion(string)
let getImageUrl = (~item, ~variant) =>
  Constants.cdnUrl ++
  ("/items/" ++
  (switch item.image {
  | Base(base) =>
    base ++
    switch (item.category, item.variations) {
    | (_, Single) => ""
    | (_, OneDimension(_)) => string_of_int(variant)
    | (_, TwoDimensions(_a, b)) =>
      "_" ++ (string_of_int(variant / b) ++ ("_" ++ string_of_int(mod(variant, b))))
    }
  | Array(variantImages) => variantImages[variant]
  } ++ ".png"))

let isRecipe = (~item: t) =>
  switch item.type_ {
  | Item(_) => false
  | Recipe(_) => true
  }

let getRecipeIdForItem = (~item: t) =>
  switch item.type_ {
  | Item(recipeId) => recipeId
  | Recipe(_) => throw(Constants.Uhoh)
  }

let getItemIdForRecipe = (~recipe: t) =>
  switch recipe.type_ {
  | Recipe(itemId) => itemId
  | Item(_) => throw(Constants.Uhoh)
  }

let getNumVariations = (~item) =>
  if isRecipe(~item) {
    1
  } else {
    switch item.variations {
    | Single => 1
    | OneDimension(a) => a
    | TwoDimensions(a, b) => a * b
    }
  }

let getCollapsedVariants = (~item: t) =>
  switch (item.type_, item.variations) {
  | (Recipe(_), _)
  | (_, Single) => [0]
  | (_, OneDimension(a)) => Array.make(a, None)->Belt.Array.mapWithIndex((i, _) => i)
  | (_, TwoDimensions(a, b)) =>
    if item.bodyCustomizable {
      [0]
    } else {
      Array.make(a, None)->Belt.Array.mapWithIndex((i, _) => i * b)
    }
  }

let getCanonicalVariant = (~item, ~variant) =>
  switch item.variations {
  | Single => 0
  | OneDimension(_a) => variant
  | TwoDimensions(_a, b) =>
    if item.bodyCustomizable {
      0
    } else {
      variant / b * b
    }
  }

type variantNames =
  | NameOneDimension(array<string>)
  | NameTwoDimensions((array<string>, array<string>))
let variantNames: dict<variantNames> = variantsJson->{
  open Json.Decode
  dict(
    oneOf(list{
      json => NameTwoDimensions(json->tuple2(array(string), array(string))),
      json => NameOneDimension(json->array(string)),
    }),
  )
}

let loadTranslation: (string, Js.Json.t => unit) => unit = %raw(`function(language, callback) {
    import(/* webpackChunkName */ './translations/' + language + '.json').then(j => callback(j.default))
  }`)
type translationItem = {
  name: string,
  variants: option<variantNames>,
}
type translations = {
  items: dict<translationItem>,
  materials: dict<string>,
}
let translations: ref<option<translations>> = ref(None)
let setTranslations = json => {
  open Json.Decode
  translations :=
    Some({
      items: json->field(
        "items",
        dict(json => {
          let row = Js.Json.decodeArray(json)->Belt.Option.getExn
          {
            name: string(row[0]),
            variants: Belt.Option.map(Belt.Array.get(row, 1), json =>
              json->oneOf(list{
                json => NameTwoDimensions(json->tuple2(array(string), array(string))),
                json => NameOneDimension(json->array(string)),
              })
            ),
          }
        }),
      ),
      materials: json->field("materials", dict(string)),
    })
}
let clearTranslations = () => translations := None

let getName = (item: t) =>
  switch item.type_ {
  | Recipe(itemId) =>
    open Belt
    translations.contents
    ->Option.flatMap(translations => Js.Dict.get(translations.items, string_of_int(itemId)))
    ->Option.map(translation => translation.name ++ " DIY")
    ->Option.getWithDefault(item.name)
  | Item(_) =>
    open Belt
    translations.contents
    ->Option.flatMap(translations => Js.Dict.get(translations.items, string_of_int(item.id)))
    ->Option.map(translation => translation.name)
    ->Option.getWithDefault(item.name)
  }

let getVariantName = (~item: t, ~variant: int, ~hideBody=false, ~hidePattern=false, ()) => {
  open Belt
  switch item.variations {
  | Single => None
  | OneDimension(_) =>
    switch translations.contents
    ->Option.flatMap(translations => Js.Dict.get(translations.items, string_of_int(item.id)))
    ->Option.flatMap(translationItem => translationItem.variants) {
    | Some(value) => Some(value)
    | None => variantNames->Js.Dict.get(_, string_of_int(item.id))
    }->Option.flatMap(value =>
      switch value {
      | NameOneDimension(names) => Some(Option.getExn(names[variant]))
      | _ => None
      }
    )
  | TwoDimensions(_a, b) =>
    switch translations.contents
    ->Option.flatMap(translations => Js.Dict.get(translations.items, string_of_int(item.id)))
    ->Option.flatMap(translationItem => translationItem.variants) {
    | Some(value) => Some(value)
    | None => variantNames->Js.Dict.get(_, string_of_int(item.id))
    }->Belt.Option.flatMap(value =>
      switch value {
      | NameTwoDimensions((nameA, nameB)) =>
        let bodyName = Option.getExn(nameA[variant / b])
        Some(
          (!hideBody ? bodyName : "") ++
          ((!hideBody && (!hidePattern && (b > 1 && bodyName != "")) ? " x " : "") ++
          if !hidePattern && b > 1 {
            Option.getExn(nameB[mod(variant, b)])
          } else {
            ""
          }),
        )
      | _ => None
      }
    )
  }
}

let getMaterialName = (material: string) => {
  open Belt
  translations.contents
  ->Option.flatMap(translations => Js.Dict.get(translations.materials, material))
  ->Option.getWithDefault(material)
}

let getCanonicalName = text => Js.String.toLowerCase(text)->Js.String.replaceByRe(spaceRegex, "-")
let getByName = (~name: string) => {
  let searchName = getCanonicalName(name)
  all->Belt.Array.getBy((item: t) =>
    getCanonicalName(item.name) == searchName || getCanonicalName(getName(item)) == searchName
  )
}

let getVariantByName = (~item: t, ~variantName: string) => {
  let collapsedVariants = getCollapsedVariants(~item)
  collapsedVariants->Belt.Array.getBy(variant =>
    switch getVariantName(~item, ~variant, ~hidePattern=true, ()) {
    | Some(name) => getCanonicalName(name) == getCanonicalName(variantName)
    | None => false
    }
  )
}
