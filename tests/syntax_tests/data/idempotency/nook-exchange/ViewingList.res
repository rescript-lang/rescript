type t =
  | ForTrade
  | CanCraft
  | Wishlist
  | Catalog

let viewingListToUrl = viewingList =>
  switch viewingList {
  | Wishlist => "wishlist"
  | ForTrade => "for-trade"
  | CanCraft => "can-craft"
  | Catalog => "catalog"
  }
let urlToViewingList = url =>
  switch url {
  | "for-trade" => Some(ForTrade)
  | "can-craft" => Some(CanCraft)
  | "wishlist" => Some(Wishlist)
  | "catalog" => Some(Catalog)
  | _ => None
  }
let viewingListToEmoji = viewingList =>
  switch viewingList {
  | Wishlist => `🙏`
  | ForTrade => `🤝`
  | CanCraft => `🔨`
  | Catalog => `📖`
  }
let viewingListToString = viewingList =>
  switch viewingList {
  | Wishlist => "Wishlist"
  | ForTrade => "For Trade"
  | CanCraft => "Can Craft"
  | Catalog => "Catalog"
  }

let doesUserItemStatusMatchViewingList = (userItemStatus: User.itemStatus, viewingList: t) =>
  switch (userItemStatus, viewingList) {
  | (ForTrade, ForTrade)
  | (CanCraft, CanCraft)
  | (Wishlist, Wishlist)
  | (ForTrade, Catalog)
  | (CanCraft, Catalog)
  | (CatalogOnly, Catalog) => true
  | _ => false
  }
