type sortBy =
  | Name
  | CreatedAt
  | UpdatedAt

type t = {
  searchString: option<string>,
  tags: array<string>,
  levelId: option<string>,
  sortBy: sortBy,
}

let searchString = t => t.searchString

let tags = t => t.tags

let levelId = t => t.levelId

let sortBy = t => t.sortBy

let empty = () => {
  searchString: None,
  tags: [],
  levelId: None,
  sortBy: Name,
}

let addTag = (tag, t) => {...t, tags: t.tags->Array.append([tag])}

let changeLevelId = (levelId, t) => {...t, levelId: levelId}

let changeSearchString = (searchString, t) => {...t, searchString: searchString}

let removeTag = (tag, t) => {
  ...t,
  tags: t.tags->Js.Array.filter(ts => ts != tag),
}

let removeLevelId = t => {...t, levelId: None}

let removeSearchString = t => {...t, searchString: None}

let updateSortBy = (sortBy, t) => {...t, sortBy: sortBy}

let sortByToString = t =>
  switch t.sortBy {
  | Name => "name"
  | CreatedAt => "created_at"
  | UpdatedAt => "updated_at"
  }

let dropdownOptionsForSortBy = t =>
  switch t.sortBy {
  | Name => [CreatedAt, UpdatedAt]
  | CreatedAt => [Name, UpdatedAt]
  | UpdatedAt => [Name, CreatedAt]
  }

let sortByTitle = sortBy =>
  switch sortBy {
  | Name => "Name"
  | CreatedAt => "Last Created"
  | UpdatedAt => "Last Updated"
  }

let sortByIcon = sortBy =>
  switch sortBy {
  | Name => "if i-sort-alpha-down-solid text-sm if-fw text-gray-700"
  | CreatedAt => "if i-clock-regular text-sm if-fw text-gray-700"
  | UpdatedAt => "if i-user-check-regular text-sm if-fw text-gray-700"
  }

let isEmpty = t =>
  switch (t.searchString, t.levelId, t.tags->ArrayUtils.isEmpty) {
  | (None, None, true) => true
  | (_, _, _) => false
  }

let clear = t => {...t, searchString: None, levelId: None, tags: []}
