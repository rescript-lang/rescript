let str = React.string

open CurriculumEditor__Types

type state = {
  loading: bool,
  contentBlocks: array<ContentBlock.t>,
  versions: array<Version.t>,
  dirtyContentBlockIds: Belt.Set.String.t,
}

type action =
  | LoadContent(array<ContentBlock.t>, array<Version.t>)
  | AddContentBlock(ContentBlock.t)
  | UpdateContentBlock(ContentBlock.t)
  | RemoveContentBlock(ContentBlock.id)
  | MoveContentBlockUp(ContentBlock.t)
  | MoveContentBlockDown(ContentBlock.t)
  | SetDirty(ContentBlock.id, bool)

let reducer = (state, action) =>
  switch action {
  | LoadContent(contentBlocks, versions) => {
      loading: false,
      contentBlocks: contentBlocks,
      versions: versions,
      dirtyContentBlockIds: Belt.Set.String.empty,
    }
  | AddContentBlock(newContentBlock) =>
    let newBlockSortIndex = newContentBlock->ContentBlock.sortIndex
    {
      ...state,
      contentBlocks: state.contentBlocks
      ->Array.map(contentBlock => {
        let sortIndex = contentBlock->ContentBlock.sortIndex

        if sortIndex < newBlockSortIndex {
          contentBlock
        } else {
          contentBlock->ContentBlock.incrementSortIndex
        }
      })
      ->Array.append([newContentBlock]),
    }
  | UpdateContentBlock(updatedContentBlock) => {
      ...state,
      contentBlocks: state.contentBlocks->Array.map(contentBlock =>
        contentBlock->ContentBlock.id == (updatedContentBlock->ContentBlock.id)
          ? updatedContentBlock
          : contentBlock
      ),
      dirtyContentBlockIds: state.dirtyContentBlockIds->Belt.Set.String.remove(
        updatedContentBlock->ContentBlock.id,
      ),
    }
  | RemoveContentBlock(contentBlockId) => {
      ...state,
      contentBlocks: state.contentBlocks->Js.Array.filter(contentBlock =>
        contentBlock->ContentBlock.id != contentBlockId
      ),
    }
  | MoveContentBlockUp(contentBlock) => {
      ...state,
      contentBlocks: state.contentBlocks->ContentBlock.moveUp(contentBlock),
    }
  | MoveContentBlockDown(contentBlock) => {
      ...state,
      contentBlocks: state.contentBlocks->ContentBlock.moveDown(contentBlock),
    }
  | SetDirty(contentBlockId, dirty) =>
    let operation = dirty ? Belt.Set.String.add : Belt.Set.String.remove
    {
      ...state,
      dirtyContentBlockIds: operation(state.dirtyContentBlockIds, contentBlockId),
    }
  }

let loadContentBlocks = (targetId, send) =>
  ContentBlock.Query.make(~targetId, ())
  ->GraphqlQuery.sendQuery(~notify=true)
  ->Js.Promise.then_(result => {
    let contentBlocks = result["contentBlocks"]->Js.Array.map(ContentBlock.makeFromJs)

    let versions = Version.makeArrayFromJs(result["versions"])

    send(LoadContent(contentBlocks, versions))

    Js.Promise.resolve()
  })
  ->ignore

let addContentBlock = (send, contentBlock) => send(AddContentBlock(contentBlock))

let removeContentBlock = (send, contentBlockId) => send(RemoveContentBlock(contentBlockId))

let moveContentBlockUp = (send, contentBlock) => send(MoveContentBlockUp(contentBlock))

let moveContentBlockDown = (send, contentBlock) => send(MoveContentBlockDown(contentBlock))

let setDirty = (contentBlockId, send, dirty) => send(SetDirty(contentBlockId, dirty))

let updateContentBlock = (send, contentBlock) => send(UpdateContentBlock(contentBlock))

let editor = (target, state, send) => {
  let currentVersion = switch state.versions {
  | [] => <span className="italic"> {"Not Versioned"->str} </span>
  | versions =>
    let latestVersion =
      versions->Array.unsafe_get(0)->Version.updatedAt->DateFns.format("MMM D, YYYY HH:MM")

    latestVersion->str
  }

  let sortedContentBlocks = state.contentBlocks->ContentBlock.sort
  let numberOfContentBlocks = state.contentBlocks->Array.length

  let removeContentBlockCB = numberOfContentBlocks > 1 ? Some(removeContentBlock(send)) : None

  <div className="mt-2">
    <div className="flex justify-between items-end">
      {switch target->Target.visibility {
      | Live =>
        <a
          href={"/targets/" ++ (target->Target.id)}
          target="_blank"
          className="py-2 px-3 font-semibold rounded-lg text-sm focus:outline-none bg-primary-100 text-primary-500">
          <FaIcon classes="fas fa-external-link-alt" />
          <span className="ml-2"> {"View as Student"->str} </span>
        </a>
      | Draft
      | Archived => React.null
      }}
      <div className="w-1/6">
        <label className="text-xs block text-gray-600 mb-1"> {"Last Updated"->str} </label>
        <span className="truncate text-left"> currentVersion </span>
      </div>
    </div>
    {sortedContentBlocks
    ->Array.mapi((index, contentBlock) => {
      let moveContentBlockUpCB = index == 0 ? None : Some(moveContentBlockUp(send))
      let moveContentBlockDownCB =
        index + 1 == numberOfContentBlocks ? None : Some(moveContentBlockDown(send))
      let isDirty = state.dirtyContentBlockIds->Belt.Set.String.has(contentBlock->ContentBlock.id)
      let updateContentBlockCB = isDirty ? Some(updateContentBlock(send)) : None

      <div key={contentBlock->ContentBlock.id}>
        <CurriculumEditor__ContentBlockCreator
          target aboveContentBlock=contentBlock addContentBlockCB={addContentBlock(send)}
        />
        <CurriculumEditor__ContentBlockEditor
          setDirtyCB={setDirty(contentBlock->ContentBlock.id, send)}
          contentBlock
          ?removeContentBlockCB
          ?moveContentBlockUpCB
          ?moveContentBlockDownCB
          ?updateContentBlockCB
        />
      </div>
    })
    ->React.array}
    <CurriculumEditor__ContentBlockCreator target addContentBlockCB={addContentBlock(send)} />
  </div>
}

@react.component
let make = (~target, ~setDirtyCB) => {
  let (state, send) = React.useReducer(
    reducer,
    {
      loading: true,
      contentBlocks: [],
      versions: [],
      dirtyContentBlockIds: Belt.Set.String.empty,
    },
  )

  React.useEffect0(() => {
    loadContentBlocks(target->Target.id, send)
    None
  })

  React.useEffect1(() => {
    let dirty = !(state.dirtyContentBlockIds->Belt.Set.String.isEmpty)
    setDirtyCB(dirty)
    None
  }, [state.dirtyContentBlockIds])

  <div className="max-w-3xl py-6 px-3 mx-auto">
    {state.loading
      ? SkeletonLoading.multiple(~count=2, ~element=SkeletonLoading.contents())
      : editor(target, state, send)}
  </div>
}
