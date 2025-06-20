open CoursesCurriculum__Types

let str = React.string

let levelZeroSelectorClasses = isSelected => {
  let defaultClasses = "w-1/2 px-4 py-2 focus:outline-none text-sm font-semibold "
  defaultClasses ++ (
    isSelected ? "bg-primary-100 text-primary-500 hover:bg-primary-100 hover:text-primary-500" : ""
  )
}

let levelName = level =>
  "L" ++ ((level->Level.number->string_of_int) ++ (": " ++ (level->Level.name)))

let selectableLevels = (orderedLevels, teamLevel, setSelectedLevelId) => {
  let teamLevelNumber = teamLevel->Level.number
  orderedLevels
  ->List.map(level => {
    let levelNumber = level->Level.number

    let icon = if levelNumber < teamLevelNumber {
      "fas fa-check text-green-500"
    } else if levelNumber == teamLevelNumber {
      "fas fa-map-marker-alt text-blue-400"
    } else if level->Level.isUnlocked {
      "inline-block"
    } else {
      "fas fa-lock text-gray-600"
    }

    <button
      className="focus:outline-none p-2 w-full text-left"
      key={level->Level.id}
      onClick={_ => setSelectedLevelId(level->Level.id)}>
      <span className="mr-2"> <FaIcon classes={"fa-fw " ++ icon} /> </span>
      {levelName(level)->str}
    </button>
  })
  ->Array.of_list
}

let untabbedLevelSelector = (selectedLevel, orderedLevels, teamLevel, setSelectedLevelId) => {
  let selected =
    <button className="font-semibold w-full px-2 h-10 flex items-center justify-between">
      <span className="flex-grow text-center truncate w-0">
        {selectedLevel->levelName->str}
      </span>
      <FaIcon classes="fas fa-caret-down ml-1" />
    </button>

  <Dropdown
    selected
    contents={selectableLevels(orderedLevels, teamLevel, setSelectedLevelId)}
    className="flex-grow cursor-pointer rounded-lg bg-primary-100 hover:bg-gray-200 hover:text-primary-500"
  />
}

let tabbedLevelSelector = (
  orderedLevels,
  teamLevel,
  selectedLevel,
  setSelectedLevelId,
  showLevelZero,
  setShowLevelZero,
  levelZero,
) => {
  let selected = hideCaret =>
    <button
      className="rounded-l-lg font-semibold w-full px-2 h-10 flex items-center justify-between">
      <span className="flex-grow text-center truncate w-0">
        {selectedLevel->levelName->str}
      </span>
      <FaIcon classes={"fas fa-caret-down ml-1" ++ (hideCaret ? " invisible" : "")} />
    </button>

  let numberedLevelSelector = showLevelZero
    ? <div
        className="cursor-pointer text-sm flex-grow rounded-l-lg hover:bg-gray-200 hover:text-primary-500"
        onClick={_ => setShowLevelZero(false)}>
        {selected(true)}
      </div>
    : <Dropdown
        key="numbered-level-selector"
        selected={selected(false)}
        contents={selectableLevels(orderedLevels, teamLevel, setSelectedLevelId)}
        className="cursor-pointer flex-grow rounded-l-lg bg-primary-100 hover:bg-gray-200 hover:text-primary-500"
      />

  [
    numberedLevelSelector,
    <button
      key="level-zero-selector"
      className={"border-l rounded-r-lg bg-white border-gray-400 font-semibold truncate hover:bg-gray-100 hover:text-primary-500 " ++
      levelZeroSelectorClasses(showLevelZero)}
      onClick={_e => setShowLevelZero(true)}>
      {levelZero->Level.name->str}
    </button>,
  ]->React.array
}

@react.component
let make = (
  ~levels,
  ~teamLevel,
  ~selectedLevel,
  ~setSelectedLevelId,
  ~showLevelZero,
  ~setShowLevelZero,
  ~levelZero,
) => {
  let orderedLevels = levels->List.filter(l => l->Level.number != 0)->Level.sort

  <div className="bg-gray-100 px-3 py-2 mt-3 md:px-0 sticky top-0 z-20">
    <div
      className="flex justify-center max-w-sm md:max-w-xl mx-auto rounded-lg border border-gray-400 h-11">
      {switch levelZero {
      | Some(levelZero) =>
        tabbedLevelSelector(
          orderedLevels,
          teamLevel,
          selectedLevel,
          setSelectedLevelId,
          showLevelZero,
          setShowLevelZero,
          levelZero,
        )
      | None => untabbedLevelSelector(selectedLevel, orderedLevels, teamLevel, setSelectedLevelId)
      }}
    </div>
  </div>
}
