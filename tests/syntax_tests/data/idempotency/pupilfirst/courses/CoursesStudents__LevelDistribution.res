%raw(`require("./CoursesStudents__LevelDistribution.css")`)

open CoursesStudents__Types
let str = React.string

let stylingForLevelPills = percentageStudents => {
  let emptyStyle = ReactDOMRe.Style.make()
  let styleWithWidth = ReactDOMRe.Style.make(
    ~width=(percentageStudents->Js.Float.toString) ++ "%",
    (),
  )
  if 0.0 <= percentageStudents && percentageStudents < 5.0 {
    ("w-8 flex-shrink-0", emptyStyle, "bg-green-200 text-green-800")
  } else if 5.0 <= percentageStudents && percentageStudents < 20.0 {
    ("", styleWithWidth, "bg-green-300 text-green-800")
  } else if 20.0 <= percentageStudents && percentageStudents < 40.0 {
    ("", styleWithWidth, "bg-green-400 text-green-900")
  } else if 40.0 <= percentageStudents && percentageStudents < 60.0 {
    ("", styleWithWidth, "bg-green-500 text-white")
  } else if 60.0 <= percentageStudents && percentageStudents < 80.0 {
    ("", styleWithWidth, "bg-green-600 text-white")
  } else {
    ("", styleWithWidth, "bg-green-700 text-white")
  }
}

@react.component
let make = (~levels, ~selectLevelCB) => {
  let totalStudentsInCourse = levels->Array.fold_left((x, y) => x + Level.studentsInLevel(y), 0)
  let completedLevels = Level.levelsCompletedByAllStudents(levels)
  totalStudentsInCourse > 0
    ? <div
        ariaLabel="Students level-wise distribution"
        className="w-full pt-8 max-w-3xl mx-auto hidden md:block">
        <div className="flex w-full border bg-gray-100 rounded font-semibold ">
          {levels
          ->Js.Array.filter(level => Level.number(level) != 0)
          ->Level.sort
          ->Array.map(level => {
            let percentageStudents = Level.percentageStudents(level, totalStudentsInCourse)
            let (pillClass, style, pillColor) = stylingForLevelPills(percentageStudents)
            let tip =
              <div className="text-left">
                <p> {"Level: " ++ string_of_int(Level.number(level))->str} </p>
                <p> {"Students: " ++ string_of_int(Level.studentsInLevel(level))->str} </p>
                {Level.studentsInLevel(level) != Level.teamsInLevel(level)
                  ? <p> {"Teams: " ++ string_of_int(Level.teamsInLevel(level))->str} </p>
                  : React.null}
                <p>
                  {"Percentage: " ++ Js.Float.toFixedWithPrecision(percentageStudents, ~digits=1)
                    ->str}
                </p>
              </div>
            <div
              ariaLabel={"Students in level " ++ (Level.number(level)->string_of_int)}
              className={"level-distribution__container text-center relative " ++ pillClass}
              style>
              <label
                className="absolute -mt-5 left-0 right-0 inline-block text-xs text-gray-700 text-center">
                {level->Level.shortName->str}
              </label>
              <Tooltip className="w-full" tip position=#Bottom>
                <div
                  onClick={_ => selectLevelCB(level)}
                  className={"level-distribution__pill hover:shadow-inner focus:shadow-inner relative cursor-pointer border-white text-xs leading-none text-center " ++ (
                    completedLevels->Array.mem(level)
                      ? "bg-yellow-300 text-yellow-900"
                      : Level.unlocked(level)
                      ? pillColor
                      : "level-distribution__pill--locked cursor-default bg-gray-300" ++ " text-gray-800"
                  )}>
                  {completedLevels->Array.mem(level)
                    ? <PfIcon className="if i-check-solid text-tiny" />
                    : <div>
                        <div
                          className={level->Level.unlocked
                            ? ""
                            : "level-distribution__team-count-value"}>
                          {level->Level.teamsInLevel->string_of_int->str}
                        </div>
                        {level->Level.unlocked
                          ? React.null
                          : <div className="level-distribution__locked-icon">
                              <i className="fas fa-lock text-tiny" />
                            </div>}
                      </div>}
                </div>
              </Tooltip>
            </div>
          })
          ->React.array}
        </div>
      </div>
    : React.null
}
