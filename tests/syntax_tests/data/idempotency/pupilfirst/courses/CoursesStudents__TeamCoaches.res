let str = React.string

let avatarClasses = size => {
  let (defaultSize, mdSize) = size
  "w-" ++
  (defaultSize ++
  (" h-" ++
  (defaultSize ++
  (" md:w-" ++
  (mdSize ++
  (" md:h-" ++
  (mdSize ++
  " text-xs border border-gray-400 rounded-full overflow-hidden flex-shrink-0 object-cover")))))))
}

let avatar = (~size=("8", "10"), avatarUrl, name) =>
  switch avatarUrl {
  | Some(avatarUrl) => <img className={avatarClasses(size)} src=avatarUrl />
  | None => <Avatar name className={avatarClasses(size)} />
  }

@react.component
let make = (
  ~tooltipPosition=#Top,
  ~defaultAvatarSize="6",
  ~mdAvatarSize="8",
  ~title,
  ~className,
  ~coaches,
) =>
  if coaches->ArrayUtils.isNotEmpty {
    let listedCoaches =
      coaches->Array.length <= 4 ? coaches : coaches->Js.Array.slice(~start=0, ~end_=3)

    let otherCoaches = if coaches->Array.length > 4 {
      let names =
        coaches
        ->Js.Array.sliceFrom(3)
        ->Js.Array.map(coach =>
          <div key={coach->UserProxy.userId}> {coach->UserProxy.name->str} </div>
        )
        ->React.array

      let count = (coaches->Array.length) - 3
      Some((names, count))
    } else {
      None
    }

    <div className>
      <div className="text-xs"> title </div>
      <div className="inline-flex">
        {listedCoaches
        ->Array.map(coach =>
          <Tooltip
            position=tooltipPosition
            tip={coach->UserProxy.name->str}
            className="-mr-1"
            key={coach->UserProxy.userId}>
            {avatar(
              ~size=(defaultAvatarSize, mdAvatarSize),
              coach->UserProxy.avatarUrl,
              coach->UserProxy.name,
            )}
          </Tooltip>
        )
        ->React.array}
        {otherCoaches->OptionUtils.mapWithDefault(
          ((names, count)) =>
            <Tooltip tip=names className="-mr-1">
              <Avatar
                name={"+ " ++ (count->string_of_int)}
                className={avatarClasses((defaultAvatarSize, mdAvatarSize))}
                colors=("#EEE", "#000")
              />
            </Tooltip>,
          React.null,
        )}
      </div>
    </div>
  } else {
    <div />
  }
