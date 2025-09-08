open QuestionsShow__Types

let str = React.string

@react.component
let make = (~user, ~createdAt, ~textForTimeStamp) =>
  <div>
    <p className="text-xs text-gray-800">
      {textForTimeStamp ++
      (" on " ++
      (createdAt->DateFns.parseString->DateFns.format("Do MMMM, YYYY HH:mm")))->str}
    </p>
    <div
      className="p-2 flex flex-row items-center bg-orange-100 text-orange-900 border border-orange-200 rounded-lg mt-1">
      <div
        className="w-10 h-10 rounded-full bg-gray-500 text-white border border-yellow-400 flex items-center justify-center flex-shrink-0 overflow-hidden">
        <img src={user->User.avatarUrl} />
      </div>
      <div className="pl-2">
        <p className="font-semibold text-xs"> {user->User.name->str} </p>
        <p className="text-xs leadig-normal"> {user->User.title->str} </p>
      </div>
    </div>
  </div>
