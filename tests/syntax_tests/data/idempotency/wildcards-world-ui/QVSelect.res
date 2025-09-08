open Globals

let hasDecimals = value => value -. value->Float.toInt->Float.fromInt > 0.

@react.component
let make = (~submitVoteFunction: float => unit, ~maxVote: float) => {
  let (voteValue, setVoteValue) = React.useState(_ => maxVote)
  let (voteText, setVoteText) = React.useState(_ =>
    maxVote->toFixedWithPrecisionNoTrailingZeros(~digits=9)
  )
  let customVote: string => unit = voteValueString => {
    let optVoteValue = Float.fromString(voteValueString)

    switch optVoteValue {
    | None =>
      if voteValueString == "" {
        setVoteText(_ => voteValueString)
      } else {
        ()
      }
    | Some(voteValue) =>
      setVoteText(_ => voteValueString)
      if voteValue < 0. {
        // If negative set to 0
        setVoteValue(_ => 0.)
      } else if voteValue >= maxVote {
        // If above max, set to max
        setVoteText(_ => maxVote->toFixedWithPrecisionNoTrailingZeros(~digits=9))
        setVoteValue(_ => maxVote)
      } else {
        setVoteValue(_ => voteValue)
      }
    }
  }

  <Rimble.Flex flexWrap="wrap" alignItems="space-between">
    {React.array(
      [1., 2., 3., 4., 5.]->Array.map(x => {
        let disabled = x >= maxVote
        <Rimble.Box key={x->Js.Float.toString} width=[1., 0.32]>
          <Rimble.Button
            className={
              open Css
              style(list{width(#percent(90.)), margin(#percent(5.))})
            }
            key={x->Float.toString}
            onClick={_ => submitVoteFunction(x)}
            disabled>
            {(x->Float.toString ++ (" Vote = " ++ ((x *. x)->Float.toString ++ " WLT")))->restr}
          </Rimble.Button>
        </Rimble.Box>
      }),
    )}
    <Rimble.Box width=[1., 0.32]>
      <form
        className={
          open Css
          style(list{width(#percent(90.)), margin(#percent(5.))})
        }>
        <input
          value=voteText
          type_="number"
          min=0
          max={maxVote->Float.toString}
          onChange={event => {
            let voteString = ReactEvent.Form.target(event)["value"]
            customVote(voteString)
          }}
          className={
            open Css
            style(list{
              width(#calc(#sub, #percent(94.), #px(2))),
              padding(#percent(3.)),
              borderWidth(#px(1)),
              borderRadius(#px(3)),
            })
          }
        />
        <button
          onClick={_ => submitVoteFunction(voteValue)}
          className={
            open Css
            style(list{width(#percent(100.)), marginTop(#px(2)), fontSize(#px(11))})
          }>
          {(voteValue->toFixedWithPrecisionNoTrailingZeros(~digits=9) ++
            (" votes = " ++
            ((voteValue *. voteValue)->toFixedWithPrecisionNoTrailingZeros(~digits=3) ++ " WLT")))
            ->restr}
        </button>
      </form>
    </Rimble.Box>
  </Rimble.Flex>
}
