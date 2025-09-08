type entity =
  | Director
  | Student({
      name: string,
      reportCard: {
        passing: bool,
        score: int
      }
    })

type user = {
  name: string,
  address: {
    street: string,
    country: string,
  }
}

let make = (props: {handleClick: Click.t => unit, value: string}) => render(props)
