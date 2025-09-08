@attr(: int)
let x = 1

@attr(?var)
let x = 1

@attr(?var when x === 1)
let x = 1

%ext(: let x: int)
%ext(:
  let x: int
  let y: float
) 

%%ext("A"; "B")
