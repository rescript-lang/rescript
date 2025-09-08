open Reprocessing

let g = 40.

let birdRad = 15.

let scrollRate = 175.

let jumpSpeed = -11.

let pipeColor = Utils.color(~r=80, ~g=185, ~b=0, ~a=255)

type pipeT = (float, float)

type stateT = {
  pos: (float, float),
  vy: float,
  running: bool,
  pipes: list<pipeT>,
  score: int,
  angle: float,
  worldPos: float,
  font: Reprocessing.fontT,
  img: Reprocessing.imageT,
}

let gapRadius = 60.

let pipeWidth = 75.

let floorHeight = 100

let floorHeightf = float_of_int(floorHeight)

let birdX = 150.

let minInterPipeGap = 120. +. pipeWidth

let intersectPipe = (birdPos, (centerX, gapCenterY), env): bool => {
  let pipeRad = pipeWidth /. 2.
  let height = float_of_int(Env.height(env))
  let tophit = Utils.intersectRectCircle(
    ~rectPos=(centerX -. pipeRad, 0.),
    ~rectW=pipeWidth,
    ~rectH=gapCenterY -. gapRadius,
    ~circlePos=birdPos,
    ~circleRad=birdRad -. 2.,
  )
  let bottomhit = Utils.intersectRectCircle(
    ~rectPos=(centerX -. pipeRad, gapCenterY +. gapRadius),
    ~rectW=pipeWidth,
    ~rectH=height -. gapCenterY -. gapRadius,
    ~circlePos=birdPos,
    ~circleRad=birdRad -. 2.,
  )
  tophit || bottomhit
}

let findNewPipePos = (pipes, screenHeight) => {
  let maxPipeX = List.fold_left((acc, (x, _y)) => max(acc, x), 0., pipes)
  (
    maxPipeX +. Utils.randomf(~min=minInterPipeGap, ~max=1.5 *. minInterPipeGap),
    Utils.randomf(~min=gapRadius *. 2., ~max=screenHeight -. floorHeightf -. gapRadius *. 2.),
  )
}

let rec generateSomePipes = (num, screenHeight) =>
  switch num {
  | 0 => list{}
  | 1 => list{(500., 200.)}
  | n =>
    let pipes = generateSomePipes(n - 1, screenHeight)
    list{findNewPipePos(pipes, screenHeight), ...pipes}
  }

let drawPipe = ((centerX, gapCenterY): pipeT, img, env) => {
  let pipeX = int_of_float(centerX -. pipeWidth /. 2.)
  let pipeWidth = int_of_float(pipeWidth)
  let pipeHeight = 400
  Draw.subImage(
    img,
    ~pos=(pipeX, int_of_float(gapCenterY -. gapRadius -. float_of_int(pipeHeight))),
    ~width=pipeWidth,
    ~height=pipeHeight,
    ~texPos=(56, 323),
    ~texWidth=26,
    ~texHeight=160,
    env,
  )
  Draw.subImage(
    img,
    ~pos=(pipeX, int_of_float(gapCenterY +. gapRadius)),
    ~width=pipeWidth,
    ~height=pipeHeight,
    ~texPos=(84, 323),
    ~texWidth=26,
    ~texHeight=160,
    env,
  )
}

let resetState = (state, env) => {
  ...state,
  pos: (birdX, 40.),
  vy: 0.,
  running: true,
  pipes: generateSomePipes(5, float_of_int(Env.height(env))),
  angle: 0.,
  worldPos: 0.,
  score: 0,
}

let setup = (env): stateT => {
  Env.resizeable(false, env)
  Env.size(~width=400, ~height=640, env)
  Draw.noStroke(env)
  resetState(
    {
      pos: (birdX, 40.),
      vy: 0.,
      running: true,
      pipes: list{},
      score: 0,
      angle: 0.,
      worldPos: 0.,
      font: Draw.loadFont(~filename="./assets/flappy.fnt", ~isPixel=true, env),
      img: Draw.loadImage(~filename="./assets/flappy.png", ~isPixel=true, env),
    },
    env,
  )
}

let show_state = ({pos: (x, y)}: stateT): string => Printf.sprintf("{x: %f, y: %f}", x, y)

let crashed = ({running, pos: (_, y)}, env) =>
  !running && float_of_int(Env.height(env)) -. birdRad -. floorHeightf -. y < 1.

let drawBird = ({pos: (x, y), img} as state, env) => {
  Draw.pushMatrix(env)
  let drawBirdRad = birdRad +. 5.
  Draw.translate(~x, ~y, env)
  Draw.rotate(state.angle, env)
  let negBirdRad = -.drawBirdRad
  Draw.translate(~x=negBirdRad, ~y=negBirdRad, env)
  switch mod(int_of_float(state.worldPos) / 20, 3) {
  | 0 =>
    Draw.subImage(
      img,
      ~pos=(0, 0),
      ~width=int_of_float(drawBirdRad) * 2,
      ~height=int_of_float(drawBirdRad) * 2,
      ~texPos=(3, 488),
      ~texWidth=17,
      ~texHeight=17,
      env,
    )
  | 1 =>
    Draw.subImage(
      img,
      ~pos=(0, 0),
      ~width=int_of_float(drawBirdRad) * 2,
      ~height=int_of_float(drawBirdRad) * 2,
      ~texPos=(31, 488),
      ~texWidth=17,
      ~texHeight=17,
      env,
    )
  | 2 =>
    Draw.subImage(
      img,
      ~pos=(0, 0),
      ~width=int_of_float(drawBirdRad) * 2,
      ~height=int_of_float(drawBirdRad) * 2,
      ~texPos=(59, 488),
      ~texWidth=17,
      ~texHeight=17,
      env,
    )
  | _ => assert(false)
  }
  Draw.popMatrix(env)
}

let drawTiled = (screenY, screenHeight, x, y, w, h, img, worldPosf, env) => {
  let sw = Env.width(env)
  let worldPos = int_of_float(worldPosf)
  Draw.subImage(
    img,
    ~pos=(-mod(worldPos, sw), screenY),
    ~width=sw,
    ~height=screenHeight,
    ~texPos=(x, y),
    ~texWidth=w,
    ~texHeight=h,
    env,
  )
  Draw.subImage(
    img,
    ~pos=(-mod(worldPos, sw) + sw - 3, screenY),
    ~width=sw,
    ~height=screenHeight,
    ~texPos=(x, y),
    ~texWidth=w,
    ~texHeight=h,
    env,
  )
}

let draw = ({pos: (x, y) as pos, vy, running, pipes, font, img} as state, env) => {
  let timeStep = Env.deltaTime(env)
  let screenHeight = Env.height(env)
  Draw.clear(env)
  drawTiled(0, screenHeight, 0, 0, 144, 256, img, state.worldPos /. 2., env)
  List.iter((pipe: pipeT) => drawPipe(pipe, img, env), pipes)
  drawTiled(screenHeight - floorHeight, floorHeight, 292, 0, 169, 56, img, state.worldPos, env)
  drawBird(state, env)
  let halfPipeWidth = pipeWidth /. 2.
  let screenHeight = float_of_int(Env.height(env))
  Draw.pushMatrix(env)
  Draw.translate(~x=float_of_int(Env.width(env) / 2 - 20), ~y=50., env)
  Draw.scale(~x=3., ~y=3., env)
  Draw.text(~font, ~body=string_of_int(state.score), ~pos=(0, 0), env)
  Draw.popMatrix(env)
  if !running {
    Draw.subImage(
      img,
      ~pos=((Env.width(env) - 98 * 3) / 2, Env.height(env) / 2 - 20),
      ~width=98 * 3,
      ~height=23 * 3,
      ~texPos=(393, 59),
      ~texWidth=98,
      ~texHeight=23,
      env,
    )
  }
  let floorLocation = screenHeight -. birdRad -. floorHeightf
  let newY = Utils.constrain(~amt=y +. vy *. g *. timeStep, ~low=birdRad, ~high=floorLocation)
  let hit = List.exists(pipe => intersectPipe(pos, pipe, env), pipes) || newY >= floorLocation
  let scrollAmount = scrollRate *. timeStep
  let isCrashed = crashed(state, env)
  let (newPipes, newScore) = isCrashed
    ? (pipes, state.score)
    : List.fold_left(((newPipes, score), (x, y)) => {
        let (newX, _) as newPos = if x < -.halfPipeWidth {
          findNewPipePos(pipes, screenHeight)
        } else {
          (x -. scrollAmount, y)
        }
        (list{newPos, ...newPipes}, score + (running && (newX < birdX && x >= birdX) ? 1 : 0))
      }, (list{}, state.score), pipes)
  {
    ...state,
    pos: (x, newY),
    vy: vy +. g *. timeStep,
    running: state.running && !hit,
    angle: {
      let highAngle = -1.2
      let targetAngle = Utils.remapf(
        ~value=vy,
        ~low1=jumpSpeed,
        ~high1=10.,
        ~low2=highAngle,
        ~high2=0.7,
      )
      let newVal = Utils.lerpf(~value=vy > 0. ? 0.1 : 0.7, ~low=state.angle, ~high=targetAngle)
      Utils.constrain(~amt=newVal, ~low=highAngle, ~high=1.0)
    },
    worldPos: isCrashed ? state.worldPos : state.worldPos +. scrollAmount,
    pipes: newPipes,
    score: newScore,
  }
}

let keyPressed = (state, env) => {
  open Events
  switch (state.running, crashed(state, env), Env.keyCode(env)) {
  | (true, false, Space) => {...state, vy: jumpSpeed}
  | (false, true, Space) => resetState(state, env)
  | _ => state
  }
}

run(~setup, ~draw, ~keyPressed, ())
