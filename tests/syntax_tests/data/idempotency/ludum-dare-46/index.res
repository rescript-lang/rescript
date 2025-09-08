open Common
open Reprocessing

let editor = ref(false)

let setup = (spriteData, env): Common.state => {
  let fontPath = "assets/font/ptsans_regular_2x.fnt"
  let spritesheetLocation = "assets/sprites/spritesheet.png"
  Env.size(~width=1000, ~height=790, env)
  {
    hooks: Hooks.empty,
    mouse: {
      down: false,
      up: false,
      pressed: false,
    },
    spriteData: Sprite.create(Draw.loadImage(~filename=spritesheetLocation, env), spriteData),
    soundData: Sound.load(env),
    font: Draw.loadFont(~filename=fontPath, env),
  }
}

let drawObj = (
  ~obj,
  ~pos as {Point.x: x, y},
  ~spriteData,
  ~height=tileSizef,
  ~width=tileSizef,
  env,
) => {
  let halfTileSize = tileSizef /. 2.
  let pos = Point.create(x +. halfTileSize, y +. halfTileSize)
  switch obj {
  | Player(_, facing, _) =>
    let assetName = switch facing {
    | Up => "guy_up"
    | Down => "guy_down"
    | Right => "guy_right"
    | Left => "guy_left"
    }
    Assets.drawSprite(spriteData, assetName, ~pos, ~width, ~height, env)
  | Boulder(_, health) =>
    switch health {
    | Hard =>
      Assets.drawSprite(
        spriteData,
        "normal_boulder",
        ~pos,
        ~width=tileSizef,
        ~height=tileSizef,
        env,
      )
    | Cracked =>
      Assets.drawSprite(
        spriteData,
        "cracked_boulder",
        ~pos,
        ~width=tileSizef,
        ~height=tileSizef,
        env,
      )
    }
  | Empty => ()
  }
}

let drawTile = (
  kind,
  {x, y}: Point.Float.t,
  ~time,
  ~noBackground=false,
  ~withObj=false,
  spriteData: Sprite.t,
  env,
) => {
  let halfTileSize = tileSizef /. 2.
  let pos = Point.create(x +. halfTileSize, y +. halfTileSize)
  switch kind {
  | Floor(kind, obj) =>
    let background = !noBackground || editor.contents
    switch kind {
    | Regular =>
      if background {
        Assets.drawSprite(spriteData, "floor", ~pos, ~width=tileSizef, ~height=tileSizef, env)
      }
    | FilledPit(_, Hard) =>
      if background {
        Assets.drawSprite(
          spriteData,
          "pit_with_boulder",
          ~pos,
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        )
      }
    | FilledPit(_, Cracked) =>
      if background {
        Assets.drawSprite(
          spriteData,
          "pit_with_cracked_boulder",
          ~pos,
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        )
      }
    | Spinner(dir) =>
      if background {
        Assets.drawSprite(spriteData, "floor", ~pos, ~width=tileSizef, ~height=tileSizef, env)
      }
      Assets.drawSpriteWithCenterRotation(
        spriteData,
        "rotator",
        ~pos,
        ~width=tileSizef,
        ~height=tileSizef,
        ~rot=dir == CW ? time : -.time,
        env,
      )
    }
    if withObj {
      drawObj(~obj, ~pos={x: x, y: y}, ~spriteData, env)
    }
  | Pit => Assets.drawSprite(spriteData, "pit", ~pos, ~width=tileSizef, ~height=tileSizef, env)
  | Wall => Assets.drawSprite(spriteData, "wall", ~pos, ~width=tileSizef, ~height=tileSizef, env)
  }
}

let getMapTile = (map, {x, y}: Point.Int.t) =>
  if y < 0 || x < 0 {
    Wall
  } else {
    switch List.nth_opt(map, y) {
    | None => Wall
    | Some(row) =>
      switch List.nth_opt(row, x) {
      | None => Wall
      | Some(tile) => tile
      }
    }
  }

let setMapTile = (map, {x, y}: Point.Int.t, newTile) =>
  List.mapi((y2, row) => List.mapi((x2, tile) => x == x2 && y == y2 ? newTile : tile, row), map)

let updateMapTile = (map, {x, y}: Point.Int.t, update) =>
  List.mapi(
    (y2, row) => List.mapi((x2, tile) => x == x2 && y == y2 ? update(tile) : tile, row),
    map,
  )

let getInventoryTopLeft = env => {
  let height = float_of_int(Env.height(env))
  let backgroundY = height -. toolbarHeight
  let xOffset = 340.
  let yOffset = backgroundY +. btnMargin
  Point.create(xOffset, yOffset)
}

let getMapTopLeft = (map, env) => {
  Point.x: Env.width(env)->float_of_int /. 2. -.
    List.length(List.hd(map))->float_of_int *. tileSizef /. 2.,
  y: (Env.height(env)->float_of_int -. toolbarHeight) /. 2. -.
    List.length(map)->float_of_int *. tileSizef /. 2.,
}

let getHoveredMapSquare = (map, env) => {
  let mousePt = Point.Float.ofIntPt(Point.fromPair(Env.mouse(env)))
  let topLeft = getMapTopLeft(map, env)
  let mapWidth = tileSizef *. float_of_int(List.length(List.hd(map)))
  let mapHeight = tileSizef *. float_of_int(List.length(map))
  let mapRect = Rect.fromPoints(topLeft, Point.create(mapWidth, mapHeight))

  if Rect.containsPtf(mapRect, mousePt) {
    let relativePos = {
      open Point.Float
      mousePt - topLeft
    }
    let {Point.x: x, y} as tilePos = Point.Int.ofFloatPt({
      open Point.Float
      \"/@"(relativePos, tileSizef)
    })
    switch getMapTile(map, tilePos) {
    | Floor(Regular, Empty) => Some(Point.create(x, y))
    | _ if editor.contents => Some(Point.create(x, y))
    | _ => None
    }
  } else {
    None
  }
}

let getHoveredInventoryIndex = (items, env) => {
  let mousePt = Point.Float.ofIntPt(Point.fromPair(Env.mouse(env)))
  let inventoryTopLeft = getInventoryTopLeft(env)

  let inventoryWidth = (btnMargin +. tileSizef) *. float_of_int(Common.toolbarItemRowLen)
  let inventoryHeight = (btnMargin +. tileSizef) *. 2.
  let inventoryRect = Rect.fromPoints(
    inventoryTopLeft,
    Point.create(inventoryWidth, inventoryHeight),
  )

  if Rect.containsPtf(inventoryRect, mousePt) {
    let relativePos = {
      open Point.Float
      mousePt - inventoryTopLeft
    }
    let tileAndMargin = tileSizef +. btnMargin
    let hoverOffset = Point.map(~f=v => mod_float(v, tileAndMargin), relativePos)
    let {x, y}: Point.Int.t = Point.Int.ofFloatPt({
      open Point.Float
      \"/@"(relativePos, tileAndMargin)
    })
    let index = x + y * toolbarItemRowLen
    if index < List.length(items) {
      Some((index, hoverOffset))
    } else {
      None
    }
  } else {
    None
  }
}

let drawInventory = (inventory, spriteData, ~dragging, ~hovered, ~time, env) => {
  let topleft = getInventoryTopLeft(env)
  List.iteri((i, item) => {
    let x = float_of_int(mod(i, toolbarItemRowLen)) *. (tileSizef +. btnMargin)
    let y = (tileSizef +. btnMargin) *. float_of_int(i / toolbarItemRowLen)
    let relativePos = Point.create(x, y)
    Draw.pushStyle(env)
    if Some(i) == dragging {
      Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=100), env)
    } else if Some(i) == hovered {
      let highlight = 300
      Draw.tint(Utils.color(~r=highlight, ~g=highlight, ~b=highlight, ~a=255), env)
    }
    drawTile(
      item,
      ~noBackground=true,
      ~withObj=true,
      ~time,
      Point.Float.add(topleft, relativePos),
      spriteData,
      env,
    )
    Draw.popStyle(env)
  }, inventory)
}

let getUndoRect = env => {
  let height = float_of_int(Env.height(env))
  let backgroundY = height -. toolbarHeight
  let size = tileSizef *. 1.4
  let delta = btnMargin /. 2.
  Rect.fromPoints(
    Point.create(btnMargin +. size +. btnMargin +. delta, btnMargin +. backgroundY -. delta),
    Point.create(size, size),
  )
}

let getBackRect = env => {
  let height = float_of_int(Env.height(env))
  let backgroundY = height -. toolbarHeight
  let size = tileSizef *. 1.4
  let delta = btnMargin /. 2.
  Rect.fromPoints(
    Point.create(btnMargin +. delta, btnMargin +. backgroundY -. delta),
    Point.create(size, size),
  )
}

let getPlayRect = env => {
  let height = float_of_int(Env.height(env))
  let backgroundY = height -. toolbarHeight
  let size = tileSizef *. 1.4
  let delta = btnMargin /. 2.
  Rect.fromPoints(
    Point.create(
      btnMargin +. size +. btnMargin +. size +. btnMargin +. delta,
      btnMargin +. backgroundY -. delta,
    ),
    Point.create(size, size),
  )
}

type allButtonStates = {
  undo: (bool, bool),
  restart: (bool, bool),
  play: (bool, bool),
}

let drawToolbar = (
  inventory,
  ~accelerateTime=false,
  ~canAccelerate=false,
  ~inPreparingLevel=false,
  ~allButtonStates,
  spriteData,
  ~hovered,
  ~dragging,
  ~time,
  env,
) => {
  let width = float_of_int(Env.width(env))
  let height = float_of_int(Env.height(env))
  let x = 0.0
  let backgroundY = height -. toolbarHeight
  // Draw.fill(Utils.color(~r=32, ~g=60, ~b=86, ~a=255), env);
  // Draw.rectf(~pos=(x, backgroundY), ~width, ~height, env);

  let pressed = ((down, hovered)) => down ? "_pressed" : hovered ? "_hovered" : ""
  let rect = getUndoRect(env)
  Assets.drawSprite(
    spriteData,
    "undo" ++ pressed(allButtonStates.undo),
    ~pos=Point.create(rect.left +. rect.width /. 2., rect.top +. rect.height /. 2.),
    ~width=rect.width,
    ~height=rect.height,
    env,
  )

  let rect = getBackRect(env)
  Assets.drawSprite(
    spriteData,
    "back" ++ pressed(allButtonStates.restart),
    ~pos=Point.create(rect.left +. rect.width /. 2., rect.top +. rect.height /. 2.),
    ~width=rect.width,
    ~height=rect.height,
    env,
  )

  let rect = getPlayRect(env)
  let playOrAcceleate = if accelerateTime || !canAccelerate {
    "play"
  } else {
    "accelerate"
  }
  Assets.drawSprite(
    spriteData,
    playOrAcceleate ++ pressed(allButtonStates.play),
    ~pos=Point.create(rect.left +. rect.width /. 2., rect.top +. rect.height /. 2.),
    ~width=rect.width,
    ~height=rect.height,
    env,
  )

  Draw.pushStyle(env)
  Draw.fill(Utils.color(~r=84, ~g=78, ~b=104, ~a=255), env)
  Draw.stroke(Utils.color(~r=84, ~g=78, ~b=104, ~a=255), env)
  Draw.strokeWeight(12, env)
  // Draw.strokeCap(Round, env);
  let x = rect.left +. rect.width +. btnMargin *. 2.
  Draw.rectf(~pos=(x, backgroundY), ~width=width -. x -. 24., ~height=84., env)

  Draw.popStyle(env)

  drawInventory(inventory, spriteData, ~dragging, ~hovered, ~time, env)
}

let facingToDelta = facing =>
  switch facing {
  | Up => Point.create(0, -1)
  | Right => Point.create(1, 0)
  | Down => Point.create(0, 1)
  | Left => Point.create(-1, 0)
  }

let turnFacing = (facing, move) =>
  switch move {
  | Forward => facing
  | TurnLeft =>
    switch facing {
    | Up => Left
    | Right => Up
    | Down => Right
    | Left => Down
    }
  | TurnRight =>
    switch facing {
    | Up => Right
    | Right => Down
    | Down => Left
    | Left => Up
    }
  }

let rec resolveMove = (level, pos, moveDelta, retrying, state, env) => {
  let secondPos = Point.Int.add(pos, moveDelta)
  let replaceWith = (level, t1, t2) => Move(setMapTile(setMapTile(level, pos, t1), secondPos, t2))

  let retryResolveMove = level =>
    !retrying ? resolveMove(level, pos, moveDelta, true, state, env) : Move(level)

  let resolveMove = (level, pos, moveDelta) =>
    !retrying ? resolveMove(level, pos, moveDelta, false, state, env) : Move(level)

  switch (getMapTile(level, pos), getMapTile(level, secondPos)) {
  | (Wall | Pit | Floor(_, Empty), _) => Move(level)
  | (Floor(k1, Boulder(id, health)), Wall) => Move(level)
  | (_, Floor(k, Player(_) as p)) =>
    Lose(
      setMapTile(level, secondPos, Floor(k, Empty)),
      list{{obj: p, position: secondPos, prevPosition: pos}},
    )
  | (Floor(k1, Boulder(id, health)), Floor(k2, Empty)) =>
    Sound.play("moving_boulder", state, env)
    replaceWith(level, Floor(k1, Empty), Floor(k2, Boulder(id, health)))
  | (Floor(k1, Boulder(id, health)), Pit) =>
    Sound.play("moving_boulder", state, env)
    replaceWith(level, Floor(k1, Empty), Floor(FilledPit(id, health), Empty))
  | (Floor(k, Player(id, facing, moves)), Floor(Spinner(dir), Empty)) =>
    let move = dir == CW ? TurnRight : TurnLeft
    replaceWith(
      level,
      Floor(k, Empty),
      Floor(Spinner(dir), Player(id, facing, list{move, ...moves})),
    )
  | (Floor(Spinner(dir), Player(id, facing, moves)), Floor(k, Boulder(id2, boulderState)))
    if retrying =>
    let move = dir == CW ? TurnRight : TurnLeft
    let obj = boulderState == Hard ? Boulder(id2, Cracked) : Empty
    Sound.play("rock_crack", state, env)
    replaceWith(level, Floor(Spinner(dir), Player(id, facing, list{move, ...moves})), Floor(k, obj))
  | (Floor(k1, Player(_) as p), Floor(k2, Empty)) =>
    replaceWith(level, Floor(k1, Empty), Floor(k2, p))
  | (Floor(k, Player(_) as p), Wall)
  | (Floor(k, Player(_) as p), Pit) =>
    Lose(
      setMapTile(level, pos, Floor(k, Empty)),
      list{{obj: p, position: secondPos, prevPosition: pos}},
    )
  | (Floor(k1, Player(_) as p), Floor(k2, Boulder(id, boulderState))) if retrying =>
    let obj = boulderState == Hard ? Boulder(id, Cracked) : Empty
    Sound.play("rock_crack", state, env)
    replaceWith(level, Floor(k1, p), Floor(k2, obj))
  | (Floor(k1, Player(_) | Boulder(_, _)), Floor(k2, Boulder(_))) =>
    switch resolveMove(level, secondPos, moveDelta) {
    | Move(level) => retryResolveMove(level)
    | other => other
    }
  }
}

type agentInfo =
  | AgentWin
  | Push(facing, Point.Int.t)

// TODO: tick needs to return a win or lose state that's compatible with animation
let tick = (level, state, env) => {
  let fold_righti = (f, a, l) =>
    snd(List.fold_right((el, (i, a)) => (i - 1, f(i, a, el)), l, (List.length(l) - 1, a)))

  let (movingAgents, level) = fold_righti((y, (agents, rows), row) => {
    let (agents, newRow) = fold_righti((x, (agents, row), tile) =>
      switch tile {
      | Floor(k, Player(id, facing, list{})) => (
          list{AgentWin, ...agents},
          list{Floor(k, Player(id, facing, list{})), ...row},
        )
      | Floor(k, Player(id, facing, list{Forward, ...moves})) => (
          list{Push(facing, Point.create(x, y)), ...agents},
          list{Floor(k, Player(id, facing, moves)), ...row},
        )
      | Floor(k, Player(id, facing, list{turn, ...moves})) => (
          agents,
          list{Floor(k, Player(id, turnFacing(facing, turn), moves)), ...row},
        )
      | _ => (agents, list{tile, ...row})
      }
    , (agents, list{}), row)
    (agents, list{newRow, ...rows})
  }, (list{}, list{}), level)

  // TODO: Optionally sort the agents
  List.fold_left((level, agent) =>
    switch (level, agent) {
    | (Move(level), Push(facing, pos)) =>
      resolveMove(level, pos, facingToDelta(facing), false, state, env)
    | (Lose(level, deadList), Push(facing, pos)) =>
      switch resolveMove(level, pos, facingToDelta(facing), false, state, env) {
      | Move(level) => Lose(level, deadList)
      | Win => Lose(level, deadList)
      | Lose(level, newDeadList) => Lose(level, \"@"(newDeadList, deadList))
      }
    | (_, AgentWin)
    | (Win, _) =>
      Win
    }
  , Move(level), movingAgents)
}

let drawMessage = (
  message,
  font,
  ~offset=0,
  ~withControl=?,
  ~alternativeText=?,
  ~time=0.,
  ~spriteData=?,
  env,
) => {
  let y = (180 + offset - fontHeight) / 2
  let textWidth = Draw.textWidth(~font, ~body=message, env)
  let x = (Env.width(env) - textWidth) / 2

  let fullWidth = Env.width(env)

  Draw.fill(Utils.color(~r=13, ~g=43, ~b=69, ~a=200), env)
  Draw.rect(~pos=(0, 0), ~width=fullWidth, ~height=y + 20, env)

  if withControl != None {
    Draw.rect(~pos=(0, y), ~width=fullWidth, ~height=fontHeight + 60, env)
  }

  Draw.tint(Utils.color(~r=255, ~g=236, ~b=214, ~a=255), env)
  Draw.text(~font, ~body=message, ~pos=(x, y), env)

  Option.iter(t => {
    let textWidth = Draw.textWidth(~font, ~body=t, env)
    let x2 = (Env.width(env) - textWidth) / 2
    let y2 = y + fontHeight + 24
    Draw.text(~font, ~body=t, ~pos=(x2, y2), env)
  }, alternativeText)

  switch (withControl, spriteData) {
  | (Some(assetName), Some(spriteData)) =>
    let body = switch assetName {
    | "back" => "press         to try again"
    | _ => "press         to continue"
    }
    let textWidth2 = Draw.textWidth(~font, ~body, env)
    let rect = getPlayRect(env)
    let x2 = (Env.width(env) - textWidth2) / 2
    let y2 = y + fontHeight + 24
    Draw.tint(Utils.color(~r=255, ~g=236, ~b=214, ~a=255), env)
    Draw.text(~font, ~body, ~pos=(x2, y2), env)

    Assets.drawSprite(
      spriteData,
      assetName,
      ~pos=Point.create(x2->float_of_int +. 166., y2->float_of_int -. 12.),
      ~width=rect.width,
      ~height=rect.height,
      env,
    )
  | _ => ()
  }

  Draw.noTint(env)
}

let drawLines = (map, env) => {
  let mapTopLeft = getMapTopLeft(map, env)
  let halfTileSize = tileSizef /. 2.
  let centerOffset = Point.create(halfTileSize, halfTileSize)

  List.iteri((y, row) => List.iteri((x, tile) =>
      switch tile {
      | Floor(_, Player(id, facing, moves)) =>
        let (_, mapPositions) = List.fold_left(
          ((currFacing, acc), move) =>
            switch acc {
            | list{prevPoint, ...rest} =>
              switch move {
              | Forward => (
                  currFacing,
                  list{Point.Int.add(facingToDelta(currFacing), prevPoint), prevPoint, ...rest},
                )
              | turn => (turnFacing(currFacing, turn), list{prevPoint, ...rest})
              }
            | _ => assert(false)
            },
          (
            facing,
            list{
              {
                open Point
                create(x, y)
              },
            },
          ),
          moves,
        )

        let screenPositions = List.map(p => {
          open Point.Float
          \"*@"(ofIntPt(p), tileSizef) + centerOffset + mapTopLeft
        }, mapPositions)

        Draw.stroke(Utils.color(~r=255, ~g=236, ~b=214, ~a=255), env)
        let _ = List.fold_left((acc, pos) =>
          switch acc {
          | None => Some(pos)
          | Some(prevPos) =>
            Draw.linef(~p1=Point.toPair(prevPos), ~p2=Point.toPair(pos), env)
            Some(pos)
          }
        , None, screenPositions)
        Draw.noStroke(env)
        ()
      | _ => ()
      }
    , row), map)
}

let drawMap = (map, spriteData, ~time, env) => {
  let topleft = getMapTopLeft(map, env)
  List.iteri((y, row) => List.iteri((x, tile) => {
      let p = Point.Int.create(x, y)
      drawTile(
        tile,
        ~time,
        {
          open Point.Float
          topleft + \"*@"(ofIntPt(p), tileSizef)
        },
        spriteData,
        env,
      )
    }, row), map)
}

let findInMap = (level, withId) => {
  let cur = ref(None)
  List.iteri((y, row) => List.iteri((x, tile) =>
      switch tile {
      | Floor(FilledPit(id, _), _) if id == withId => cur := Some((Point.create(x, y), None))
      | Floor(_, Boulder(id, _) as obj)
      | Floor(_, Player(id, _, _) as obj) if id === withId =>
        cur := Some((Point.create(x, y), Some(obj)))
      | _ => ()
      }
    , row), level)
  cur.contents
}

let easeInOutQuad = t => t < 0.5 ? 2. *. t *. t : -1. +. (4. -. 2. *. t) *. t

let easeInOutCubic = t =>
  t < 0.5 ? 4. *. t *. t *. t : (t -. 1.) *. (2. *. t -. 2.) *. (2. *. t -. 2.) +. 1.

let calculateBounce = (~elapsedTime, ~tickTimeMS, pos: Point.Float.t, prevPos: Point.Float.t) => {
  let numBounces = 2.
  // TODO: There's something we could do here to ease more proportionally.
  let time = easeInOutQuad(elapsedTime /. tickTimeMS)

  let bounce = Utils.remapf(
    ~value=time,
    ~low1=0.,
    ~high1=1.0,
    ~low2=0.,
    ~high2=numBounces *. Constants.pi,
  )

  // If we're moving in the x position, we need to calculate gravity from a Y perspective
  let bounceY = if prevPos.x != pos.x {
    sin(bounce) *. 6.
  } else if prevPos.y != pos.y {
    sin(bounce) *. 6.
  } else {
    0.
  }

  abs_float(bounceY) *. -1.0
}

let drawObjWithAnimation = (
  ~time,
  ~tickTimeMS,
  ~obj,
  ~scaleX=1.,
  ~scaleY=1.,
  pos: Point.Float.t,
  prevPos: Point.Float.t,
  state,
  env,
) => {
  let elapsedTime = time
  let time = easeInOutQuad(time /. tickTimeMS)

  let animatingPosX = Utils.remapf(~value=time, ~low1=0., ~high1=1.0, ~low2=prevPos.x, ~high2=pos.x)
  let animatingPosY = Utils.remapf(~value=time, ~low1=0., ~high1=1.0, ~low2=prevPos.y, ~high2=pos.y)

  let animatedPosition = Point.Float.create(animatingPosX, animatingPosY)
  let bounceY = switch obj {
  | Player(_, _, _) => calculateBounce(~elapsedTime, ~tickTimeMS, pos, prevPos)
  | _ => 0.
  }

  let squishY = switch obj {
  | Player(_, _, _) => tileSizef -. bounceY *. -0.75
  | _ => tileSizef
  }
  let squishX = switch obj {
  | Player(_, _, _) => tileSizef +. bounceY *. -0.75
  | _ => tileSizef
  }

  let bouncedPosition = Point.Float.create(0., bounceY)

  drawObj(
    ~obj,
    ~pos={
      open Point.Float
      animatedPosition + bouncedPosition
    },
    ~spriteData=state.spriteData,
    ~height=squishY *. scaleY,
    ~width=squishX *. scaleX,
    env,
  )
}

let drawObjects = (~previousLevel=?, ~time=0., ~tickTimeMS, level, state, env) => {
  let topleft = getMapTopLeft(level, env)
  let drawHelper = (x, y, obj) => {
    let p = Point.Int.create(x, y)
    let pos = {
      open Point.Float
      topleft + \"*@"(ofIntPt(p), tileSizef)
    }
    drawObj(~obj, ~pos, ~spriteData=state.spriteData, env)
  }

  switch previousLevel {
  | None => List.iteri((y, row) => List.iteri((x, tile) =>
        switch tile {
        | Floor(_, Boulder(id, _) as obj)
        | Floor(_, Player(id, _, _) as obj) =>
          drawHelper(x, y, obj)
        | _ => ()
        }
      , row), level)
  | Some({map: previousLevel}) => List.iteri((y, row) => List.iteri((x, tile) =>
        switch tile {
        | Floor(_, Boulder(id, _) as obj)
        | Floor(_, Player(id, _, _) as obj) =>
          switch findInMap(level, id) {
          | None => drawHelper(x, y, obj)
          | Some((p, newObj)) =>
            let obj = switch newObj {
            | Some(o) => o
            | None => obj
            }
            let pos = {
              open Point.Float
              topleft + \"*@"(ofIntPt(p), tileSizef)
            }
            let prevP = Point.Int.create(x, y)
            let prevPos = {
              open Point.Float
              topleft + \"*@"(ofIntPt(prevP), tileSizef)
            }
            drawObjWithAnimation(~time, ~tickTimeMS, ~obj, pos, prevPos, state, env)
            ()
          }

        | _ => ()
        }
      , row), previousLevel)
  }
}

let drawAnimatingDead = (~deadList, ~lastTickTime, ~level, ~state, env) => {
  let topleft = getMapTopLeft(level.map, env)

  List.iter(deadThing =>
    switch deadThing.obj {
    | Player(id, _, _) as obj =>
      let p = deadThing.position
      let pos = {
        open Point.Float
        topleft + \"*@"(ofIntPt(p), tileSizef)
      }
      let prevP = deadThing.prevPosition
      let prevPos = {
        open Point.Float
        topleft + \"*@"(ofIntPt(prevP), tileSizef)
      }
      let time = easeInOutCubic(lastTickTime /. tickTimeMS)
      let (scaleX, scaleY, pos) = switch List.nth_opt(List.nth(level.map, p.y), p.x) {
      | Some(Pit) =>
        let v = Utils.lerpf(~low=1.0, ~high=0.6, ~value=time)
        (v, v, pos)
      | Some(Wall) =>
        if p.y == prevP.y {
          let dir = p.x > prevP.x ? -1. : 1.
          let pos = Point.create(
            pos.x +.
            dir *.
            (Utils.lerpf(~low=0.0, ~high=tileSizef /. 8. +. 4., ~value=time) +.
            tileSizef /. 4. +. 8.),
            pos.y,
          )
          (Utils.lerpf(~low=1.0, ~high=0.3, ~value=time), 1.0, pos)
        } else {
          let dir = p.y > prevP.y ? -1. : 1.
          let pos = Point.create(
            pos.x,
            pos.y +.
            dir *.
            (Utils.lerpf(~low=0.0, ~high=tileSizef /. 8. +. 4., ~value=time) +.
            tileSizef /. 4. +. 8.),
          )
          (1.0, Utils.lerpf(~low=1.0, ~high=0.3, ~value=time), pos)
        }
      | _ => (1.0, 1.0, pos)
      }
      drawObjWithAnimation(
        ~time=lastTickTime,
        ~tickTimeMS,
        ~obj,
        ~scaleX,
        ~scaleY,
        pos,
        prevPos,
        state,
        env,
      )
    | _ => ()
    }
  , deadList)
}

let getClickOn = (rect, mousePtf, (down, setDown), env) => {
  let hovered = rect->Rect.containsPtf(mousePtf)
  let clicked = if Env.mousePressed(env) {
    setDown(hovered)
    false
  } else if down.contents {
    setDown(false)
    hovered
  } else {
    false
  }
  (clicked, (down.contents, hovered))
}

let draw = (state, env) => {
  Hooks.initialize(state.hooks)
  let (levels, setLevels) = Hooks.useState(__LOC__, Levels.all)
  let (gameState, setGameState) = Hooks.useState(__LOC__, Intro)

  // This value always starts at MAX so we tick once immediately
  let (lastTickTime, setLastTickTime) = Hooks.useState(__LOC__, tickTimeMS +. 1.)

  let mousePtf = {
    open Point
    Float.ofIntPt(fromPair(Env.mouse(env)))
  }

  let undo = if Env.mousePressed(env) {
    let rect = getUndoRect(env)
    rect->Rect.containsPtf({
      open Point
      Float.ofIntPt(fromPair(Env.mouse(env)))
    })
  } else {
    false
  }

  let backButtonState = Hooks.useState(__LOC__, false)
  let backButtonRect = getBackRect(env)
  let (restartClicked, restartButtonDown) = getClickOn(
    backButtonRect,
    mousePtf,
    backButtonState,
    env,
  )
  let restartClicked = restartClicked || Env.keyPressed(R, env)

  let playButtonState = Hooks.useState(__LOC__, false)
  let playButtonRect = getPlayRect(env)
  let (playClicked, playButtonDown) = getClickOn(playButtonRect, mousePtf, playButtonState, env)
  let playClicked = playClicked || Env.keyPressed(Space, env)
  let (lastAccelerating, setLastAccelerating) = Hooks.useState(__LOC__, false)

  let undoButtonState = Hooks.useState(__LOC__, false)
  let undoButtonRect = getUndoRect(env)
  let (undoClicked, undoButtonDown) = getClickOn(undoButtonRect, mousePtf, undoButtonState, env)
  let undoClicked = undoClicked || (Env.keyPressed(Z, env) || Env.keyPressed(U, env))

  // Start button code
  let startButtonState = Hooks.useState(__LOC__, false)
  let width = 642. /. 2.
  let height = 369. /. 2.
  let startButtonRect = Rect.fromPoints(
    Point.create(
      Env.width(env)->float_of_int /. 2. -. width /. 2.,
      Env.height(env)->float_of_int /. 2. -. height /. 2.,
    ),
    Point.create(width, height),
  )
  let (startButtonClicked, (startButtonDown, startButtonHovered)) = getClickOn(
    startButtonRect,
    mousePtf,
    startButtonState,
    env,
  )

  let (lossCounter, setLossCounter) = Hooks.useState(__LOC__, 0)

  let allButtonStates = {
    restart: restartButtonDown,
    play: playButtonDown,
    undo: undoButtonDown,
  }

  let (totalTime, setTotalTime) = Hooks.useState(__LOC__, 0.)
  setTotalTime(mod_float(totalTime.contents +. Env.deltaTime(env), 10000000.))

  if restartClicked {
    setLastTickTime(tickTimeMS +. 1.)
  }

  if editor.contents && Env.keyPressed(T, env) {
    setLevels(Levels.all)
    setGameState(Intro)
    setLastTickTime(tickTimeMS +. 1.)
  }

  /* if (Env.keyPressed(E, env)) { */
  /* editor := ! editor^; */
  /* }; */

  if editor.contents {
    Draw.background(Constants.red, env)
  } else {
    Draw.background(Utils.color(~r=13, ~g=43, ~b=69, ~a=255), env)
  }

  switch (levels.contents, gameState.contents) {
  | (list{}, _) => drawMessage("You WON the whole game", state.font, env)
  | (list{first, ...rest}, Intro) =>
    drawMessage("Death Trap II", ~alternativeText="Revenge of the Walls", state.font, env)

    Assets.drawSprite(
      state.spriteData,
      "start" ++ (startButtonDown ? "_pressed" : startButtonHovered ? "_hovered" : ""),
      ~pos=Point.create(
        startButtonRect.left +. startButtonRect.width /. 2.,
        startButtonRect.top +. startButtonRect.height /. 2.,
      ),
      ~width=startButtonRect.width,
      ~height=startButtonRect.height,
      env,
    )

    if startButtonClicked || playClicked {
      let (playingMusic, setPlayingMusic) = Hooks.useState(__LOC__, false)
      if !playingMusic.contents {
        setPlayingMusic(true)
        Sound.play("background_tunes", state, ~loop=true, env)
      }
      setGameState(PreparingLevel(list{first}))
    }
  | (
      list{levelInitialState, ...restOfLevels},
      PreparingLevel(list{levelCurrentState, ...undoStates}),
    ) =>
    let (dragging, setDragging) = Hooks.useState(__LOC__, None)

    let hoveredItem = getHoveredInventoryIndex(levelCurrentState.items, env)
    let hoveredMapSquare = getHoveredMapSquare(levelCurrentState.map, env)
    let hoveredTile = Option.map(p => (getMapTile(levelCurrentState.map, p), p), hoveredMapSquare)

    if editor.contents && Env.keyPressed(P, env) {
      Serialize.map(levelCurrentState.map)
    }

    let levelCurrentState = if editor.contents {
      let removeLast = l =>
        switch List.rev(l) {
        | list{_, ...tl} => List.rev(tl)
        | list{} => list{}
        }

      let append = (e, l) => List.rev(list{e, ...List.rev(l)})

      switch hoveredTile {
      | Some((Floor(k, Player(id, facing, moves)), pt)) =>
        if Env.keyPressed(Backspace, env) || Env.keyPressed(Down, env) {
          {
            ...levelCurrentState,
            map: setMapTile(
              levelCurrentState.map,
              pt,
              Floor(k, Player(id, facing, removeLast(moves))),
            ),
          }
        } else if Env.keyPressed(Right, env) {
          {
            ...levelCurrentState,
            map: setMapTile(
              levelCurrentState.map,
              pt,
              Floor(k, Player(id, facing, append(TurnRight, moves))),
            ),
          }
        } else if Env.keyPressed(Left, env) {
          {
            ...levelCurrentState,
            map: setMapTile(
              levelCurrentState.map,
              pt,
              Floor(k, Player(id, facing, append(TurnLeft, moves))),
            ),
          }
        } else if Env.keyPressed(Up, env) {
          {
            ...levelCurrentState,
            map: setMapTile(
              levelCurrentState.map,
              pt,
              Floor(k, Player(id, facing, append(Forward, moves))),
            ),
          }
        } else {
          levelCurrentState
        }

      | Some(_)
      | None => levelCurrentState
      }
    } else {
      levelCurrentState
    }

    let maybeUpdatedLevel = switch (hoveredItem, hoveredMapSquare, dragging.contents) {
    | (None, _, None) => levelCurrentState
    | (Some(v), _, None) if state.mouse.down =>
      setDragging(Some(v))
      Sound.play("pickup", state, env)
      levelCurrentState
    | (Some(_), _, None) => levelCurrentState
    | (_, _, Some(i)) if Env.mousePressed(env) => levelCurrentState
    | (_, Some(mapSquare), Some((draggedI, _))) =>
      setDragging(None)
      let newTile = switch List.nth(levelCurrentState.items, draggedI) {
      | Floor(k, Boulder(_, bk)) => Floor(k, Boulder(Levels.id(), bk))
      | Floor(k, Player(_, f, m)) => Floor(k, Player(Levels.id(), f, m))
      | t => t
      }

      Sound.play("drop", state, env)
      {
        ...levelCurrentState,
        items: List.filteri((i, _) => editor.contents || i != draggedI, levelCurrentState.items),
        map: setMapTile(levelCurrentState.map, mapSquare, newTile),
      }
    | (_, _, Some(i)) =>
      setDragging(None)
      levelCurrentState
    }

    let (levelCurrentState, undoStates) = if maybeUpdatedLevel != levelCurrentState {
      (maybeUpdatedLevel, list{levelCurrentState, ...undoStates})
    } else if undoClicked {
      switch undoStates {
      | list{} => (levelCurrentState, list{})
      | list{undoState, ...undoStates} => (undoState, undoStates)
      }
    } else {
      (levelCurrentState, undoStates)
    }

    setGameState(PreparingLevel(list{levelCurrentState, ...undoStates}))

    if restartClicked {
      setGameState(PreparingLevel(list{levelInitialState}))
    }
    if playClicked {
      setGameState(
        RunningLevel({
          states: list{levelCurrentState},
          preparingUndoStack: list{levelCurrentState, ...undoStates},
        }),
      )
    }
    drawMap(levelCurrentState.map, state.spriteData, ~time=totalTime.contents, env)
    drawLines(levelCurrentState.map, env)
    drawObjects(levelCurrentState.map, state, ~tickTimeMS, env)
    drawMessage(levelCurrentState.title, state.font, env)
    drawToolbar(
      levelCurrentState.items,
      ~inPreparingLevel=true,
      ~allButtonStates,
      state.spriteData,
      ~dragging=Option.map(fst, dragging.contents),
      ~hovered=Option.map(fst, hoveredItem),
      ~time=totalTime.contents,
      env,
    )
    Option.iter(((i, dragOffset)) => {
      switch hoveredTile {
      | Some((Floor(k, Empty), pt)) =>
        let pos: Point.Float.t = {
          open Point.Float
          getMapTopLeft(levelCurrentState.map, env) + \"*@"(ofIntPt(pt), tileSizef)
        }
        Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=100), env)
        drawTile(
          List.nth(levelCurrentState.items, i),
          pos,
          ~time=totalTime.contents,
          ~noBackground=true,
          ~withObj=true,
          state.spriteData,
          env,
        )
        Draw.noTint(env)
      | _ => ()
      }

      drawTile(
        List.nth(levelCurrentState.items, i),
        {
          open Point.Float
          mousePtf - dragOffset
        },
        ~time=totalTime.contents,
        ~noBackground=true,
        ~withObj=true,
        state.spriteData,
        env,
      )
    }, dragging.contents)
  | (list{levelInitialState, ...restOfLevels}, RunningLevel({states: list{}})) =>
    failwith("This should not happen, RunningLevel got an empty list.")
  | (
      list{levelInitialState, ...restOfLevels} as allLevels,
      RunningLevel({states: list{levelCurrentState, ...pastLevelStates}, preparingUndoStack}),
    ) =>
    let deltaTime = Env.deltaTime(env) *. 1000.0
    if playClicked {
      setLastAccelerating(!lastAccelerating.contents)
    }
    let accelerating = lastAccelerating.contents
    let tickTimeMS = accelerating ? tickTimeMS /. accelerateMult : tickTimeMS *. 1.

    let (pastLevelStates, levelCurrentState) = if lastTickTime.contents > tickTimeMS {
      switch tick(levelCurrentState.map, state, env) {
      | Move(level) =>
        setLastTickTime(0.0)
        let newLevelState = {...levelCurrentState, map: level}
        setGameState(
          RunningLevel({
            states: list{newLevelState, levelCurrentState, ...pastLevelStates},
            preparingUndoStack: preparingUndoStack,
          }),
        )
        (list{levelCurrentState, ...pastLevelStates}, newLevelState)
      | Win =>
        if editor.contents {
          setGameState(PreparingLevel(preparingUndoStack))
        } else {
          Sound.play("win", state, env)
          setLevels(restOfLevels)
          setGameState(WinLevel(levelInitialState, levelCurrentState))
        }
        setLastTickTime(tickTimeMS +. 1.)
        (pastLevelStates, levelCurrentState)
      | Lose(level, deadList) =>
        Sound.play("lose", state, env)
        setLossCounter(lossCounter.contents + 1)
        let newLevelState = {...levelCurrentState, map: level}
        setGameState(
          LoseLevel({
            deadList: deadList,
            loseState: newLevelState,
            preparingUndoStack: preparingUndoStack,
          }),
        )

        // Set to 0 to make sure the death animation can play for a tick
        setLastTickTime(0.)
        (list{levelCurrentState, ...pastLevelStates}, newLevelState)
      }
    } else {
      setLastTickTime(lastTickTime.contents +. deltaTime)
      (pastLevelStates, levelCurrentState)
    }
    let pastLevel = switch List.nth_opt(pastLevelStates, 0) {
    | None => levelCurrentState
    | Some(pastLevel) => pastLevel
    }
    drawMap(pastLevel.map, state.spriteData, ~time=totalTime.contents, env)
    if lastTickTime.contents /. tickTimeMS < 0.5 {
      drawLines(pastLevel.map, env)
    } else {
      drawLines(levelCurrentState.map, env)
    }
    drawObjects(
      ~previousLevel=pastLevel,
      ~tickTimeMS,
      ~time=lastTickTime.contents,
      levelCurrentState.map,
      state,
      env,
    )
    drawMessage(levelCurrentState.title, state.font, env)
    drawToolbar(
      list{},
      state.spriteData,
      ~dragging=None,
      ~hovered=None,
      ~accelerateTime=accelerating,
      ~canAccelerate=true,
      ~allButtonStates,
      ~inPreparingLevel=false,
      ~time=totalTime.contents,
      env,
    ) // TODO: Any items?
    if restartClicked {
      setGameState(PreparingLevel(preparingUndoStack))
      setLastTickTime(tickTimeMS +. 1.)
    }
  | (list{nextLevel, ...restLevels}, WinLevel(wonLevel, level)) =>
    drawMap(level.map, state.spriteData, ~time=totalTime.contents, env)
    drawLines(level.map, env)
    drawObjects(level.map, state, ~tickTimeMS, env)
    drawToolbar(
      list{},
      state.spriteData,
      ~dragging=None,
      ~hovered=None,
      ~allButtonStates,
      ~inPreparingLevel=false,
      ~time=totalTime.contents,
      env,
    )
    let deltaTime = Env.deltaTime(env) *. 1000.0
    if playClicked {
      setGameState(PreparingLevel(list{nextLevel}))
    } else if restartClicked {
      setLevels(list{wonLevel, nextLevel, ...restLevels})
      setGameState(PreparingLevel(list{wonLevel}))
    }
    drawMessage(
      "That's how it's done!",
      ~withControl="play",
      ~spriteData=state.spriteData,
      state.font,
      env,
    )
  | (
      list{initialLevel, ..._},
      LoseLevel({loseState, preparingUndoStack: prepLevelState, deadList}),
    ) =>
    drawMap(loseState.map, state.spriteData, ~time=totalTime.contents, env)
    drawLines(loseState.map, env)
    drawObjects(loseState.map, state, ~tickTimeMS, env)

    drawAnimatingDead(~deadList, ~level=loseState, ~lastTickTime=lastTickTime.contents, ~state, env)

    drawToolbar(
      list{},
      state.spriteData,
      ~dragging=None,
      ~hovered=None,
      ~allButtonStates,
      ~inPreparingLevel=false,
      ~time=totalTime.contents,
      env,
    )

    let deltaTime = Env.deltaTime(env) *. 1000.0
    if lastTickTime.contents < tickTimeMS {
      setLastTickTime(lastTickTime.contents +. deltaTime)
    }

    if playClicked || restartClicked {
      setGameState(PreparingLevel(prepLevelState))
    }

    let message = if (
      lossCounter.contents != 0 && mod(lossCounter.contents, lossCountRudeMessage) == 0
    ) {
      let index = mod(
        lossCounter.contents / lossCountRudeMessage + String.length(initialLevel.title),
        List.length(rudeLossMessages),
      )
      List.nth(rudeLossMessages, index)
    } else {
      "Oh no! Keep him alive next time, ok?"
    }
    drawMessage(message, state.font, ~withControl="back", ~spriteData=state.spriteData, env)
  }

  {
    ...state,
    hooks: Hooks.finalize(),
    mouse: {
      ...state.mouse,
      down: false,
      up: false,
    },
  }
}

let mouseDown = (state, _) => {
  ...state,
  mouse: {
    down: true,
    up: false,
    pressed: true,
  },
}
let mouseUp = (state, _) => {
  ...state,
  mouse: {
    down: false,
    up: true,
    pressed: false,
  },
}

Assets.loadSpriteSheet("assets/sprites/spritesheet.json", assets =>
  run(~setup=setup(assets), ~draw, ~mouseDown, ~mouseUp, ())
)
