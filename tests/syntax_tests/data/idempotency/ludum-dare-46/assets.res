open Common
open Reprocessing

let spriteSize = 120.

let maybeInt = f =>
  switch int_of_float(f) {
  | i => Some(i)
  | exception Failure(_) => None
  }

let loadSpriteSheet = (filename, cb) =>
  Reasongl.Gl.File.readFile(~filename, ~cb=jsonString => {
    open Json.Infix
    let json = Json.parse(jsonString)
    let things = \"|!"(
      \"|?>"(Json.get("frames", json), Json.array),
      "Expected field `frames` to be an array",
    )
    let assets = List.fold_left((assets, thing) => {
      let frame = \"|!"(Json.get("frame", thing), "Expected field `frame` in `frames` array")
      let x = \"|!"(
        \"|?>"(\"|?>"(Json.get("x", frame), Json.number), maybeInt),
        "Invalid field `x`",
      )
      let y = \"|!"(
        \"|?>"(\"|?>"(Json.get("y", frame), Json.number), maybeInt),
        "Invalid field `y`",
      )
      let w = \"|!"(
        \"|?>"(\"|?>"(Json.get("w", frame), Json.number), maybeInt),
        "Invalid field `w`",
      )
      let h = \"|!"(
        \"|?>"(\"|?>"(Json.get("h", frame), Json.number), maybeInt),
        "Invalid field `h`",
      )
      let name = \"|!"(\"|?>"(Json.get("filename", thing), Json.string), "Invalid field `filename`")
      let name = List.hd(Reprocessing.Utils.split(name, ~sep='.'))
      StringMap.add(
        name,
        {
          open Sprite
          {x: x, y: y, w: w, h: h}
        },
        assets,
      )
    }, StringMap.empty, things)
    cb(assets)
  })

let placeholder = (name, ~pos, ~width, ~height, env) => {
  Draw.fill(Constants.red, env)
  Draw.noStroke(env)
  Draw.rectf(
    ~pos={
      open Point.Float
      toPair(pos)
    },
    ~width,
    ~height,
    env,
  )
  Draw.text(~body=name, ~pos=(int_of_float(pos.x), int_of_float(pos.y)), env)
}

let drawSprite = (t, name, ~pos, ~width=?, ~height=?, env) =>
  switch StringMap.find(name, t.Sprite.map) {
  | {x, y, w, h} =>
    let width = switch width {
    | None => float_of_int(w)
    | Some(w) => w
    }
    let height = switch height {
    | None => float_of_int(h)
    | Some(h) => h
    }

    Draw.subImagef(
      t.sheet,
      ~pos={
        open Point.Float
        toPair(pos - create(width /. 2., height /. 2.))
      },
      ~width,
      ~height,
      ~texPos=(x, y),
      ~texWidth=w,
      ~texHeight=h,
      env,
    )
  | exception Not_found =>
    placeholder(
      name,
      ~pos={
        open Point.Float
        pos - create(spriteSize /. 2., spriteSize /. 2.)
      },
      ~width=spriteSize,
      ~height=spriteSize,
      env,
    )
  }

let drawSpriteWithCenterRotation = (
  t,
  name,
  ~pos as {x, y}: Point.Float.t,
  ~width,
  ~height,
  ~rot,
  env,
) => {
  Draw.pushMatrix(env)
  Draw.translate(~x, ~y, env)
  Draw.rotate(rot, env)
  drawSprite(t, name, ~pos=Point.Float.zero, ~width, ~height, env)
  Draw.popMatrix(env)
}
