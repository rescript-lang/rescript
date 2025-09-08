open Reprocessing_Common

@@ocaml.doc("
 * The Draw module
 *
 * This is where all the fancy things happen.
 *
 * ```reason;shared(sandbox)
 * [@val] external sandboxCanvasId: string = \"\";
 * [@val] external sandboxCanvas: 'canvas = \"\";
 * [@val] external containerDiv: 'node = \"\";
 * [@send] external addEventListener: ('node, string, 'eventT => unit) => unit = \"addEventListener\";
 * let id = sandboxCanvasId;
 * addEventListener(containerDiv, \"mouseleave\", (_) => Reprocessing.playPause(id, false)->ignore);
 * addEventListener(containerDiv, \"mouseenter\", (_) => Reprocessing.playPause(id, true)->ignore);
 * Reprocessing.setScreenId(sandboxCanvasId);
 * ```
 *
 * ```reason;shared(draw);use(sandbox)
 * open Reprocessing;
 * run(~setup=(_) => (), ~draw=((), env) => {
 * %{code}%
 * })
 * ```
 *
 * ```canvas;use(draw)
 * fill(Constants.red, env);
 * rect(~pos=Env.mouse(env), ~width=5, ~height=5, env);
 * ```
 ")

module Internal = Reprocessing_Internal

module Matrix = Reprocessing_Matrix

module Env = Reprocessing_Env

module Font = Reprocessing_Font.Font

let translate = (~x, ~y, env) => {
  open Matrix
  matmatmul(env.matrix, createTranslation(x, y))
}

let rotate = (theta, env) => {
  open Matrix
  matmatmul(env.matrix, createRotation(theta))
}

let scale = (~x, ~y, env) => {
  open Matrix
  matmatmul(env.matrix, createScaling(x, y))
}

let shear = (~x, ~y, env) => {
  open Matrix
  matmatmul(env.matrix, createShearing(x, y))
}

let fill = (color, env: glEnv) => env.style = {...env.style, fillColor: Some(color)}

let noFill = (env: glEnv) => env.style = {...env.style, fillColor: None}

let tint = (color, env: glEnv) => env.style = {...env.style, tintColor: Some(color)}

let noTint = (env: glEnv) => env.style = {...env.style, tintColor: None}

let stroke = (color, env) => env.style = {...env.style, strokeColor: Some(color)}

let noStroke = env => env.style = {...env.style, strokeColor: None}

let strokeWeight = (weight, env) => env.style = {...env.style, strokeWeight: weight}

let strokeCap = (cap, env) => env.style = {...env.style, strokeCap: cap}

let rectMode = (rm, env) => env.style = {...env.style, rectMode: rm}

let pushStyle = env => env.styleStack = list{env.style, ...env.styleStack}

let popStyle = env =>
  switch env.styleStack {
  /* Matches Processing error message */
  | list{} => failwith("Too many `popStyle` without enough `pushStyle`.")
  | list{hd, ...tl} =>
    env.style = hd
    env.styleStack = tl
  }

let pushMatrix = env => {
  let copy = Matrix.createIdentity()
  Matrix.copyInto(~src=env.matrix, ~dst=copy)
  env.matrixStack = list{copy, ...env.matrixStack}
}

let popMatrix = env =>
  switch env.matrixStack {
  | list{} => failwith("Too many `popMatrix` without enough `pushMatrix`.")
  | list{hd, ...tl} =>
    env.matrix = hd
    env.matrixStack = tl
  }

let loadImage = (~filename, ~isPixel=false, env) => Internal.loadImage(env, filename, isPixel)

let subImage = (
  img,
  ~pos as (x, y),
  ~width,
  ~height,
  ~texPos as (subx, suby),
  ~texWidth as subw,
  ~texHeight as subh,
  env,
) =>
  switch img.glData {
  | None => print_endline("image not ready yet, just doing nothing :D")
  | Some(i) =>
    Internal.drawImageWithMatrix(i, ~x, ~y, ~width, ~height, ~subx, ~suby, ~subw, ~subh, env)
  }

let subImagef = (
  img,
  ~pos as (x, y),
  ~width,
  ~height,
  ~texPos as (subx, suby),
  ~texWidth as subw,
  ~texHeight as subh,
  env,
) =>
  switch img.glData {
  | None => print_endline("image not ready yet, just doing nothing :D")
  | Some(i) =>
    Internal.drawImageWithMatrixf(i, ~x, ~y, ~width, ~height, ~subx, ~suby, ~subw, ~subh, env)
  }

let imagef = (img, ~pos as (x, y), ~width=?, ~height=?, env: glEnv) =>
  switch img.glData {
  | None => print_endline("image not ready yet, just doing nothing :D")
  | Some({width: imgw, height: imgh} as img) =>
    let imgwf = float_of_int(imgw)
    let imghf = float_of_int(imgh)
    switch (width, imgwf, height, imghf) {
    | (None, w, None, h)
    | (None, w, Some(h), _)
    | (Some(w), _, None, h)
    | (Some(w), _, Some(h), _) =>
      Internal.drawImageWithMatrixf(
        img,
        ~x,
        ~y,
        ~width=w,
        ~height=h,
        ~subx=0,
        ~suby=0,
        ~subw=imgw,
        ~subh=imgh,
        env,
      )
    }
  }

let image = (img, ~pos as (x, y), ~width=?, ~height=?, env: glEnv) =>
  switch img.glData {
  | None => print_endline("image not ready yet, just doing nothing :D")
  | Some({width: imgw, height: imgh} as img) =>
    switch (width, imgw, height, imgh) {
    | (None, w, None, h)
    | (None, w, Some(h), _)
    | (Some(w), _, None, h)
    | (Some(w), _, Some(h), _) =>
      Internal.drawImageWithMatrix(
        img,
        ~x,
        ~y,
        ~width=w,
        ~height=h,
        ~subx=0,
        ~suby=0,
        ~subw=imgw,
        ~subh=imgh,
        env,
      )
    }
  }

let linef = (~p1, ~p2, env: glEnv) => {
  let color = switch env.style.strokeColor {
  | None => Reprocessing_Constants.black /* default color black */
  | Some(color) => color
  }
  let width = float_of_int(env.style.strokeWeight)
  let radius = width /. 2.
  let project = env.style.strokeCap == Project
  Internal.drawLineWithMatrix(~p1, ~p2, ~matrix=env.matrix, ~color, ~width, ~project, env)
  if env.style.strokeCap == Round {
    Internal.drawEllipse(env, p1, radius, radius, env.matrix, color)
    Internal.drawEllipse(env, p2, radius, radius, env.matrix, color)
  }
}

let line = (~p1 as (x1, y1), ~p2 as (x2, y2), env: glEnv) =>
  linef(~p1=(float_of_int(x1), float_of_int(y1)), ~p2=(float_of_int(x2), float_of_int(y2)), env)

let ellipsef = (~center, ~radx, ~rady, env: glEnv) => {
  switch env.style.fillColor {
  | None => () /* Don't draw fill */
  | Some(fill) => Internal.drawEllipse(env, center, radx, rady, env.matrix, fill)
  }
  switch env.style.strokeColor {
  | None => () /* Don't draw stroke */
  | Some(stroke) =>
    Internal.drawArcStroke(
      env,
      center,
      radx,
      rady,
      0.,
      Reprocessing_Constants.tau,
      true,
      false,
      env.matrix,
      stroke,
      env.style.strokeWeight,
    )
  }
}

let ellipse = (~center as (cx, cy), ~radx, ~rady, env: glEnv) =>
  ellipsef(
    ~center=(float_of_int(cx), float_of_int(cy)),
    ~radx=float_of_int(radx),
    ~rady=float_of_int(rady),
    env,
  )

let quadf = (~p1, ~p2, ~p3, ~p4, env: glEnv) => {
  let transform = Matrix.matptmul(env.matrix)
  let (tp1, tp2, tp3, tp4) = (transform(p1), transform(p2), transform(p3), transform(p4))
  switch env.style.fillColor {
  | None => () /* Don't draw fill */
  | Some(fill) =>
    Internal.addRectToGlobalBatch(
      env,
      ~topLeft=tp1,
      ~topRight=tp2,
      ~bottomRight=tp3,
      ~bottomLeft=tp4,
      ~color=fill,
    )
  }
  switch env.style.strokeColor {
  | None => () /* don't draw stroke */
  | Some(color) =>
    let width = float_of_int(env.style.strokeWeight)
    let project = false
    let matrix = env.matrix
    Internal.drawLineWithMatrix(~p1, ~p2, ~matrix, ~color, ~width, ~project, env)
    Internal.drawLineWithMatrix(~p1=p2, ~p2=p3, ~matrix, ~color, ~width, ~project, env)
    Internal.drawLineWithMatrix(~p1=p3, ~p2=p4, ~matrix, ~color, ~width, ~project, env)
    Internal.drawLineWithMatrix(~p1, ~p2=p4, ~matrix, ~color, ~width, ~project, env)
    let r = width /. 2.
    Internal.drawEllipse(env, p1, r, r, matrix, color)
    Internal.drawEllipse(env, p2, r, r, matrix, color)
    Internal.drawEllipse(env, p3, r, r, matrix, color)
    Internal.drawEllipse(env, p4, r, r, matrix, color)
  }
}

let quad = (~p1 as (x1, y1), ~p2 as (x2, y2), ~p3 as (x3, y3), ~p4 as (x4, y4), env: glEnv) =>
  quadf(
    ~p1=(float_of_int(x1), float_of_int(y1)),
    ~p2=(float_of_int(x2), float_of_int(y2)),
    ~p3=(float_of_int(x3), float_of_int(y3)),
    ~p4=(float_of_int(x4), float_of_int(y4)),
    env,
  )

let rectf = (~pos as (x, y), ~width, ~height, env: glEnv) =>
  switch env.style.rectMode {
  | Corner =>
    quadf(~p1=(x, y), ~p2=(x +. width, y), ~p3=(x +. width, y +. height), ~p4=(x, y +. height), env)
  | Center =>
    let x = x -. width /. 2.
    let y = y -. height /. 2.
    quadf(~p1=(x, y), ~p2=(x +. width, y), ~p3=(x +. width, y +. height), ~p4=(x, y +. height), env)
  | Radius =>
    let x = x -. width
    let y = y -. height
    let width = width *. 2.
    let height = height *. 2.
    quadf(~p1=(x, y), ~p2=(x +. width, y), ~p3=(x +. width, y +. height), ~p4=(x, y +. height), env)
  }

let rect = (~pos as (x, y), ~width, ~height, env: glEnv) =>
  rectf(
    ~pos=(float_of_int(x), float_of_int(y)),
    ~width=float_of_int(width),
    ~height=float_of_int(height),
    env,
  )

let bezierPoint = ((xx1, yy1), (xx2, yy2), (xx3, yy3), (xx4, yy4), t) => (
  (1. -. t) ** 3. *. xx1 +.
  3. *. (1. -. t) ** 2. *. t *. xx2 +.
  3. *. (1. -. t) *. t ** 2. *. xx3 +.
  t ** 3. *. xx4,
  (1. -. t) ** 3. *. yy1 +.
  3. *. (1. -. t) ** 2. *. t *. yy2 +.
  3. *. (1. -. t) *. t ** 2. *. yy3 +.
  t ** 3. *. yy4,
)

let bezierTangent = ((xx1, yy1), (xx2, yy2), (xx3, yy3), (xx4, yy4), t) => (
  -3. *. (1. -. t) ** 2. *. xx1 +.
  3. *. (1. -. t) ** 2. *. xx2 -.
  6. *. t *. (1. -. t) *. xx2 -.
  3. *. t ** 2. *. xx3 +.
  6. *. t *. (1. -. t) *. xx3 +.
  3. *. t ** 2. *. xx4,
  -3. *. (1. -. t) ** 2. *. yy1 +.
  3. *. (1. -. t) ** 2. *. yy2 -.
  6. *. t *. (1. -. t) *. yy2 -.
  3. *. t ** 2. *. yy3 +.
  6. *. t *. (1. -. t) *. yy3 +.
  3. *. t ** 2. *. yy4,
)

let bezier = (
  ~p1 as (xx1, yy1),
  ~p2 as (xx2, yy2),
  ~p3 as (xx3, yy3),
  ~p4 as (xx4, yy4),
  env: glEnv,
) =>
  /* @speed this thing can reuse points */
  for i in 0 to 19 {
    let (x1, y1) = bezierPoint(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i) /. 20.0,
    )
    let (x2, y2) = bezierPoint(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i + 1) /. 20.0,
    )
    let (tangent_x1, tangent_y1) = bezierTangent(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i) /. 20.0,
    )
    let (tangent_x2, tangent_y2) = bezierTangent(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i + 1) /. 20.0,
    )
    let a1 = atan2(tangent_y1, tangent_x1) -. Reprocessing_Constants.half_pi
    let a2 = atan2(tangent_y2, tangent_x2) -. Reprocessing_Constants.half_pi
    let strokeWeightf = float_of_int(env.style.strokeWeight)
    quadf(
      ~p1=(x1 +. cos(a1) *. strokeWeightf /. 2., y1 +. sin(a1) *. strokeWeightf /. 2.),
      ~p2=(x1 -. cos(a1) *. strokeWeightf /. 2., y1 -. sin(a1) *. strokeWeightf /. 2.),
      ~p3=(x2 -. cos(a2) *. strokeWeightf /. 2., y2 -. sin(a2) *. strokeWeightf /. 2.),
      ~p4=(x2 +. cos(a2) *. strokeWeightf /. 2., y2 +. sin(a2) *. strokeWeightf /. 2.),
      env,
    )
  }

let curvePoint = ((xx1, yy1), (xx2, yy2), (xx3, yy3), (xx4, yy4), t) => {
  let mx0 = (1. -. 0.5) *. (xx3 -. xx1) /* @feature tightness will be defined as the 0 here */
  let my0 = (1. -. 0.5) *. (yy3 -. yy1)
  let mx1 = (1. -. 0.5) *. (xx4 -. xx2)
  let my1 = (1. -. 0.5) *. (yy4 -. yy2)
  (
    (2. *. t ** 3. -. 3. *. t ** 2. +. 1.) *. xx2 +.
    (t ** 3. -. 2. *. t ** 2. +. t) *. mx0 +.
    (-2. *. t ** 3. +. 3. *. t ** 2.) *. xx3 +.
    (t ** 3. -. t ** 2.) *. mx1,
    (2. *. t ** 3. -. 3. *. t ** 2. +. 1.) *. yy2 +.
    (t ** 3. -. 2. *. t ** 2. +. t) *. my0 +.
    (-2. *. t ** 3. +. 3. *. t ** 2.) *. yy3 +.
    (t ** 3. -. t ** 2.) *. my1,
  )
}

let curveTangent = ((xx1, yy1), (xx2, yy2), (xx3, yy3), (xx4, yy4), t) => {
  let mx0 = (1. -. 0.5) *. (xx3 -. xx1) /* @feature tightness will be defined as the 0 here */
  let my0 = (1. -. 0.5) *. (yy3 -. yy1)
  let mx1 = (1. -. 0.5) *. (xx4 -. xx2)
  let my1 = (1. -. 0.5) *. (yy4 -. yy2)
  (
    (6. *. t ** 2. -. 6. *. t) *. xx2 +.
    (3. *. t ** 2. -. 4. *. t +. 1.) *. mx0 +.
    (-6. *. t ** 2. +. 6. *. t) *. xx3 +.
    (3. *. t ** 2. -. 2. *. t) *. mx1,
    (6. *. t ** 2. -. 6. *. t) *. yy2 +.
    (3. *. t ** 2. -. 4. *. t +. 1.) *. my0 +.
    (-6. *. t ** 2. +. 6. *. t) *. yy3 +.
    (3. *. t ** 2. -. 2. *. t) *. my1,
  )
}

let curve = ((xx1, yy1), (xx2, yy2), (xx3, yy3), (xx4, yy4), env: glEnv) =>
  for i in 0 to 19 {
    let (x1, y1) = curvePoint(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i) /. 20.0,
    )
    let (x2, y2) = curvePoint(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i + 1) /. 20.0,
    )
    let (tangent_x1, tangent_y1) = curveTangent(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i) /. 20.0,
    )
    let (tangent_x2, tangent_y2) = curveTangent(
      (xx1, yy1),
      (xx2, yy2),
      (xx3, yy3),
      (xx4, yy4),
      float_of_int(i + 1) /. 20.0,
    )
    let a1 = atan2(tangent_y1, tangent_x1) -. Reprocessing_Constants.half_pi
    let a2 = atan2(tangent_y2, tangent_x2) -. Reprocessing_Constants.half_pi
    let strokeWeightf = float_of_int(env.style.strokeWeight)
    quadf(
      ~p1=(x1 +. cos(a1) *. strokeWeightf /. 2., y1 +. sin(a1) *. strokeWeightf /. 2.),
      ~p2=(x1 -. cos(a1) *. strokeWeightf /. 2., y1 -. sin(a1) *. strokeWeightf /. 2.),
      ~p3=(x2 -. cos(a2) *. strokeWeightf /. 2., y2 -. sin(a2) *. strokeWeightf /. 2.),
      ~p4=(x2 +. cos(a2) *. strokeWeightf /. 2., y2 +. sin(a2) *. strokeWeightf /. 2.),
      env,
    )
  }

let pixelf = (~pos as (x, y), ~color, env: glEnv) => {
  let w = float_of_int(env.style.strokeWeight)
  Internal.addRectToGlobalBatch(
    env,
    ~bottomRight=(x +. w, y +. w),
    ~bottomLeft=(x, y +. w),
    ~topRight=(x +. w, y),
    ~topLeft=(x, y),
    ~color,
  )
}

let pixel = (~pos as (x, y), ~color, env: glEnv) =>
  pixelf(~pos=(float_of_int(x), float_of_int(y)), ~color, env)

let trianglef = (~p1, ~p2, ~p3, env: glEnv) => {
  let transform = Matrix.matptmul(env.matrix)
  let (tp1, tp2, tp3) = (transform(p1), transform(p2), transform(p3))
  switch env.style.fillColor {
  | None => () /* don't draw fill */
  | Some(color) => Internal.drawTriangle(env, tp1, tp2, tp3, ~color)
  }
  switch env.style.strokeColor {
  | None => () /* don't draw stroke */
  | Some(color) =>
    let width = float_of_int(env.style.strokeWeight)
    let project = false
    let matrix = env.matrix
    Internal.drawLineWithMatrix(~p1, ~p2, ~matrix, ~color, ~width, ~project, env)
    Internal.drawLineWithMatrix(~p1=p2, ~p2=p3, ~matrix, ~color, ~width, ~project, env)
    Internal.drawLineWithMatrix(~p1, ~p2=p3, ~matrix, ~color, ~width, ~project, env)
    let r = width /. 2.
    Internal.drawEllipse(env, p1, r, r, matrix, color)
    Internal.drawEllipse(env, p2, r, r, matrix, color)
    Internal.drawEllipse(env, p3, r, r, matrix, color)
  }
}

let triangle = (~p1 as (x1, y1), ~p2 as (x2, y2), ~p3 as (x3, y3), env: glEnv) =>
  trianglef(
    ~p1=(float_of_int(x1), float_of_int(y1)),
    ~p2=(float_of_int(x2), float_of_int(y2)),
    ~p3=(float_of_int(x3), float_of_int(y3)),
    env,
  )

let arcf = (~center, ~radx, ~rady, ~start, ~stop, ~isOpen, ~isPie, env: glEnv) => {
  switch env.style.fillColor {
  | None => () /* don't draw fill */
  | Some(color) => Internal.drawArc(env, center, radx, rady, start, stop, isPie, env.matrix, color)
  }
  switch env.style.strokeColor {
  | None => () /* don't draw stroke */
  | Some(stroke) =>
    Internal.drawArcStroke(
      env,
      center,
      radx,
      rady,
      start,
      stop,
      isOpen,
      isPie,
      env.matrix,
      stroke,
      env.style.strokeWeight,
    )
  }
}

let arc = (~center as (cx, cy), ~radx, ~rady, ~start, ~stop, ~isOpen, ~isPie, env: glEnv) =>
  arcf(
    ~center=(float_of_int(cx), float_of_int(cy)),
    ~radx=float_of_int(radx),
    ~rady=float_of_int(rady),
    ~start,
    ~stop,
    ~isOpen,
    ~isPie,
    env,
  )

let loadFont = (~filename, ~isPixel=false, env: glEnv) =>
  Font.parseFontFormat(env, filename, isPixel)

let text = (~font=?, ~body, ~pos as (x, y), env: glEnv) =>
  Font.drawString(env, font, body, float_of_int(x), float_of_int(y))

let textWidth = (~font=?, ~body, env) => int_of_float(Font.calcStringWidth(env, font, body))

let clear = env =>
  Reasongl.Gl.clear(
    ~context=env.gl,
    ~mask=lor(Constants.color_buffer_bit, Constants.depth_buffer_bit),
  )

let background = (color, env: glEnv) => {
  clear(env)
  let w = float_of_int(Env.width(env))
  let h = float_of_int(Env.height(env))
  Internal.addRectToGlobalBatch(
    env,
    ~bottomRight=(w, h),
    ~bottomLeft=(0., h),
    ~topRight=(w, 0.),
    ~topLeft=(0., 0.),
    ~color,
  )
}

let createImage = (~width, ~height, env) => Internal.createImage(width, height, env)

let withImage = (image, env, cb) => Internal.drawOnImage(image, env, cb)

let isImageDrawnTo = image => image.drawnTo

let clearImage = (image, env) => Internal.clearImage(image, env)
