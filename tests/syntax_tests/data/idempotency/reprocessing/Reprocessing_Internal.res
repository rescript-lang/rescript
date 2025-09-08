open Reprocessing_Common

open Reasongl

module Matrix = Reprocessing_Matrix

let getProgram = (
  ~context,
  ~vertexShader as vertexShaderSource: string,
  ~fragmentShader as fragmentShaderSource: string,
): option<Gl.programT> => {
  let vertexShader = Gl.createShader(~context, RGLConstants.vertex_shader)
  Gl.shaderSource(~context, ~shader=vertexShader, ~source=vertexShaderSource)
  Gl.compileShader(~context, vertexShader)
  let compiledCorrectly =
    Gl.getShaderParameter(~context, ~shader=vertexShader, ~paramName=Gl.Compile_status) == 1
  if compiledCorrectly {
    let fragmentShader = Gl.createShader(~context, RGLConstants.fragment_shader)
    Gl.shaderSource(~context, ~shader=fragmentShader, ~source=fragmentShaderSource)
    Gl.compileShader(~context, fragmentShader)
    let compiledCorrectly =
      Gl.getShaderParameter(~context, ~shader=fragmentShader, ~paramName=Gl.Compile_status) == 1
    if compiledCorrectly {
      let program = Gl.createProgram(~context)
      Gl.attachShader(~context, ~program, ~shader=vertexShader)
      Gl.deleteShader(~context, vertexShader)
      Gl.attachShader(~context, ~program, ~shader=fragmentShader)
      Gl.deleteShader(~context, fragmentShader)
      Gl.linkProgram(~context, program)
      let linkedCorrectly =
        Gl.getProgramParameter(~context, ~program, ~paramName=Gl.Link_status) == 1
      if linkedCorrectly {
        Some(program)
      } else {
        print_endline("Linking error: " ++ Gl.getProgramInfoLog(~context, program))
        None
      }
    } else {
      print_endline(
        "Fragment shader error: " ++ Gl.getShaderInfoLog(~context, fragmentShader),
      )
      None
    }
  } else {
    print_endline("Vertex shader error: " ++ Gl.getShaderInfoLog(~context, vertexShader))
    None
  }
}

let createCanvas = (window, height: int, width: int): glEnv => {
  Gl.Window.setWindowSize(~window, ~width, ~height)
  let context = Gl.Window.getContext(window)
  Gl.viewport(~context, ~x=-1, ~y=-1, ~width, ~height)
  Gl.clearColor(~context, ~r=0., ~g=0., ~b=0., ~a=1.)
  Gl.clear(~context, ~mask=lor(RGLConstants.color_buffer_bit, RGLConstants.depth_buffer_bit))

  /* ** Camera is a simple record containing one matrix used to project a point in 3D onto the screen. * */
  let camera = {projectionMatrix: Gl.Mat4.create()}
  let vertexBuffer = Gl.createBuffer(~context)
  let elementBuffer = Gl.createBuffer(~context)
  let program = switch getProgram(
    ~context,
    ~vertexShader=Reprocessing_Shaders.vertexShaderSource,
    ~fragmentShader=Reprocessing_Shaders.fragmentShaderSource,
  ) {
  | None => failwith("Could not create the program and/or the shaders. Aborting.")
  | Some(program) => program
  }
  Gl.useProgram(~context, program)

  /* ** Get the attribs ahead of time to be used inside the render function * */
  let aVertexPosition = Gl.getAttribLocation(~context, ~program, ~name="aVertexPosition")
  Gl.enableVertexAttribArray(~context, ~attribute=aVertexPosition)
  let aVertexColor = Gl.getAttribLocation(~context, ~program, ~name="aVertexColor")
  Gl.enableVertexAttribArray(~context, ~attribute=aVertexColor)
  let pMatrixUniform = Gl.getUniformLocation(~context, ~program, ~name="uPMatrix")
  Gl.uniformMatrix4fv(~context, ~location=pMatrixUniform, ~value=camera.projectionMatrix)

  /* ** Get attribute and uniform locations for later usage in the draw code. * */
  let aTextureCoord = Gl.getAttribLocation(~context, ~program, ~name="aTextureCoord")
  Gl.enableVertexAttribArray(~context, ~attribute=aTextureCoord)

  /* ** Generate texture buffer that we'll use to pass image data around. * */
  let texture = Gl.createTexture(~context)

  /* ** This tells OpenGL that we're going to be using texture0. OpenGL imposes a limit on the number of
       texture we can manipulate at the same time. That limit depends on the device. We don't care as we'll just
       always use texture0. **/
  Gl.activeTexture(~context, RGLConstants.texture0)

  /* ** Bind `texture` to `texture_2d` to modify it's magnification and minification params. * */
  Gl.bindTexture(~context, ~target=RGLConstants.texture_2d, ~texture)
  let uSampler = Gl.getUniformLocation(~context, ~program, ~name="uSampler")

  /* ** Load a dummy texture. This is because we're using the same shader for things with and without a texture */
  Gl.texImage2D_RGBA(
    ~context,
    ~target=RGLConstants.texture_2d,
    ~level=0,
    ~width=1,
    ~height=1,
    ~border=0,
    ~data=Gl.Bigarray.of_array(Gl.Bigarray.Uint8, [255, 255, 255, 255]),
  )
  Gl.texParameteri(
    ~context,
    ~target=RGLConstants.texture_2d,
    ~pname=RGLConstants.texture_mag_filter,
    ~param=RGLConstants.linear,
  )
  Gl.texParameteri(
    ~context,
    ~target=RGLConstants.texture_2d,
    ~pname=RGLConstants.texture_min_filter,
    ~param=RGLConstants.linear_mipmap_nearest,
  )

  /* ** Enable blend and tell OpenGL how to blend. */
  Gl.enable(~context, RGLConstants.blend)
  Gl.blendFunc(~context, RGLConstants.src_alpha, RGLConstants.one_minus_src_alpha)

  /* **
   * Will mutate the projectionMatrix to be an ortho matrix with the given boundaries.
   * See this link for quick explanation of what this is.
   * https://shearer12345.github.io/graphics/assets/projectionPerspectiveVSOrthographic.png
   */
  Gl.Mat4.ortho(
    ~out=camera.projectionMatrix,
    ~left=0.,
    ~right=float_of_int(width),
    ~bottom=float_of_int(height),
    ~top=0.,
    ~near=0.,
    ~far=1.,
  )
  {
    camera: camera,
    window: window,
    gl: context,
    batch: {
      vertexArray: Gl.Bigarray.create(Gl.Bigarray.Float32, circularBufferSize * vertexSize),
      elementArray: Gl.Bigarray.create(Gl.Bigarray.Uint16, circularBufferSize),
      vertexPtr: 0,
      elementPtr: 0,
      currTex: None,
      nullTex: texture,
    },
    vertexBuffer: vertexBuffer,
    elementBuffer: elementBuffer,
    aVertexPosition: aVertexPosition,
    aTextureCoord: aTextureCoord,
    aVertexColor: aVertexColor,
    pMatrixUniform: pMatrixUniform,
    uSampler: uSampler,
    keyboard: {
      keyCode: Reprocessing_Events.Nothing,
      pressed: Reprocessing_Common.KeySet.empty,
      released: Reprocessing_Common.KeySet.empty,
      down: Reprocessing_Common.KeySet.empty,
    },
    mouse: {
      pos: (0, 0),
      prevPos: (0, 0),
      pressed: false,
    },
    style: {
      fillColor: Some({r: 0., g: 0., b: 0., a: 1.}),
      strokeWeight: 3,
      strokeCap: Round,
      strokeColor: None,
      tintColor: None,
      rectMode: Corner,
    },
    styleStack: list{},
    matrix: Matrix.createIdentity(),
    matrixStack: list{},
    frame: {
      count: 1,
      rate: 10,
      deltaTime: 0.001,
    },
    size: {
      height: height,
      width: width,
      resizeable: true,
    },
  }
}

let makeLocalBatch = env => {
  vertexArray: Gl.Bigarray.create(Gl.Bigarray.Float32, circularBufferSize * vertexSize),
  elementArray: Gl.Bigarray.create(Gl.Bigarray.Uint16, circularBufferSize),
  vertexPtr: 0,
  elementPtr: 0,
  currTex: None,
  nullTex: env.batch.nullTex,
}

let drawGeometry = (
  ~vertexArray: Gl.Bigarray.t<float, Gl.Bigarray.float32_elt>,
  ~elementArray: Gl.Bigarray.t<int, Gl.Bigarray.int16_unsigned_elt>,
  ~mode,
  ~count,
  ~textureBuffer,
  env,
) => {
  /* Bind `vertexBuffer`, a pointer to chunk of memory to be sent to the GPU to the "register" called
   `array_buffer` */
  Gl.bindBuffer(~context=env.gl, ~target=RGLConstants.array_buffer, ~buffer=env.vertexBuffer)

  /* ** Copy all of the data over into whatever's in `array_buffer` (so here it's `vertexBuffer`) * */
  Gl.bufferData(
    ~context=env.gl,
    ~target=RGLConstants.array_buffer,
    ~data=vertexArray,
    ~usage=RGLConstants.stream_draw,
  )

  /* ** Tell the GPU about the shader attribute called `aVertexPosition` so it can access the data per vertex */
  Gl.vertexAttribPointer(
    ~context=env.gl,
    ~attribute=env.aVertexPosition,
    ~size=2,
    ~type_=RGLConstants.float_,
    ~normalize=false,
    ~stride=vertexSize * 4,
    ~offset=0,
  )

  /* ** Same as above but for `aVertexColor` * */
  Gl.vertexAttribPointer(
    ~context=env.gl,
    ~attribute=env.aVertexColor,
    ~size=4,
    ~type_=RGLConstants.float_,
    ~normalize=false,
    ~stride=vertexSize * 4,
    ~offset=2 * 4,
  )

  /* ** Same as above but for `aTextureCoord` * */
  Gl.vertexAttribPointer(
    ~context=env.gl,
    ~attribute=env.aTextureCoord,
    ~size=2,
    ~type_=RGLConstants.float_,
    ~normalize=false,
    ~stride=vertexSize * 4,
    ~offset=6 * 4,
  )

  /* ** Tell OpenGL about what the uniform called `uSampler` is pointing at, here it's given 0 which is what
   texture0 represent.  * */
  Gl.uniform1i(~context=env.gl, ~location=env.uSampler, ~value=0)

  /* ** Bind `elementBuffer`, a pointer to GPU memory to `element_array_buffer`. That "register" is used for
   the data representing the indices of the vertex. * */
  Gl.bindBuffer(
    ~context=env.gl,
    ~target=RGLConstants.element_array_buffer,
    ~buffer=env.elementBuffer,
  )

  /* ** Copy the `elementArray` into whatever buffer is in `element_array_buffer` * */
  Gl.bufferData(
    ~context=env.gl,
    ~target=RGLConstants.element_array_buffer,
    ~data=elementArray,
    ~usage=RGLConstants.stream_draw,
  )

  /* ** We bind `texture` to texture_2d, like we did for the vertex buffers in some ways (I think?) * */
  Gl.bindTexture(~context=env.gl, ~target=RGLConstants.texture_2d, ~texture=textureBuffer)

  /* ** Final call which actually tells the GPU to draw. * */
  Gl.drawElements(~context=env.gl, ~mode, ~count, ~type_=RGLConstants.unsigned_short, ~offset=0)
}

/*
 * Helper that will send the currently available data inside globalVertexArray.
 * This function assumes that the vertex data is stored as simple triangles.
 *
 * That function creates a new big array with a new size given the offset and len but does NOT copy the
 * underlying array of memory. So mutation done to that sub array will be reflected in the original one.
 */
let flushGlobalBatch = env =>
  if env.batch.elementPtr > 0 {
    let textureBuffer = switch env.batch.currTex {
    | None => env.batch.nullTex
    | Some(textureBuffer) => textureBuffer
    }
    drawGeometry(
      ~vertexArray=Gl.Bigarray.sub(env.batch.vertexArray, ~offset=0, ~len=env.batch.vertexPtr),
      ~elementArray=Gl.Bigarray.sub(env.batch.elementArray, ~offset=0, ~len=env.batch.elementPtr),
      ~mode=RGLConstants.triangles,
      ~count=env.batch.elementPtr,
      ~textureBuffer,
      env,
    )
    env.batch.currTex = None
    env.batch.vertexPtr = 0
    env.batch.elementPtr = 0
  }

let maybeFlushBatch = (~texture, ~el, ~vert, env) =>
  if (
    env.batch.elementPtr + el >= circularBufferSize ||
      (env.batch.vertexPtr + vert >= circularBufferSize ||
      env.batch.elementPtr > 0 && env.batch.currTex !== texture)
  ) {
    flushGlobalBatch(env)
  }

/*
 * This array packs all of the values that the shaders need: vertices, colors and texture coordinates.
 * We put them all in one as an optimization, so there are less back and forths between us and the GPU.
 *
 * The vertex array looks like:
 *
 * |<--------  8 * 4 bytes  ------->|
 *  --------------------------------
 * |  x  y  |  r  g  b  a  |  s  t  |  x2  y2  |  r2  g2  b2  a2  |  s2  t2  | ....
 *  --------------------------------
 * |           |              |
 * +- offset: 0 bytes, stride: 8 * 4 bytes (because we need to move by 8*4 bytes to get to the next x)
 *             |              |
 *             +- offset: 3 * 4 bytes, stride: 8 * 4 bytes
 *                            |
 *                            +- offset: (3 + 4) * 4 bytes, stride: 8 * 4 bytes
 *
 *
 * The element array is just an array of indices of vertices given that each vertex takes 8 * 4 bytes.
 * For example, if the element array looks like [|0, 1, 2, 1, 2, 3|], we're telling the GPU to draw 2
 * triangles: one with the vertices 0, 1 and 2 from the vertex array, and one with the vertices 1, 2 and 3.
 * We can "point" to duplicated vertices in our geometry to avoid sending those vertices.
 */
let addRectToGlobalBatch = (
  env,
  ~bottomRight as (x1, y1),
  ~bottomLeft as (x2, y2),
  ~topRight as (x3, y3),
  ~topLeft as (x4, y4),
  ~color as {r, g, b, a},
) => {
  maybeFlushBatch(~texture=None, ~el=6, ~vert=32, env)
  let set = Gl.Bigarray.set
  let i = env.batch.vertexPtr
  let vertexArrayToMutate = env.batch.vertexArray
  set(vertexArrayToMutate, i + 0, x1)
  set(vertexArrayToMutate, i + 1, y1)
  set(vertexArrayToMutate, i + 2, r)
  set(vertexArrayToMutate, i + 3, g)
  set(vertexArrayToMutate, i + 4, b)
  set(vertexArrayToMutate, i + 5, a)
  set(vertexArrayToMutate, i + 6, 0.0)
  set(vertexArrayToMutate, i + 7, 0.0)
  set(vertexArrayToMutate, i + 8, x2)
  set(vertexArrayToMutate, i + 9, y2)
  set(vertexArrayToMutate, i + 10, r)
  set(vertexArrayToMutate, i + 11, g)
  set(vertexArrayToMutate, i + 12, b)
  set(vertexArrayToMutate, i + 13, a)
  set(vertexArrayToMutate, i + 14, 0.0)
  set(vertexArrayToMutate, i + 15, 0.0)
  set(vertexArrayToMutate, i + 16, x3)
  set(vertexArrayToMutate, i + 17, y3)
  set(vertexArrayToMutate, i + 18, r)
  set(vertexArrayToMutate, i + 19, g)
  set(vertexArrayToMutate, i + 20, b)
  set(vertexArrayToMutate, i + 21, a)
  set(vertexArrayToMutate, i + 22, 0.0)
  set(vertexArrayToMutate, i + 23, 0.0)
  set(vertexArrayToMutate, i + 24, x4)
  set(vertexArrayToMutate, i + 25, y4)
  set(vertexArrayToMutate, i + 26, r)
  set(vertexArrayToMutate, i + 27, g)
  set(vertexArrayToMutate, i + 28, b)
  set(vertexArrayToMutate, i + 29, a)
  set(vertexArrayToMutate, i + 30, 0.0)
  set(vertexArrayToMutate, i + 31, 0.0)
  let ii = i / vertexSize
  let j = env.batch.elementPtr
  let elementArrayToMutate = env.batch.elementArray
  set(elementArrayToMutate, j + 0, ii)
  set(elementArrayToMutate, j + 1, ii + 1)
  set(elementArrayToMutate, j + 2, ii + 2)
  set(elementArrayToMutate, j + 3, ii + 1)
  set(elementArrayToMutate, j + 4, ii + 2)
  set(elementArrayToMutate, j + 5, ii + 3)
  env.batch.vertexPtr = i + 4 * vertexSize
  env.batch.elementPtr = j + 6
}

let drawTriangle = (env, (x1, y1), (x2, y2), (x3, y3), ~color as {r, g, b, a}) => {
  maybeFlushBatch(~texture=None, ~vert=3, ~el=24, env)
  let set = Gl.Bigarray.set
  let i = env.batch.vertexPtr
  let vertexArrayToMutate = env.batch.vertexArray
  set(vertexArrayToMutate, i + 0, x1)
  set(vertexArrayToMutate, i + 1, y1)
  set(vertexArrayToMutate, i + 2, r)
  set(vertexArrayToMutate, i + 3, g)
  set(vertexArrayToMutate, i + 4, b)
  set(vertexArrayToMutate, i + 5, a)
  set(vertexArrayToMutate, i + 6, 0.0)
  set(vertexArrayToMutate, i + 7, 0.0)
  set(vertexArrayToMutate, i + 8, x2)
  set(vertexArrayToMutate, i + 9, y2)
  set(vertexArrayToMutate, i + 10, r)
  set(vertexArrayToMutate, i + 11, g)
  set(vertexArrayToMutate, i + 12, b)
  set(vertexArrayToMutate, i + 13, a)
  set(vertexArrayToMutate, i + 14, 0.0)
  set(vertexArrayToMutate, i + 15, 0.0)
  set(vertexArrayToMutate, i + 16, x3)
  set(vertexArrayToMutate, i + 17, y3)
  set(vertexArrayToMutate, i + 18, r)
  set(vertexArrayToMutate, i + 19, g)
  set(vertexArrayToMutate, i + 20, b)
  set(vertexArrayToMutate, i + 21, a)
  set(vertexArrayToMutate, i + 22, 0.0)
  set(vertexArrayToMutate, i + 23, 0.0)
  let ii = i / vertexSize
  let j = env.batch.elementPtr
  let elementArrayToMutate = env.batch.elementArray
  set(elementArrayToMutate, j + 0, ii)
  set(elementArrayToMutate, j + 1, ii + 1)
  set(elementArrayToMutate, j + 2, ii + 2)
  env.batch.vertexPtr = i + 3 * vertexSize
  env.batch.elementPtr = j + 3
}

let drawLineWithMatrix = (
  ~p1 as (xx1, yy1),
  ~p2 as (xx2, yy2),
  ~matrix,
  ~color,
  ~width,
  ~project,
  env,
) => {
  let transform = Matrix.matptmul(matrix)
  let dx = xx2 -. xx1
  let dy = yy2 -. yy1
  let mag = sqrt(dx *. dx +. dy *. dy)
  let radius = width /. 2.
  let xthing = dy /. mag *. radius
  let ything = -.dx /. mag *. radius
  let (projectx, projecty) = project ? (dx /. mag *. radius, xthing) : (0., 0.)
  let x1 = xx2 +. xthing +. projectx
  let y1 = yy2 +. ything +. projecty
  let x2 = xx1 +. xthing -. projectx
  let y2 = yy1 +. ything -. projecty
  let x3 = xx2 -. xthing +. projectx
  let y3 = yy2 -. ything +. projecty
  let x4 = xx1 -. xthing -. projectx
  let y4 = yy1 -. ything -. projecty
  addRectToGlobalBatch(
    env,
    ~bottomRight=transform((x1, y1)),
    ~bottomLeft=transform((x2, y2)),
    ~topRight=transform((x3, y3)),
    ~topLeft=transform((x4, y4)),
    ~color,
  )
}

let drawArc = (
  env,
  (xCenterOfCircle: float, yCenterOfCircle: float),
  radx: float,
  rady: float,
  start: float,
  stop: float,
  isPie: bool,
  matrix: array<float>,
  {r, g, b, a},
) => {
  let transform = Matrix.matptmul(matrix)
  let noOfFans = int_of_float(radx +. rady) / 2 + 10
  maybeFlushBatch(~texture=None, ~vert=vertexSize * (noOfFans + 3), ~el=3 * noOfFans, env)
  let (start, stop) = stop < start ? (stop, start) : (start, stop)
  let pi = 4.0 *. atan(1.0)
  let anglePerFan = 2. *. pi /. float_of_int(noOfFans)
  let verticesData = env.batch.vertexArray
  let elementData = env.batch.elementArray
  let set = Gl.Bigarray.set
  let get = Gl.Bigarray.get
  let vertexArrayOffset = env.batch.vertexPtr
  let elementArrayOffset = env.batch.elementPtr
  let start_i = if isPie {
    /* Start one earlier and force the first point to be the center */
    int_of_float(start /. anglePerFan) - 3
  } else {
    int_of_float(start /. anglePerFan) - 2
  }
  let stop_i = int_of_float(stop /. anglePerFan) + 1
  for i in start_i to stop_i {
    let (xCoordinate, yCoordinate) = transform(
      if isPie && i - start_i == 0 {
        (
          /* force the first point to be the center */
          xCenterOfCircle,
          yCenterOfCircle,
        )
      } else {
        let angle = max(min(anglePerFan *. float_of_int(i + 1), stop), start)
        (xCenterOfCircle +. cos(angle) *. radx, yCenterOfCircle +. sin(angle) *. rady)
      },
    )
    let ii = (i - start_i) * vertexSize + vertexArrayOffset
    set(verticesData, ii + 0, xCoordinate)
    set(verticesData, ii + 1, yCoordinate)
    set(verticesData, ii + 2, r)
    set(verticesData, ii + 3, g)
    set(verticesData, ii + 4, b)
    set(verticesData, ii + 5, a)
    set(verticesData, ii + 6, 0.0)
    set(verticesData, ii + 7, 0.0)
    /* For the first three vertices, we don't do any deduping. Then for the subsequent ones, we'll actually
       have 3 elements, one pointing at the first vertex, one pointing at the previously added vertex and one
       pointing at the current vertex. This mimicks the behavior of triangle_fan. */
    if i - start_i < 3 {
      set(elementData, i - start_i + elementArrayOffset, ii / vertexSize)
    } else {
      /* We've already added 3 elements, for i = 0, 1 and 2. From now on, we'll add 3 elements _per_ i.
         To calculate the correct offset in `elementData` we remove 3 from i as if we're starting from 0 (the
         first time we enter this loop i = 3), then for each i we'll add 3 elements (so multiply by 3) BUT for
         i = 3 we want `jj` to be 3 so we shift everything by 3 (so add 3). Everything's also shifted by
         `elementArrayOffset` */
      let jj = (i - start_i - 3) * 3 + elementArrayOffset + 3
      set(elementData, jj, vertexArrayOffset / vertexSize)
      set(elementData, jj + 1, get(elementData, jj - 1))
      set(elementData, jj + 2, ii / vertexSize)
    }
  }
  env.batch.vertexPtr = env.batch.vertexPtr + (noOfFans + 3) * vertexSize
  env.batch.elementPtr = env.batch.elementPtr + (stop_i - start_i - 3) * 3 + 3
}

let drawEllipse = (env, center, radx: float, rady: float, matrix: array<float>, c) =>
  drawArc(env, center, radx, rady, 0., Reprocessing_Constants.tau, false, matrix, c)

let drawArcStroke = (
  env,
  (xCenterOfCircle: float, yCenterOfCircle: float),
  radx: float,
  rady: float,
  start: float,
  stop: float,
  isOpen: bool,
  isPie: bool,
  matrix: array<float>,
  {r, g, b, a} as strokeColor,
  strokeWidth,
) => {
  let transform = Matrix.matptmul(matrix)
  let verticesData = env.batch.vertexArray
  let elementData = env.batch.elementArray
  let noOfFans = int_of_float(radx +. rady) / 2 + 10
  let set = Gl.Bigarray.set
  maybeFlushBatch(~texture=None, ~vert=noOfFans * 2 * vertexSize, ~el=noOfFans * 6, env)
  let (start, stop) = stop < start ? (stop, start) : (start, stop)
  let pi = 4.0 *. atan(1.0)
  let anglePerFan = 2. *. pi /. float_of_int(noOfFans)
  /* I calculated this roughly by doing:
     anglePerFan *. float_of_int (i + 1) == start
     i+1 == start /. anglePerFan
 */
  let start_i = int_of_float(start /. anglePerFan) - 2
  let stop_i = int_of_float(stop /. anglePerFan)
  let prevEl: ref<option<(int, int)>> = ref(None)
  let strokeWidth = float_of_int(strokeWidth)
  let halfStrokeWidth = strokeWidth /. 2.
  for i in start_i to stop_i {
    let angle = max(start, min(anglePerFan *. float_of_int(i + 1), stop))
    let (xCoordinateInner, yCoordinateInner) = transform((
      xCenterOfCircle +. cos(angle) *. (radx -. halfStrokeWidth),
      yCenterOfCircle +. sin(angle) *. (rady -. halfStrokeWidth),
    ))
    let (xCoordinateOuter, yCoordinateOuter) = transform((
      xCenterOfCircle +. cos(angle) *. (radx +. halfStrokeWidth),
      yCenterOfCircle +. sin(angle) *. (rady +. halfStrokeWidth),
    ))
    let ii = env.batch.vertexPtr
    set(verticesData, ii + 0, xCoordinateInner)
    set(verticesData, ii + 1, yCoordinateInner)
    set(verticesData, ii + 2, r)
    set(verticesData, ii + 3, g)
    set(verticesData, ii + 4, b)
    set(verticesData, ii + 5, a)
    set(verticesData, ii + 6, 0.0)
    set(verticesData, ii + 7, 0.0)
    let ii = ii + vertexSize
    set(verticesData, ii + 0, xCoordinateOuter)
    set(verticesData, ii + 1, yCoordinateOuter)
    set(verticesData, ii + 2, r)
    set(verticesData, ii + 3, g)
    set(verticesData, ii + 4, b)
    set(verticesData, ii + 5, a)
    set(verticesData, ii + 6, 0.0)
    set(verticesData, ii + 7, 0.0)
    env.batch.vertexPtr = env.batch.vertexPtr + vertexSize * 2
    let currOuter = ii / vertexSize
    let currInner = ii / vertexSize - 1
    let currEl = Some((currInner, currOuter))
    switch prevEl.contents {
    | None => prevEl := currEl
    | Some((prevInner, prevOuter)) =>
      let elementArrayOffset = env.batch.elementPtr
      set(elementData, elementArrayOffset, prevInner)
      set(elementData, elementArrayOffset + 1, prevOuter)
      set(elementData, elementArrayOffset + 2, currOuter)
      set(elementData, elementArrayOffset + 3, currOuter)
      set(elementData, elementArrayOffset + 4, prevInner)
      set(elementData, elementArrayOffset + 5, currInner)
      env.batch.elementPtr = env.batch.elementPtr + 6
      prevEl := currEl
    }
  }
  if !isOpen {
    let startPt = (xCenterOfCircle +. cos(start) *. radx, yCenterOfCircle +. sin(start) *. rady)
    let stopPt = (xCenterOfCircle +. cos(stop) *. radx, yCenterOfCircle +. sin(stop) *. rady)
    let centerOfCircle = (xCenterOfCircle, yCenterOfCircle)
    if isPie {
      drawLineWithMatrix(
        ~p1=startPt,
        ~p2=centerOfCircle,
        ~matrix,
        ~color=strokeColor,
        ~width=strokeWidth,
        ~project=false,
        env,
      )
      drawLineWithMatrix(
        ~p1=stopPt,
        ~p2=centerOfCircle,
        ~matrix,
        ~color=strokeColor,
        ~width=strokeWidth,
        ~project=false,
        env,
      )
      drawEllipse(env, centerOfCircle, halfStrokeWidth, halfStrokeWidth, matrix, strokeColor)
    } else {
      drawLineWithMatrix(
        ~p1=startPt,
        ~p2=stopPt,
        ~matrix,
        ~color=strokeColor,
        ~width=strokeWidth,
        ~project=false,
        env,
      )
    }
    drawEllipse(env, startPt, halfStrokeWidth, halfStrokeWidth, matrix, strokeColor)
    drawEllipse(env, stopPt, halfStrokeWidth, halfStrokeWidth, matrix, strokeColor)
  }
}

let loadImage = (env: glEnv, filename, isPixel): imageT => {
  let imageRef = {glData: None, drawnTo: false}
  Gl.loadImage(
    ~filename,
    ~loadOption=LoadRGBA,
    ~callback=imageData =>
      switch imageData {
      | None =>
        failwith("Could not load image '" ++ (filename ++ "'.")) /* TODO: handle this better? */
      | Some(img) =>
        let context = env.gl
        let texture = Gl.createTexture(~context)
        let height = Gl.getImageHeight(img)
        let width = Gl.getImageWidth(img)
        let filter = isPixel ? Constants.nearest : Constants.linear
        imageRef.glData = Some({texture: texture, height: height, width: width, framebuffer: None})
        Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture)
        Gl.texImage2DWithImage(~context, ~target=Constants.texture_2d, ~level=0, ~image=img)
        Gl.texParameteri(
          ~context,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_mag_filter,
          ~param=filter,
        )
        Gl.texParameteri(
          ~context,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_min_filter,
          ~param=filter,
        )
        Gl.texParameteri(
          ~context,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_wrap_s,
          ~param=Constants.clamp_to_edge,
        )
        Gl.texParameteri(
          ~context,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_wrap_t,
          ~param=Constants.clamp_to_edge,
        )
      },
    (),
  )
  imageRef
}

let loadImageFromMemory = (env: glEnv, data, isPixel): imageT => {
  let imageRef = {glData: None, drawnTo: false}
  Gl.loadImageFromMemory(
    ~data,
    ~loadOption=LoadRGBA,
    ~callback=imageData =>
      switch imageData {
      | None => failwith("Could not load image") /* TODO: handle this better? */
      | Some(img) =>
        let env = env
        let texture = Gl.createTexture(~context=env.gl)
        let height = Gl.getImageHeight(img)
        let width = Gl.getImageWidth(img)
        let filter = isPixel ? Constants.nearest : Constants.linear
        imageRef.glData = Some({texture: texture, height: height, width: width, framebuffer: None})
        Gl.bindTexture(~context=env.gl, ~target=Constants.texture_2d, ~texture)
        Gl.texImage2DWithImage(~context=env.gl, ~target=Constants.texture_2d, ~level=0, ~image=img)
        Gl.texParameteri(
          ~context=env.gl,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_mag_filter,
          ~param=filter,
        )
        Gl.texParameteri(
          ~context=env.gl,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_min_filter,
          ~param=filter,
        )
        Gl.texParameteri(
          ~context=env.gl,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_wrap_s,
          ~param=Constants.clamp_to_edge,
        )
        Gl.texParameteri(
          ~context=env.gl,
          ~target=Constants.texture_2d,
          ~pname=Constants.texture_wrap_t,
          ~param=Constants.clamp_to_edge,
        )
      },
    (),
  )
  imageRef
}

let drawImage = (
  {width: imgw, height: imgh, texture},
  ~p1 as (x1, y1),
  ~p2 as (x2, y2),
  ~p3 as (x3, y3),
  ~p4 as (x4, y4),
  ~subx,
  ~suby,
  ~subw,
  ~subh,
  env,
) => {
  let {r, g, b, a} = switch env.style.tintColor {
  | Some(c) => c
  | None => {r: 1., g: 1., b: 1., a: 1.}
  }
  maybeFlushBatch(~texture=Some(texture), ~vert=32, ~el=6, env)
  let (fsubx, fsuby, fsubw, fsubh) = (
    float_of_int(subx) /. float_of_int(imgw),
    float_of_int(suby) /. float_of_int(imgh),
    float_of_int(subw) /. float_of_int(imgw),
    float_of_int(subh) /. float_of_int(imgh),
  )
  let set = Gl.Bigarray.set
  let ii = env.batch.vertexPtr
  let vertexArray = env.batch.vertexArray
  set(vertexArray, ii + 0, x1)
  set(vertexArray, ii + 1, y1)
  set(vertexArray, ii + 2, r)
  set(vertexArray, ii + 3, g)
  set(vertexArray, ii + 4, b)
  set(vertexArray, ii + 5, a)
  set(vertexArray, ii + 6, fsubx +. fsubw)
  set(vertexArray, ii + 7, fsuby +. fsubh)
  set(vertexArray, ii + 8, x2)
  set(vertexArray, ii + 9, y2)
  set(vertexArray, ii + 10, r)
  set(vertexArray, ii + 11, g)
  set(vertexArray, ii + 12, b)
  set(vertexArray, ii + 13, a)
  set(vertexArray, ii + 14, fsubx)
  set(vertexArray, ii + 15, fsuby +. fsubh)
  set(vertexArray, ii + 16, x3)
  set(vertexArray, ii + 17, y3)
  set(vertexArray, ii + 18, r)
  set(vertexArray, ii + 19, g)
  set(vertexArray, ii + 20, b)
  set(vertexArray, ii + 21, a)
  set(vertexArray, ii + 22, fsubx +. fsubw)
  set(vertexArray, ii + 23, fsuby)
  set(vertexArray, ii + 24, x4)
  set(vertexArray, ii + 25, y4)
  set(vertexArray, ii + 26, r)
  set(vertexArray, ii + 27, g)
  set(vertexArray, ii + 28, b)
  set(vertexArray, ii + 29, a)
  set(vertexArray, ii + 30, fsubx)
  set(vertexArray, ii + 31, fsuby)
  let jj = env.batch.elementPtr
  let elementArray = env.batch.elementArray
  set(elementArray, jj, ii / vertexSize)
  set(elementArray, jj + 1, ii / vertexSize + 1)
  set(elementArray, jj + 2, ii / vertexSize + 2)
  set(elementArray, jj + 3, ii / vertexSize + 1)
  set(elementArray, jj + 4, ii / vertexSize + 2)
  set(elementArray, jj + 5, ii / vertexSize + 3)
  env.batch.vertexPtr = ii + 4 * vertexSize
  env.batch.elementPtr = jj + 6
  env.batch.currTex = Some(texture)
}

let drawImageWithMatrixf = (image, ~x, ~y, ~width, ~height, ~subx, ~suby, ~subw, ~subh, env) => {
  let transform = Matrix.matptmul(env.matrix)
  let p1 = transform((x +. width, y +. height))
  let p2 = transform((x, y +. height))
  let p3 = transform((x +. width, y))
  let p4 = transform((x, y))
  drawImage(image, ~p1, ~p2, ~p3, ~p4, ~subx, ~suby, ~subw, ~subh, env)
}

let drawImageWithMatrix = (image, ~x, ~y, ~width, ~height, ~subx, ~suby, ~subw, ~subh, env) =>
  drawImageWithMatrixf(
    image,
    ~x=float_of_int(x),
    ~y=float_of_int(y),
    ~width=float_of_int(width),
    ~height=float_of_int(height),
    ~subx,
    ~suby,
    ~subw,
    ~subh,
    env,
  )

/* ** Recomputes matrices while resetting size of window */
let resetSize = (env, width, height) => {
  env.size.width = width
  env.size.height = height
  let (pixelWidth, pixelHeight) = {
    open Gl.Window
    (getPixelWidth(env.window), getPixelHeight(env.window))
  }
  Gl.viewport(~context=env.gl, ~x=0, ~y=0, ~width=pixelWidth, ~height=pixelHeight)
  Gl.clearColor(~context=env.gl, ~r=0., ~g=0., ~b=0., ~a=1.)
  Gl.Mat4.ortho(
    ~out=env.camera.projectionMatrix,
    ~left=0.,
    ~right=float_of_int(width),
    ~bottom=float_of_int(height),
    ~top=0.,
    ~near=0.,
    ~far=1.,
  )

  /* ** Tell OpenGL about what the uniform called `pMatrixUniform` is, here it's the projectionMatrix. * */
  Gl.uniformMatrix4fv(
    ~context=env.gl,
    ~location=env.pMatrixUniform,
    ~value=env.camera.projectionMatrix,
  )
}

let createImage = (width, height, env) => {
  let context = env.gl
  let texture = Gl.createTexture(~context)
  let isPixel = true
  let filter = isPixel ? Constants.nearest : Constants.linear
  Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture)
  Gl.texImage2D_null(~context, ~target=Constants.texture_2d, ~level=0, ~width, ~height)
  Gl.texParameteri(
    ~context,
    ~target=Constants.texture_2d,
    ~pname=Constants.texture_mag_filter,
    ~param=filter,
  )
  Gl.texParameteri(
    ~context,
    ~target=Constants.texture_2d,
    ~pname=Constants.texture_min_filter,
    ~param=filter,
  )
  Gl.texParameteri(
    ~context,
    ~target=Constants.texture_2d,
    ~pname=Constants.texture_wrap_s,
    ~param=Constants.clamp_to_edge,
  )
  Gl.texParameteri(
    ~context,
    ~target=Constants.texture_2d,
    ~pname=Constants.texture_wrap_t,
    ~param=Constants.clamp_to_edge,
  )
  let framebuffer = Gl.createFramebuffer(~context)
  Gl.bindFramebuffer(~context, ~target=Constants.framebuffer, ~framebuffer)

  /* ** Enable blend and tell OpenGL how to blend. */
  /* Gl.enable(~context, Constants.blend); */
  /* Gl.blendFunc(~context, Constants.src_alpha, Constants.one_minus_src_alpha); */
  Gl.framebufferTexture2D(
    ~context,
    ~target=Constants.framebuffer,
    ~attachment=Constants.color_attachment0,
    ~texTarget=Constants.texture_2d,
    ~texture,
  )
  Gl.bindDefaultFramebuffer(~context, ~target=Constants.framebuffer)
  {
    glData: Some({framebuffer: Some(framebuffer), texture: texture, width: width, height: height}),
    drawnTo: false,
  }
}

let drawOnImage = (image, env, cb) =>
  switch image.glData {
  | None => ()
  | Some(glData) =>
    let context = env.gl
    /* Create the framebuffer on the fly if it wasn't created (when you start drawing to an image
         you loaded from a file for example).
 */
    let framebuffer = switch glData.framebuffer {
    | None =>
      Gl.bindTexture(~context, ~target=Constants.texture_2d, ~texture=glData.texture)
      let framebuffer = Gl.createFramebuffer(~context)
      Gl.bindFramebuffer(~context, ~target=Constants.framebuffer, ~framebuffer)
      Gl.framebufferTexture2D(
        ~context,
        ~target=Constants.framebuffer,
        ~attachment=Constants.color_attachment0,
        ~texTarget=Constants.texture_2d,
        ~texture=glData.texture,
      )
      framebuffer
    | Some(framebuffer) => framebuffer
    }
    Gl.bindFramebuffer(~context, ~target=Constants.framebuffer, ~framebuffer)
    /* Create a new env that we pass to the callback */
    let newEnv = {
      ...env,
      batch: makeLocalBatch(env),
      camera: {
        projectionMatrix: Gl.Mat4.create(),
      },
      style: {
        ...env.style,
        strokeWeight: env.style.strokeWeight /* we need this because ocaml doesn't support record spread without any value added */,
      },
      matrix: Matrix.createIdentity(),
      matrixStack: List.map(m => {
        let mm = Matrix.createIdentity()
        Matrix.copyInto(~src=m, ~dst=mm)
        mm
      }, env.matrixStack),
      styleStack: List.map(s => {...s, strokeWeight: s.strokeWeight}, env.styleStack),
      size: {
        ...env.size,
        width: glData.width,
        height: glData.height,
      },
    }
    Matrix.copyInto(~src=env.matrix, ~dst=newEnv.matrix)
    /* Set the size of the viewport.
         This may be wrong on retina screens... I'm not 100% sure.

         Also calculate a new ortho matrix and send it to the GPU immediately.

                      Ben - March 25th 2018
 */
    Gl.viewport(~context, ~x=0, ~y=0, ~width=glData.width, ~height=glData.height)
    Gl.Mat4.ortho(
      ~out=newEnv.camera.projectionMatrix,
      ~left=0.,
      ~right=float_of_int(glData.width),
      ~bottom=0.,
      ~top=float_of_int(glData.height),
      ~near=0.,
      ~far=1.,
    )
    Gl.uniformMatrix4fv(
      ~context=newEnv.gl,
      ~location=newEnv.pMatrixUniform,
      ~value=newEnv.camera.projectionMatrix,
    )
    Gl.clearColor(~context, ~r=0., ~g=0., ~b=0., ~a=0.)
    cb(newEnv)
    flushGlobalBatch(newEnv)
    image.drawnTo = true
    Gl.clearColor(~context, ~r=0., ~g=0., ~b=0., ~a=1.)
    /* Reset everything back to normal */
    Gl.bindDefaultFramebuffer(~context, ~target=Constants.framebuffer)
    let (pixelWidth, pixelHeight) = {
      open Gl.Window
      (getPixelWidth(env.window), getPixelHeight(env.window))
    }
    Gl.viewport(~context, ~x=0, ~y=0, ~width=pixelWidth, ~height=pixelHeight)
    Gl.uniformMatrix4fv(~context, ~location=env.pMatrixUniform, ~value=env.camera.projectionMatrix)
  }

let clearImage = (image, env) => {
  image.drawnTo = false
  switch image.glData {
  | None => ()
  | Some(glData) =>
    switch glData.framebuffer {
    | None => ()
    | Some(framebuffer) =>
      Gl.bindFramebuffer(~context=env.gl, ~target=Constants.framebuffer, ~framebuffer)
      Reasongl.Gl.clear(
        ~context=env.gl,
        ~mask=lor(Constants.color_buffer_bit, Constants.depth_buffer_bit),
      )
      Gl.bindDefaultFramebuffer(~context=env.gl, ~target=Constants.framebuffer)
    }
  }
}
