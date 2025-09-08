module type t = {
  let target: string
  type contextT
  module type FileT = {
    type t
    let readFile: (~filename: string, ~cb: string => unit) => unit
  }
  module File: FileT
  module type WindowT = {
    type t
    let getWidth: t => int
    let getHeight: t => int
    let getPixelWidth: t => int
    let getPixelHeight: t => int
    let getPixelScale: t => float
    let init: (~screen: string=?, ~argv: array<string>) => t
    let setWindowSize: (~window: t, ~width: int, ~height: int) => unit
    let setWindowTitle: (~window: t, ~title: string) => unit
    let getContext: t => contextT
  }
  module Window: WindowT
  module type AudioT = {
    type t
    let loadSound: (Window.t, string, t => unit) => unit
    let playSound: (Window.t, t, ~volume: float, ~loop: bool) => unit
  }
  module Audio: AudioT
  module Events: RGLEvents.t

  /* ** We're currently mimicking the JS asynchronous event handling allowing the user to register callbacks.
   * Instead of mutating global state in the Events module, we simply force the user to register all events
   * handlers at once, allowing us to use the closure to keep track of the data for us.
   * For native, the easiest way to handle events is in the render loop, so we force the user to also
   * register the draw call `displayFunc` which will effectively do all of the rendering.
   */
  let render: (
    ~window: Window.t,
    ~mouseDown: (~button: Events.buttonStateT, ~state: Events.stateT, ~x: int, ~y: int) => unit=?,
    ~mouseUp: (~button: Events.buttonStateT, ~state: Events.stateT, ~x: int, ~y: int) => unit=?,
    ~mouseMove: (~x: int, ~y: int) => unit=?,
    ~keyDown: (~keycode: Events.keycodeT, ~repeat: bool) => unit=?,
    ~keyUp: (~keycode: Events.keycodeT) => unit=?,
    ~windowResize: unit => unit=?,
    ~displayFunc: float => unit,
    unit,
    bool,
  ) => bool
  type programT
  type shaderT
  let clearColor: (~context: contextT, ~r: float, ~g: float, ~b: float, ~a: float) => unit
  let createProgram: (~context: contextT) => programT
  let createShader: (~context: contextT, int) => shaderT
  let attachShader: (~context: contextT, ~program: programT, ~shader: shaderT) => unit
  let deleteShader: (~context: contextT, shaderT) => unit
  let shaderSource: (~context: contextT, ~shader: shaderT, ~source: string) => unit
  let compileShader: (~context: contextT, shaderT) => unit
  let linkProgram: (~context: contextT, programT) => unit
  let useProgram: (~context: contextT, programT) => unit
  type bufferT
  type attributeT
  type uniformT
  let createBuffer: (~context: contextT) => bufferT
  let bindBuffer: (~context: contextT, ~target: int, ~buffer: bufferT) => unit
  type textureT
  let createTexture: (~context: contextT) => textureT
  let activeTexture: (~context: contextT, int) => unit
  let bindTexture: (~context: contextT, ~target: int, ~texture: textureT) => unit
  let texParameteri: (~context: contextT, ~target: int, ~pname: int, ~param: int) => unit
  type framebufferT
  let createFramebuffer: (~context: contextT) => framebufferT
  let bindFramebuffer: (~context: contextT, ~target: int, ~framebuffer: framebufferT) => unit
  let bindDefaultFramebuffer: (~context: contextT, ~target: int) => unit
  let framebufferTexture2D: (
    ~context: contextT,
    ~target: int,
    ~attachment: int,
    ~texTarget: int,
    ~texture: textureT,
  ) => unit
  /* let drawBuffers : (~context : contextT, ~target: int) => unit; */
  /* type rawTextureDataT;
   let toTextureData: array int => rawTextureDataT; */
  let enable: (~context: contextT, int) => unit
  let disable: (~context: contextT, int) => unit
  let blendFunc: (~context: contextT, int, int) => unit
  module type Bigarray = {
    type t<'a, 'b>
    type float64_elt
    type float32_elt
    type int16_unsigned_elt
    type int16_signed_elt
    type int8_unsigned_elt
    type int8_signed_elt
    type int_elt
    type int32_elt
    type int64_elt
    type rec kind<'a, 'b> =
      | Float64: kind<float, float64_elt>
      | Float32: kind<float, float32_elt>
      | Int16: kind<int, int16_signed_elt>
      | Uint16: kind<int, int16_unsigned_elt>
      | Int8: kind<int, int8_signed_elt>
      | Uint8: kind<int, int8_unsigned_elt>
      | Char: kind<char, int8_unsigned_elt>
      | Int: kind<int, int_elt>
      | Int64: kind<int64, int64_elt>
      | Int32: kind<int32, int32_elt>
    let create: (kind<'a, 'b>, int) => t<'a, 'b>
    let of_array: (kind<'a, 'b>, array<'a>) => t<'a, 'b>
    let dim: t<'a, 'b> => int
    let blit: (t<'a, 'b>, t<'a, 'b>) => unit
    let unsafe_blit: (t<'a, 'b>, t<'a, 'b>, ~offset: int, ~numOfBytes: int) => unit
    let get: (t<'a, 'b>, int) => 'a
    let unsafe_get: (t<'a, 'b>, int) => 'a
    let set: (t<'a, 'b>, int, 'a) => unit
    let unsafe_set: (t<'a, 'b>, int, 'a) => unit
    let sub: (t<'a, 'b>, ~offset: int, ~len: int) => t<'a, 'b>
  }
  module Bigarray: Bigarray
  let texSubImage2D: (
    ~context: contextT,
    ~target: int,
    ~level: int,
    ~xoffset: int,
    ~yoffset: int,
    ~width: int,
    ~height: int,
    ~format: int,
    ~type_: int,
    ~pixels: Bigarray.t<'a, 'b>,
  ) => unit
  let readPixels_RGBA: (
    ~context: contextT,
    ~x: int,
    ~y: int,
    ~width: int,
    ~height: int,
  ) => Bigarray.t<int, Bigarray.int8_unsigned_elt>
  type imageT
  let getImageWidth: imageT => int
  let getImageHeight: imageT => int
  type loadOptionT =
    | LoadAuto
    | LoadL
    | LoadLA
    | LoadRGB
    | LoadRGBA
  let loadImage: (
    ~filename: string,
    ~loadOption: loadOptionT=?,
    ~callback: option<imageT> => unit,
    unit,
  ) => unit
  let loadImageFromMemory: (
    ~data: string,
    ~loadOption: loadOptionT=?,
    ~callback: option<imageT> => unit,
    unit,
  ) => unit
  let texImage2DWithImage: (~context: contextT, ~target: int, ~level: int, ~image: imageT) => unit
  let uniform1i: (~context: contextT, ~location: uniformT, ~value: int) => unit
  let uniform1f: (~context: contextT, ~location: uniformT, ~value: float) => unit
  let uniform2f: (~context: contextT, ~location: uniformT, ~v1: float, ~v2: float) => unit
  let uniform3f: (
    ~context: contextT,
    ~location: uniformT,
    ~v1: float,
    ~v2: float,
    ~v3: float,
  ) => unit
  let uniform4f: (
    ~context: contextT,
    ~location: uniformT,
    ~v1: float,
    ~v2: float,
    ~v3: float,
    ~v4: float,
  ) => unit
  let texImage2D_RGBA: (
    ~context: contextT,
    ~target: int,
    ~level: int,
    ~width: int,
    ~height: int,
    ~border: int,
    ~data: Bigarray.t<'a, 'b>,
  ) => unit
  let texImage2D_null: (
    ~context: contextT,
    ~target: int,
    ~level: int,
    ~width: int,
    ~height: int,
  ) => unit
  let bufferData: (~context: contextT, ~target: int, ~data: Bigarray.t<'a, 'b>, ~usage: int) => unit
  let viewport: (~context: contextT, ~x: int, ~y: int, ~width: int, ~height: int) => unit
  let clear: (~context: contextT, ~mask: int) => unit
  let getUniformLocation: (~context: contextT, ~program: programT, ~name: string) => uniformT
  let getAttribLocation: (~context: contextT, ~program: programT, ~name: string) => attributeT
  let enableVertexAttribArray: (~context: contextT, ~attribute: attributeT) => unit
  let vertexAttribPointer: (
    ~context: contextT,
    ~attribute: attributeT,
    ~size: int,
    ~type_: int,
    ~normalize: bool,
    ~stride: int,
    ~offset: int,
  ) => unit
  let vertexAttribDivisor: (~context: contextT, ~attribute: attributeT, ~divisor: int) => unit
  module type Mat4T = {
    type t
    let to_array: t => array<float>
    let create: unit => t
    let identity: (~out: t) => unit
    let translate: (~out: t, ~matrix: t, ~vec: array<float>) => unit
    let scale: (~out: t, ~matrix: t, ~vec: array<float>) => unit
    let rotate: (~out: t, ~matrix: t, ~rad: float, ~vec: array<float>) => unit
    let ortho: (
      ~out: t,
      ~left: float,
      ~right: float,
      ~bottom: float,
      ~top: float,
      ~near: float,
      ~far: float,
    ) => unit
    let perspective: (~out: t, ~fovy: float, ~aspect: float, ~near: float, ~far: float) => unit
    let lookAt: (~out: t, ~eye: array<float>, ~center: array<float>, ~up: array<float>) => unit
  }
  module Mat4: Mat4T
  let uniformMatrix4fv: (~context: contextT, ~location: uniformT, ~value: Mat4.t) => unit
  type shaderParamsT =
    | Shader_delete_status
    | Compile_status
    | Shader_type
  type programParamsT =
    | Program_delete_status
    | Link_status
    | Validate_status
  let getProgramParameter: (
    ~context: contextT,
    ~program: programT,
    ~paramName: programParamsT,
  ) => int
  let getShaderParameter: (~context: contextT, ~shader: shaderT, ~paramName: shaderParamsT) => int
  let getShaderInfoLog: (~context: contextT, shaderT) => string
  let getProgramInfoLog: (~context: contextT, programT) => string
  let getShaderSource: (~context: contextT, shaderT) => string
  let drawArrays: (~context: contextT, ~mode: int, ~first: int, ~count: int) => unit
  let drawElements: (~context: contextT, ~mode: int, ~count: int, ~type_: int, ~offset: int) => unit
  let drawElementsInstanced: (
    ~context: contextT,
    ~mode: int,
    ~count: int,
    ~type_: int,
    ~indices: int,
    ~primcount: int,
  ) => unit
}
