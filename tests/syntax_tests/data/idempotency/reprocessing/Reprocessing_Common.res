open Reasongl

module Constants = RGLConstants

type strokeCapT =
  | Round
  | Square
  | Project

type rectModeT =
  | Corner
  | Center
  | Radius

type glState = Gl.Window.t

type glCamera = {projectionMatrix: Gl.Mat4.t}

type colorT = {
  r: float,
  g: float,
  b: float,
  a: float,
}

type styleT = {
  strokeColor: option<colorT>,
  strokeWeight: int,
  strokeCap: strokeCapT,
  fillColor: option<colorT>,
  tintColor: option<colorT>,
  rectMode: rectModeT,
}

type mouseT = {
  mutable pos: (int, int),
  mutable prevPos: (int, int),
  mutable pressed: bool,
}

module KeySet = Set.Make({
  type t = Reprocessing_Events.keycodeT
  let compare = compare
})

type keyboardT = {
  mutable keyCode: Reprocessing_Events.keycodeT,
  mutable pressed: KeySet.t,
  mutable released: KeySet.t,
  mutable down: KeySet.t,
}

type frameT = {
  count: int,
  rate: int,
  deltaTime: float,
}

type sizeT = {
  mutable height: int,
  mutable width: int,
  mutable resizeable: bool,
}

let circularBufferSize = 6 * 10000

let vertexSize = 8

type _imageT = {
  framebuffer: option<Gl.framebufferT>,
  texture: Gl.textureT,
  /* both redundant with tgls, maybe make Gl expose functions to get the width/height */
  height: int,
  width: int,
}

type imageT = {
  mutable glData: option<_imageT>,
  mutable drawnTo: bool,
}

type soundLoadStatusT =
  | Loading
  | ShouldPlay(float, bool)
  | Loaded(Gl.Audio.t)

type soundT = ref<soundLoadStatusT>

type batchT = {
  vertexArray: Gl.Bigarray.t<float, Gl.Bigarray.float32_elt>,
  elementArray: Gl.Bigarray.t<int, Gl.Bigarray.int16_unsigned_elt>,
  mutable vertexPtr: int,
  mutable elementPtr: int,
  mutable currTex: option<Gl.textureT>,
  nullTex: Gl.textureT,
}

type glEnv = {
  camera: glCamera,
  window: Gl.Window.t,
  gl: Gl.contextT,
  vertexBuffer: Gl.bufferT,
  elementBuffer: Gl.bufferT,
  aVertexColor: Gl.attributeT,
  aTextureCoord: Gl.attributeT,
  aVertexPosition: Gl.attributeT,
  pMatrixUniform: Gl.uniformT,
  uSampler: Gl.uniformT,
  batch: batchT,
  keyboard: keyboardT,
  mouse: mouseT,
  mutable style: styleT,
  mutable styleStack: list<styleT>,
  mutable frame: frameT,
  mutable matrix: array<float>,
  mutable matrixStack: list<array<float>>,
  size: sizeT,
}

module type ReProcessorT = {
  type t
  let run: (
    ~setup: glEnv => 'a,
    ~draw: ('a, glEnv) => 'a=?,
    ~mouseMove: ('a, glEnv) => 'a=?,
    ~mouseDragged: ('a, glEnv) => 'a=?,
    ~mouseDown: ('a, glEnv) => 'a=?,
    ~mouseUp: ('a, glEnv) => 'a=?,
    ~keyPressed: ('a, glEnv) => 'a=?,
    ~keyReleased: ('a, glEnv) => 'a=?,
    ~keyTyped: ('a, glEnv) => 'a=?,
    unit,
  ) => unit
}

module Stream = {
  type t = (string, int)
  let empty = list{}
  let peekch = ((str, i): t): option<char> =>
    if i < String.length(str) {
      Some(String.get(str, i))
    } else {
      None
    }
  let popch = ((str, i): t): t => (str, i + 1)
  let peekn = ((str, i), len) =>
    if i + len < String.length(str) {
      Some(String.sub(str, i, len))
    } else {
      None
    }
  let skipWhite = ((str, i): t): t => {
    let len = String.length(str)
    let rec loop = n =>
      if n >= len {
        (str, n)
      } else if String.get(str, n) === ' ' {
        loop(n + 1)
      } else {
        (str, n)
      }
    loop(i)
  }
  let popn = ((str, i), len) => (str, i + len)
  let match_ = (stream, matchstr) => {
    let len = String.length(matchstr)
    switch peekn(stream, len) {
    | Some(peek) if peek == matchstr => popn(stream, len)
    | Some(peek) =>
      failwith("Could not match '" ++ (matchstr ++ ("', got '" ++ (peek ++ "' instead."))))
    | None => failwith("Could not match " ++ matchstr)
    }
  }
  let charsRemaining = ((str, i)) => String.length(str) - i
  let create = (str: string): t => (str, 0)
}

let read = (name: string) => {
  let ic = open_in(name)
  let try_read = () =>
    switch input_line(ic) {
    | exception End_of_file => None
    | x => Some(x)
    }
  let rec loop = acc =>
    switch try_read() {
    | Some(s) => loop(list{String.make(1, '\n'), s, ...acc})
    | None =>
      close_in(ic)
      List.rev(acc)
    }
  loop(list{})->String.concat("")
}

let append_char = (s: string, c: char): string => s ++ String.make(1, c)

let rec split = (stream, sep, accstr, acc) =>
  switch Stream.peekch(stream) {
  | Some(c) if c == sep => split(Stream.popch(stream), sep, "", list{accstr, ...acc})
  | Some(c) => split(Stream.popch(stream), sep, append_char(accstr, c), acc)
  | None => List.rev(list{accstr, ...acc})
  }

let split = (str, ~sep) => split(Stream.create(str), sep, "", list{})
