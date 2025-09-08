type glT
type programT
type shaderT
type bufferT

/* ClearBufferMask */
let _DEPTH_BUFFER_BIT: int = 256
let _STENCIL_BUFFER_BIT: int = 1024
let _COLOR_BUFFER_BIT: int = 16384

/* BeginMode */
let _POINTS: int = 0
let _LINES: int = 1
let _LINE_LOOP: int = 2
let _LINE_STRIP: int = 3
let _TRIANGLES: int = 4
let _TRIANGLE_STRIP: int = 5
let _TRIANGLE_FAN: int = 6

/* TEXTURE_2D */
let _CULL_FACE: int = 2884
let _BLEND: int = 3042
let _DITHER: int = 3024
let _STENCIL_TEST: int = 2960
let _DEPTH_TEST: int = 2929
let _SCISSOR_TEST: int = 3089
let _POLYGON_OFFSET_FILL: int = 32823
let _SAMPLE_ALPHA_TO_COVERAGE: int = 32926
let _SAMPLE_COVERAGE: int = 32928

/* BlendingFactorDest */
let _ZERO: int = 0
let _ONE: int = 1
let _SRC_COLOR: int = 768
let _ONE_MINUS_SRC_COLOR: int = 769
let _SRC_ALPHA: int = 770
let _ONE_MINUS_SRC_ALPHA: int = 771
let _DST_ALPHA: int = 772
let _ONE_MINUS_DST_ALPHA: int = 773

/* DataType */
let _BYTE: int = 5120
let _UNSIGNED_BYTE: int = 5121
let _SHORT: int = 5122
let _UNSIGNED_SHORT: int = 5123
let _INT: int = 5124
let _UNSIGNED_INT: int = 5125
let _FLOAT: int = 5126

/* CullFaceMode */
let _FRONT: int = 1028
let _BACK: int = 1029
let _FRONT_AND_BACK: int = 1032

/* Shaders */
let _FRAGMENT_SHADER: int = 35632
let _VERTEX_SHADER: int = 35633

/* Buffer Objects */
let _ARRAY_BUFFER: int = 34962
let _ELEMENT_ARRAY_BUFFER: int = 34963
let _ARRAY_BUFFER_BINDING: int = 34964
let _ELEMENT_ARRAY_BUFFER_BINDING: int = 34965
let _STREAM_DRAW: int = 35040
let _STATIC_DRAW: int = 35044
let _DYNAMIC_DRAW: int = 35048

/* void clear(GLbitfield mask); */
@send external clear: (glT, int) => unit = "clear"
/* void clearColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha); */
@send external clearColor: (glT, float, float, float, float) => unit = "clearColor"
/* void enable(GLenum cap); */
@send external enable: (glT, int) => unit = "enable"
/* void disable(GLenum cap); */
@send external disable: (glT, int) => unit = "disable"
/* void blendFunc(GLenum sfactor, GLenum dfactor); */
@send external blendFunc: (glT, int, int) => unit = "blendFunc"
/* void cullFace(GLenum mode); */
@send external cullFace: (glT, int) => unit = "cullFace"
@send external createBuffer: glT => bufferT = "createBuffer"
@send external deleteBuffer: (glT, bufferT) => unit = "deleteBuffer"
@send external bindBuffer: (glT, int, bufferT) => unit = "bindBuffer"
@send external bufferData: (glT, int, Js.Typed_array.Uint16Array.t, int) => unit = "bufferData"
@send
external bufferFloatData: (glT, int, Js.Typed_array.Float32Array.t, int) => unit = "bufferData"
@send external createProgram: glT => programT = "createProgram"
@send external linkProgram: (glT, programT) => unit = "linkProgram"
@send external useProgram: (glT, programT) => unit = "useProgram"
@send external getProgramInfoLog: (glT, programT) => string = "getProgramInfoLog"
@send external bindAttribLocation: (glT, programT, int, string) => unit = "bindAttribLocation"
@send external createShader: (glT, int) => shaderT = "createShader"
@send external shaderSource: (glT, shaderT, string) => unit = "shaderSource"
@send external compileShader: (glT, shaderT) => unit = "compileShader"
@send external attachShader: (glT, programT, shaderT) => unit = "attachShader"
@send external getShaderInfoLog: (glT, shaderT) => string = "getShaderInfoLog"
/* void drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset); */
@send external drawElements: (glT, int, int, int, int) => unit = "drawElements"
/* void enableVertexAttribArray(GLuint index); */
@send external enableVertexAttribArray: (glT, int) => unit = "enableVertexAttribArray"
/* void vertexAttribPointer(GLuint indx, GLint size, GLenum type,
 GLboolean normalized, GLsizei stride, GLintptr offset); */
@send
external vertexAttribPointer: (glT, int, int, int, bool, int, int) => unit = "vertexAttribPointer"
/* GLint gl.getAttribLocation(program, name); */
@send external getAttribLocation: (glT, programT, string) => int = "getAttribLocation"
/* void gl.drawArrays(mode, first, count); */
@send external drawArrays: (glT, int, int, int) => unit = "drawArrays"
