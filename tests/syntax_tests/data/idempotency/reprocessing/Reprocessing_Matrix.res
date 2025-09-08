let identity = [1., 0., 0., 0., 1., 0., 0., 0., 1.]

let createIdentity = () => [1., 0., 0., 0., 1., 0., 0., 0., 1.]

let createTranslation = (dx, dy) => [1., 0., dx, 0., 1., dy, 0., 0., 1.]

let createRotation = theta => [cos(theta), -.sin(theta), 0., sin(theta), cos(theta), 0., 0., 0., 1.]

let createScaling = (sx, sy) => [sx, 0., 0., 0., sy, 0., 0., 0., 1.]

let createShearing = (sx, sy) => [1., sx, 0., sy, 1., 0., 0., 0., 1.]

let copyInto = (~src, ~dst) => {
  dst[0] = src[0]
  dst[1] = src[1]
  dst[2] = src[2]
  dst[3] = src[3]
  dst[4] = src[4]
  dst[5] = src[5]
  dst[6] = src[6]
  dst[7] = src[7]
  dst[8] = src[8]
}

/* **
 [0 1 2]   [a b c]   [a0 + d1 + g2, b0 + e1 + h2, c0 + f1 + i2]
 [3 4 5] * [d e f] = [a3 + d4 + g5, b3 + e4 + h5, c3 + f4 + i5]
 [6 7 8]   [g h i]   [a6 + d7 + g8, b6 + e7 + h8, c6 + f7 + i8]
 */
let matmatmul = (mat1: array<float>, mat2: array<float>) =>
  switch (mat1, mat2) {
  | ([m0, m1, m2, m3, m4, m5, m6, m7, m8], [ma, mb, mc, md, me, mf, mg, mh, mi]) =>
    mat1[0] = ma *. m0 +. md *. m1 +. mg *. m2
    mat1[1] = mb *. m0 +. me *. m1 +. mh *. m2
    mat1[2] = mc *. m0 +. mf *. m1 +. mi *. m2
    mat1[3] = ma *. m3 +. md *. m4 +. mg *. m5
    mat1[4] = mb *. m3 +. me *. m4 +. mh *. m5
    mat1[5] = mc *. m3 +. mf *. m4 +. mi *. m5
    mat1[6] = ma *. m6 +. md *. m7 +. mg *. m8
    mat1[7] = mb *. m6 +. me *. m7 +. mh *. m8
    mat1[8] = mc *. m6 +. mf *. m7 +. mi *. m8
  | _ => assert(false)
  }

/* **
 [0 1 2]   [a]   [a0 + b1 + c2]
 [3 4 5] * [b] = [a3 + b4 + c5]
 [6 7 8]   [c]   [a6 + b7 + c8]
 */
let matvecmul = (m, v) => {
  let a = v[0]
  let b = v[1]
  let c = v[2]
  v[0] = a *. m[0] +. b *. m[1] +. c *. m[2]
  v[1] = a *. m[3] +. b *. m[4] +. c *. m[5]
  v[2] = a *. m[6] +. b *. m[7] +. c *. m[8]
}

/* **
 [0 1 2]   [x]   [x0 + y1 + 2]
 [3 4 5] * [y] = [x3 + y4 + 5]
 [6 7 8]   [1]   [ who cares ]
 */
let matptmul = (m, (x, y)) => (x *. m[0] +. y *. m[1] +. m[2], x *. m[3] +. y *. m[4] +. m[5])

/* **
 Invert a matrix
 https://www.geometrictools.com/Documentation/LaplaceExpansionTheorem.pdf
 */
let matinv = mat =>
  switch mat {
  | [m00, m01, m02, m10, m11, m12, m20, m21, m22] =>
    let det =
      m00 *. m11 *. m22 +.
      m01 *. m12 *. m20 +.
      m02 *. m10 *. m21 -.
      m00 *. m12 *. m21 -.
      m01 *. m10 *. m22 -.
      m02 *. m11 *. m20
    if det == 0. {
      invalid_arg("The current transform matrix cannot be inverted")
    }
    let invdet = 1. /. det
    let adj00 = m11 *. m22 -. m12 *. m21
    let adj01 = -.(m01 *. m22 -. m02 *. m21)
    let adj02 = m01 *. m12 -. m02 *. m11
    let adj10 = -.(m10 *. m22 -. m12 *. m20)
    let adj11 = m00 *. m22 -. m02 *. m20
    let adj12 = -.(m00 *. m12 -. m02 *. m10)
    let adj20 = m10 *. m21 -. m11 *. m20
    let adj21 = -.(m00 *. m21 -. m01 *. m20)
    let adj22 = m00 *. m11 -. m01 *. m10
    [
      invdet *. adj00,
      invdet *. adj01,
      invdet *. adj02,
      invdet *. adj10,
      invdet *. adj11,
      invdet *. adj12,
      invdet *. adj20,
      invdet *. adj21,
      invdet *. adj22,
    ]
  | _ => assert(false)
  }
