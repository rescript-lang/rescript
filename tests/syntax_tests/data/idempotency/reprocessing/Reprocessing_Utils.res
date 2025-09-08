open Reprocessing_Common

let foi = float_of_int

let lookup_table: ref<array<int>> = ref([])

/* Calculation Functions */
let round = i => floor(i +. 0.5)

let sq = x => x * x

let rec pow = (~base, ~exp) =>
  switch exp {
  | 0 => 1
  | 1 => base
  | n =>
    let b = pow(~base, ~exp=n / 2)
    b *
    b * if mod(n, 2) == 0 {
      1
    } else {
      base
    }
  }

let constrain = (~amt, ~low, ~high) => max(min(amt, high), low)

let remapf = (~value, ~low1, ~high1, ~low2, ~high2) =>
  low2 +. (high2 -. low2) *. ((value -. low1) /. (high1 -. low1))

let remap = (~value, ~low1, ~high1, ~low2, ~high2) =>
  int_of_float(
    remapf(
      ~value=foi(value),
      ~low1=foi(low1),
      ~high1=foi(high1),
      ~low2=foi(low2),
      ~high2=foi(high2),
    ),
  )

let norm = (~value, ~low, ~high) => remapf(~value, ~low1=low, ~high1=high, ~low2=0., ~high2=1.)

let randomf = (~min, ~max) => Random.float(max -. min) +. min

let random = (~min, ~max) => Random.int(max - min) + min

let randomSeed = seed => Random.init(seed)

let randomGaussian = () => {
  let u1 = ref(0.0)
  let u2 = ref(0.0)
  while u1.contents <= min_float {
    u1 := Random.float(1.0)
    u2 := Random.float(1.0)
  }
  sqrt(-2.0 *. log(u1.contents)) *. cos(Reprocessing_Constants.two_pi *. u2.contents)
}

let lerpf = (~low, ~high) => remapf(~low1=0., ~high1=1., ~low2=low, ~high2=high)

let lerp = (~low, ~high, ~value) => int_of_float(lerpf(~low=foi(low), ~high=foi(high), ~value))

let distf = (~p1 as (x1: float, y1: float), ~p2 as (x2: float, y2: float)) => {
  let dx = x2 -. x1
  let dy = y2 -. y1
  sqrt(dx *. dx +. dy *. dy)
}

let dist = (~p1 as (x1, y1), ~p2 as (x2, y2)) =>
  distf(~p1=(foi(x1), foi(y1)), ~p2=(foi(x2), foi(y2)))

let magf = vec => distf(~p1=(0., 0.), ~p2=vec)

let mag = vec => dist(~p1=(0, 0), ~p2=vec)

let lerpColor = (~low, ~high, ~value) => {
  r: lerpf(~low=low.r, ~high=high.r, ~value),
  g: lerpf(~low=low.g, ~high=high.g, ~value),
  b: lerpf(~low=low.b, ~high=high.b, ~value),
  a: lerpf(~low=low.a, ~high=high.a, ~value),
}

let degrees = x => 180.0 /. Reprocessing_Constants.pi *. x

let radians = x => Reprocessing_Constants.pi /. 180.0 *. x

let noise = (x, y, z) => {
  let p = lookup_table.contents
  let fade = t => t *. t *. t *. (t *. (t *. 6.0 -. 15.0) +. 10.0)
  let grad = (hash, x, y, z) =>
    switch land(hash, 15) {
    | 0 => x +. y
    | 1 => -.x +. y
    | 2 => x -. y
    | 3 => -.x -. y
    | 4 => x +. z
    | 5 => -.x +. z
    | 6 => x -. z
    | 7 => -.x -. z
    | 8 => y +. z
    | 9 => -.y +. z
    | 10 => y -. z
    | 11 => -.y -. z
    | 12 => y +. x
    | 13 => -.y +. z
    | 14 => y -. x
    | 15 => -.y -. z
    | _ => 0.0
    }
  let xi = land(int_of_float(x), 255)
  let yi = land(int_of_float(y), 255)
  let zi = land(int_of_float(z), 255)
  let xf = x -. floor(x)
  let yf = y -. floor(y)
  let zf = z -. floor(z)
  let u = fade(xf)
  let v = fade(yf)
  let w = fade(zf)
  let aaa = p[p[p[xi] + yi] + zi]
  let aba = p[p[p[xi] + (yi + 1)] + zi]
  let aab = p[p[p[xi] + yi] + (zi + 1)]
  let abb = p[p[p[xi] + (yi + 1)] + (zi + 1)]
  let baa = p[p[p[xi + 1] + yi] + zi]
  let bba = p[p[p[xi + 1] + (yi + 1)] + zi]
  let bab = p[p[p[xi + 1] + yi] + (zi + 1)]
  let bbb = p[p[p[xi + 1] + (yi + 1)] + (zi + 1)]
  let x1 = lerpf(~low=grad(aaa, xf, yf, zf), ~high=grad(baa, xf -. 1.0, yf, zf), ~value=u)
  let x2 = lerpf(
    ~low=grad(aba, xf, yf -. 1.0, zf),
    ~high=grad(bba, xf -. 1.0, yf -. 1.0, zf),
    ~value=u,
  )
  let y1 = lerpf(~low=x1, ~high=x2, ~value=v)
  let x1 = lerpf(
    ~low=grad(aab, xf, yf, zf -. 1.0),
    ~high=grad(bab, xf -. 1.0, yf, zf -. 1.0),
    ~value=u,
  )
  let x2 = lerpf(
    ~low=grad(abb, xf, yf -. 1.0, zf -. 1.0),
    ~high=grad(bbb, xf -. 1.0, yf -. 1.0, zf -. 1.0),
    ~value=u,
  )
  let y2 = lerpf(~low=x1, ~high=x2, ~value=v)
  (lerpf(~low=y1, ~high=y2, ~value=w) +. 1.0) /. 2.0
}

let shuffle = array => {
  let array = Array.copy(array)
  let length = Array.length(array)
  for i in 0 to 256 - 1 {
    let j = Random.int(length - i)
    let tmp = array[i]
    array[i] = array[i + j]
    array[i + j] = tmp
  }
  array
}

let noiseSeed = seed => {
  let state = Random.get_state()
  Random.init(seed)
  let array = Array.make(256, 0)
  let array = Array.mapi((i, _) => i, array)
  let array = shuffle(array)
  let double_array = Array.append(array, array)
  lookup_table := double_array
  Random.set_state(state)
}

let split = Reprocessing_Common.split

let color = (~r, ~g, ~b, ~a): colorT => {
  r: foi(r) /. 255.,
  g: foi(g) /. 255.,
  b: foi(b) /. 255.,
  a: foi(a) /. 255.,
}

let colorf = (~r, ~g, ~b, ~a): colorT => {r: r, g: g, b: b, a: a}

let intersectRectCircle = (
  ~rectPos as (rx, ry),
  ~rectW,
  ~rectH,
  ~circlePos as (cx, cy),
  ~circleRad,
) => {
  let halfW = rectW /. 2.
  let halfH = rectH /. 2.
  let cdistX = abs_float(cx -. (rx +. halfW))
  let cdistY = abs_float(cy -. (ry +. halfH))
  if cdistX > halfW +. circleRad || cdistY > halfH +. circleRad {
    false
  } else if cdistX <= halfW || cdistY <= halfH {
    true
  } else {
    let cornerDistSq = (cdistX -. halfW) ** 2. +. (cdistY -. halfH) ** 2.
    cornerDistSq <= circleRad ** 2.
  }
}

let intersectRectRect = (
  ~rect1Pos as (rx1, ry1),
  ~rect1W,
  ~rect1H,
  ~rect2Pos as (rx2, ry2),
  ~rect2W,
  ~rect2H,
) => !(rx2 > rx1 +. rect1W || (rx2 +. rect2W < rx1 || (ry2 > ry1 +. rect1H || ry2 +. rect2H < ry1)))
