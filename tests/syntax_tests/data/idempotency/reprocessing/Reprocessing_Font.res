open Reasongl

@ocaml.doc(" # Fonts & Text
 *
 * This module is internal - you probably shouldn't depend on any implementation details.
 *
 * Take a look at [Draw.text](./Reprocessing_Draw.html#value-text) for information on drawing text.
 *
 ")
module Internal = Reprocessing_Internal

open Reprocessing_Common

/* Mostly to suppress the bsc warning */
let intCompare = (i: int, j: int) => i == j ? 0 : i < j ? -1 : 1

module Font = {
  module IntMap = Map.Make({
    type t = int
    let compare = intCompare
  })
  module IntPairMap = Map.Make({
    type t = (int, int)
    let compare = ((a1, a2), (b1, b2)) => {
      let first = intCompare(a1, b1)
      if first != 0 {
        first
      } else {
        intCompare(a2, b2)
      }
    }
  })
  type charT = {
    x: float,
    y: float,
    width: float,
    height: float,
    xoffset: float,
    yoffset: float,
    xadvance: float,
  }
  type internalType = {
    chars: IntMap.t<charT>,
    kerning: IntPairMap.t<float>,
    res: float,
    lineHeight: float,
    image: imageT,
  }
  type t = ref<option<internalType>>
  let defaultFont = ref(None)
  let rec parse_num = (stream: Stream.t, acc): (Stream.t, float) =>
    switch Stream.peekch(stream) {
    | Some('-' as c)
    | Some('.' as c)
    | Some('0' .. '9' as c) =>
      parse_num(Stream.popch(stream), append_char(acc, c))
    | _ =>
      try (stream, float_of_string(acc)) catch {
      | _ => failwith("Could not parse number [" ++ (acc ++ "]."))
      }
    }
  let parse_num = stream => parse_num(stream, "")
  let rec parse_string = (stream: Stream.t, acc: string): (Stream.t, string) =>
    switch Stream.peekch(stream) {
    | Some('"') => (Stream.popch(stream), acc)
    | Some(c) => parse_string(Stream.popch(stream), append_char(acc, c))
    | None => failwith("Unterminated string.")
    }
  let parse_string = stream => parse_string(stream, "")
  let rec pop_line = stream =>
    switch Stream.peekch(stream) {
    | Some('\n') => Stream.popch(stream)
    | Some(_) => pop_line(Stream.popch(stream))
    | None => failwith("could not pop line")
    }
  let rec parse_char_fmt = (stream, num, map) =>
    if num < 0 {
      (stream, map)
    } else if Stream.peekn(stream, 4) != Some("char") {
      prerr_string("Warning: encountered end of char sequence early when loading font.\n")
      (stream, map)
    } else {
      let stream = Stream.match_(stream, "char id=")
      let (stream, char_id) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "x=")
      let (stream, x) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "y=")
      let (stream, y) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "width=")
      let (stream, width) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "height=")
      let (stream, height) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "xoffset=")
      let (stream, xoffset) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "yoffset=")
      let (stream, yoffset) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "xadvance=")
      let (stream, xadvance) = parse_num(stream)
      let stream = pop_line(stream)
      let new_map = IntMap.add(
        int_of_float(char_id),
        {
          x: x,
          y: y,
          width: width,
          height: height,
          xoffset: xoffset,
          yoffset: yoffset,
          xadvance: xadvance,
        },
        map,
      )
      parse_char_fmt(stream, num - 1, new_map)
    }
  let rec parse_kern_fmt = (stream, num, map) =>
    if num == 0 {
      (stream, map)
    } else {
      let stream = Stream.match_(stream, "kerning first=")
      let (stream, first) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "second=")
      let (stream, second) = parse_num(stream)
      let stream = Stream.match_(Stream.skipWhite(stream), "amount=")
      let (stream, amount) = parse_num(stream)
      let stream = pop_line(stream)
      let new_map = IntPairMap.add((int_of_float(first), int_of_float(second)), amount, map)
      parse_kern_fmt(stream, num - 1, new_map)
    }
  let replaceFilename = (path, filename) => {
    let splitStr = Reprocessing_Common.split(path, ~sep='/')
    let revLst = List.rev(splitStr)
    let newRevLst = switch revLst {
    | list{_, ...tl} => list{filename, ...tl}
    | list{} => list{}
    }
    let newLst = List.rev(newRevLst)
    String.concat("/", newLst)
  }
  let getCharMapAndKernMap = str => {
    let stream = Stream.create(str ++ "\n")
    let (stream, res) = switch Stream.peekn(stream, 9) {
    | Some("info res=") =>
      let stream = Stream.match_(stream, "info res=")
      parse_num(stream)
    | Some(_)
    | None => (stream, 1.)
    }
    let stream = pop_line(stream)
    let stream = Stream.match_(stream, "common lineHeight=")
    let (stream, lineHeight) = parse_num(stream)
    let stream = pop_line(stream)
    let stream = Stream.match_(stream, "page id=0 file=\"")
    let (stream, filename) = parse_string(stream)
    let stream = pop_line(stream)
    let stream = Stream.match_(stream, "chars count=")
    let (stream, num_chars) = parse_num(stream)
    let stream = pop_line(stream)
    let (stream, char_map) = parse_char_fmt(stream, int_of_float(num_chars), IntMap.empty)
    let stream = Stream.match_(stream, "kernings count=")
    let (stream, num_kerns) = parse_num(stream)
    let stream = pop_line(stream)
    let (_, kern_map) = parse_kern_fmt(stream, int_of_float(num_kerns), IntPairMap.empty)
    (char_map, kern_map, filename, res, lineHeight)
  }
  let parseFontFormat = (env, path, isPixel) => {
    let ret = ref(None)
    Gl.File.readFile(~filename=path, ~cb=str => {
      let (char_map, kern_map, filename, res, lineHeight) = getCharMapAndKernMap(str)
      let img_filename = replaceFilename(path, filename)
      ret :=
        Some({
          chars: char_map,
          kerning: kern_map,
          res: res,
          lineHeight: lineHeight,
          image: Internal.loadImage(env, img_filename, isPixel),
        })
    })
    ret
  }
  let getChar = (fnt, ch) => {
    let code = Char.code(ch)
    try IntMap.find(code, fnt.chars) catch {
    | _ => failwith("Could not find character " ++ (string_of_int(code) ++ " in font."))
    }
  }
  let drawChar = (env: glEnv, fnt, image, ch: char, last: option<char>, x, y) => {
    let c = getChar(fnt, ch)
    let kernAmount = switch last {
    | Some(lastCh) =>
      let first = Char.code(lastCh)
      let second = Char.code(ch)
      try IntPairMap.find((first, second), fnt.kerning) catch {
      | _ => 0.
      }
    | None => 0.
    }
    switch image {
    | Some(img) =>
      Internal.drawImageWithMatrixf(
        img,
        ~x=x +. (c.xoffset +. kernAmount) /. fnt.res,
        ~y=y +. c.yoffset /. fnt.res,
        ~width=c.width /. fnt.res,
        ~height=c.height /. fnt.res,
        ~subx=int_of_float(c.x),
        ~suby=int_of_float(c.y),
        ~subw=int_of_float(c.width),
        ~subh=int_of_float(c.height),
        env,
      )
      (c.xadvance +. kernAmount) /. fnt.res
    | None => (c.xadvance +. kernAmount) /. fnt.res
    }
  }
  let drawString = (env: glEnv, fnt, str: string, x, y) => {
    let fnt = switch fnt {
    | None => defaultFont
    | Some(fnt) => fnt
    }
    switch fnt.contents {
    | None => ()
    | Some(fnt) =>
      switch fnt.image.glData {
      | Some(img) =>
        let xOffset = ref(x)
        let yOffset = ref(y)
        let lastChar = ref(None)
        String.iter(c =>
          switch c {
          | '\r' =>
            xOffset := x
            lastChar := None
          | '\n' =>
            xOffset := x
            yOffset := yOffset.contents +. fnt.lineHeight
            lastChar := None
          | c =>
            let advance = drawChar(
              env,
              fnt,
              Some(img),
              c,
              lastChar.contents,
              xOffset.contents,
              yOffset.contents,
            )
            xOffset := xOffset.contents +. advance
            lastChar := Some(c)
          }
        , str)
      | None => print_endline("loading font.")
      }
    }
  }
  let calcStringWidth = (env, fnt, str: string) => {
    let fnt = switch fnt {
    | None => defaultFont
    | Some(fnt) => fnt
    }
    switch fnt.contents {
    | None => 0.
    | Some(fnt) =>
      let offset = ref(0.)
      let maxOffset = ref(0.)
      let lastChar = ref(None)
      String.iter(c =>
        switch c {
        | '\r' | '\n' =>
          maxOffset := max(maxOffset.contents, offset.contents)
          offset := 0.
          lastChar := None
        | c =>
          offset :=
            offset.contents +. drawChar(env, fnt, None, c, lastChar.contents, offset.contents, 0.)
          lastChar := Some(c)
        }
      , str)
      max(maxOffset.contents, offset.contents)
    }
  }
  let loadDefaultFont = env => {
    let data = switch Reprocessing_DefaultFont.read("font.fnt") {
    | None => failwith("Failed to load default font. This shouldn't happen.")
    | Some(data) => data
    }
    let imageData = switch Reprocessing_DefaultFont.read("font.png") {
    | None => failwith("Failed to load default font image. This shouldn't happen")
    | Some(data) => data
    }
    let (char_map, kern_map, _, res, lineHeight) = getCharMapAndKernMap(data)
    defaultFont :=
      Some({
        chars: char_map,
        kerning: kern_map,
        res: res,
        lineHeight: lineHeight,
        image: Internal.loadImageFromMemory(env, imageData, false),
      })
  }
}

type fontT = ref<option<Font.internalType>>
