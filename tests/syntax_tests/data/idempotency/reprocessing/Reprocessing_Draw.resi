@@ocaml.doc(" # The Draw module
 *
 * This is where all the fancy things happen.
 *
 * ```reason;shared(sandbox)
 * [@val] external sandboxCanvasId: string = \"\";
 * [@val] external sandboxCanvas: 'canvas = \"\";
 * [@val] external containerDiv: 'node = \"\";
 * [@send] external addEventListener: ('node, string, 'eventT => unit) => unit = \"addEventListener\";
 * let id = sandboxCanvasId;
 * addEventListener(containerDiv, \"mouseleave\", (_) => Reprocessing.playPause(id, false) |> ignore);
 * addEventListener(containerDiv, \"mouseenter\", (_) => Reprocessing.playPause(id, true) |> ignore);
 * Reprocessing.setScreenId(sandboxCanvasId);
 * ```
 *
 * ```reason;shared(draw);use(sandbox)
 * open Reprocessing;
 * open Draw;
 * run(~setup=env => Env.size(~width=200, ~height=200, env), ~draw=((), env) => {
 * %{code}%
 * }, ())
 * ```
 *
 * ```canvas;use(draw)
 * fill(Constants.red, env);
 * rect(~pos=Env.mouse(env), ~width=5, ~height=5, env);
 * ```
 *
 * ## Core drawing operations
 *
 * @doc rect, rectf, clear, background, line, linef, ellipse, ellipsef, quad, quadf, pixel, pixelf, triangle, trianglef, bexier, arc, arcf, curve
 *
 * ## Style operations
 *
 * @doc fill, noFill, stroke, noStroke, strokeWeight, strokeCap, tint, noTint, pushStyle, popStyle
 *
 * ## Text
 *
 * @doc loadFont, text
 *
 * ## Images
 *
 * @doc loadImage, image, subImage, subImagef
 *
 * ## Matrix/transform operations
 *
 * @doc translate, rotate, scale, shear, pushMatrix, popMatrix, rectMode
 *
 ")

@ocaml.doc(" Specifies an amount to displace objects within the display window.
   * The dx parameter specifies left/right translation, the dy parameter
   * specifies up/down translation.
   *
   * Transformations are cumulative and apply to everything that happens
   * after and subsequent calls to the function accumulates the effect.
   * For example, calling `translate dx::50 dy::0 env` and then
   * `translate dx::20 dy::0 env` is the same as `translate dx::70 dy::0 env`.
   * If `translate` is called within `draw`, the transformation is reset
   * when the loop begins again. This function can be further controlled
   * by using `pushMatrix` and `popMatrix`.
 ")
let translate: (~x: float, ~y: float, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Rotates the amount specified by the angle parameter. Angles must be
 * specified in radians (values from 0 to two_pi), or they can be converted
 * from degrees to radians with the `radians` function.

 * The coordinates are always rotated around their relative position to the
 * origin. Positive numbers rotate objects in a clockwise direction and
 * negative numbers rotate in the couterclockwise direction. Transformations
 * apply to everything that happens afterward, and subsequent calls to the
 * function compound the effect. For example, calling
 * `rotate Constants.pi/2. env` once and then calling `rotate Constants.pi/2. env`
 * a second time is the same as a single `rotate Constants.pi env`. All
 * tranformations are reset when `draw` begins again.

 * Technically, `rotate` multiplies the current transformation matrix by a
 * rotation matrix. This function can be further controlled by `pushMatrix`
 * and `popMatrix`.
 ")
let rotate: (float, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" The scale() function increases or decreases the size of a shape by expanding
 * and contracting vertices.
 ")
let scale: (~x: float, ~y: float, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" The shear() function shears the matrix along the axes the amount
 * specified by the angle parameters. Angles should be specified in radians
 * (values from 0 to PI*2) or converted to radians with the Utils.radians()
 * function.
 ")
let shear: (~x: float, ~y: float, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Sets the color used to fill shapes.")
let fill: (Reprocessing_Types.Types.colorT, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Disables filling geometry. If both `noStroke` and `noFill` are called,
   * nothing will be drawn to the screen.
 ")
let noFill: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" Sets the fill value for displaying images. Images can be tinted to specified colors
   * or made transparent by including an alpha value.
 ")
let tint: (Reprocessing_Types.Types.colorT, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Removes the current fill value for displaying images and reverts to displaying
   * images with their original hues.
 ")
let noTint: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" Sets the color used to draw lines and borders around shapes. ")
let stroke: (Reprocessing_Types.Types.colorT, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Disables drawing the stroke (outline). If both noStroke() and noFill()
   * are called, nothing will be drawn to the screen.
 ")
let noStroke: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" Sets the width of the stroke used for lines, points, and the border around
   * shapes. All widths are set in units of pixels.
 ")
let strokeWeight: (int, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Sets the style for rendering line endings. These ends are either squared,
  * extended, or rounded.
 ")
let strokeCap: (Reprocessing_Types.Types.strokeCapT, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Sets the style to modify the location from which rectangles are drawn by
  * changing the way in which parameters given to rect() and rectf() are intepreted.
  *
  * The default mode is rectMode(Corner), which interprets the position of rect()
  * as the upper-left corner of the shape, while the third and fourth parameters
  * are its width and height.
  *
  * rectMode(Center) interprets the position of rect() as the shape's center point,
  * while the third and fourth parameters are its width and height.
  *
  * rectMode(Radius) also uses the position of rect() as the shape's center point,
  * but uses the third and fourth parameters to specify half of the shapes's width
  * and height.
 ")
let rectMode: (Reprocessing_Types.Types.rectModeT, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" The `pushStyle` function saves the current style settings and `popStyle`
   * restores the prior settings. Note that these functions are always used
   * together. They allow you to change the style settings and later return to
   * what you had. When a new style is started with `pushStyle`, it builds on the
   * current style information. The `pushStyle` and `popStyle` functions can be
   * embedded to provide more control.
   *
   * The style information controlled by the following functions are included in
   * the style: fill, stroke, strokeWeight
 ")
let pushStyle: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" The `pushStyle` function saves the current style settings and
   * `popStyle` restores the prior settings; these functions are always used
   * together. They allow you to change the style settings and later return to
   * what you had. When a new style is started with `pushStyle`, it builds on the
   * current style information. The `pushStyle` and `popStyle` functions can be
   * embedded to provide more control.
   *
   * The style information controlled by the following functions are included in
   * the style: fill, stroke, strokeWeight
 ")
let popStyle: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" Pushes the current transformation matrix onto the matrix stack. Understanding pushMatrix() and popMatrix()
  * requires understanding the concept of a matrix stack. The pushMatrix() function saves the current coordinate
  * system to the stack and popMatrix() restores the prior coordinate system. pushMatrix() and popMatrix() are
  * used in conjuction with the other transformation methods and may be embedded to control the scope of
  * the transformations.
 ")
let pushMatrix: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" Pops the current transformation matrix off the matrix stack. Understanding pushing and popping requires
  * understanding the concept of a matrix stack. The pushMatrix() function saves the current coordinate system to
  * the stack and popMatrix() restores the prior coordinate system. pushMatrix() and popMatrix() are used in
  * conjuction with the other transformation methods and may be embedded to control the scope of the transformations.
 ")
let popMatrix: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" Loads an image and returns a handle to it. This will lazily load and
   * attempting to draw an image that has not finished loading will result
   * in nothing being drawn.
   * In general, all images should be loaded in `setup` to preload them at
   * the start of the program.
   * If isPixel is set to true, then when scaling the image, it will use
   * GL_NEAREST (you want this setting if your image is meant to look
   * pixelated)
 ")
let loadImage: (
  ~filename: string,
  ~isPixel: bool=?,
  Reprocessing_Types.Types.glEnvT,
) => Reprocessing_Types.Types.imageT

@ocaml.doc(" The `imagef` function draws an image to the display window.
   * The image should be loaded using the `loadImage` function.
   * The image is displayed at its original size unless width and
   * height are optionally specified.
 ")
let imagef: (
  Reprocessing_Types.Types.imageT,
  ~pos: (float, float),
  ~width: float=?,
  ~height: float=?,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" The `image` function draws an image to the display window.
   * The image should be loaded using the `loadImage` function.
   * The image is displayed at its original size unless width and
   * height are optionally specified.
   * 
   * This is the same as `imagef` but takes in integers instead of floats for convenience.
 ")
let image: (
  Reprocessing_Types.Types.imageT,
  ~pos: (int, int),
  ~width: int=?,
  ~height: int=?,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" The `subImage` function draws a section of an image to the
   * display window. The image should be loaded using the
   * `loadImage` function. The image is displayed at the size
   * specified by width and height.  texPos, texWidth, and
   * texHeight describe the section of the full image that
   * should be drawn.
   *
   * This function is useful for a spritesheet-style of
   * drawing strategy.
 ")
let subImage: (
  Reprocessing_Types.Types.imageT,
  ~pos: (int, int),
  ~width: int,
  ~height: int,
  ~texPos: (int, int),
  ~texWidth: int,
  ~texHeight: int,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" The `subImagef` function draws a section of an image to the
   * display window. The image should be loaded using the
   * `loadImage` function. The image is displayed at the size
   * specified by width and height.  texPos, texWidth, and
   * texHeight describe the section of the full image that
   * should be drawn.
   *
   * This function is useful for a spritesheet-style of
   * drawing strategy.
 ")
let subImagef: (
  Reprocessing_Types.Types.imageT,
  ~pos: (float, float),
  ~width: float,
  ~height: float,
  ~texPos: (int, int),
  ~texWidth: int,
  ~texHeight: int,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Draws a rectangle to the screen. A rectangle is a four-sided shape with
   * every angle at ninety degrees.
 ")
let rectf: (
  ~pos: (float, float),
  ~width: float,
  ~height: float,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Draws a rectangle to the screen. A rectangle is a four-sided shape with
   * every angle at ninety degrees.
   *
   * This is the same as `rectf` but takes in integers instead of floats for convenience.
 ")
let rect: (~pos: (int, int), ~width: int, ~height: int, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Draws a curved line on the screen. The first parameter specifies
   * the beginning control point and the last parameter specifies the ending
   * control point. The middle parameters specify the start and stop of the curve.
 ")
let curve: (
  (float, float),
  (float, float),
  (float, float),
  (float, float),
  Reprocessing_Common.glEnv,
) => unit

@ocaml.doc(" Draws a line (a direct path between two points) to the screen.
   * To color a line, use the `stroke` function. A line cannot be filled,
   * therefore the `fill` function will not affect the color of a line. Lines are
   * drawn with a width of one pixel by default, but this can be changed with
   * the `strokeWeight` function.
 ")
let linef: (~p1: (float, float), ~p2: (float, float), Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Draws a line (a direct path between two points) to the screen.
   * To color a line, use the `stroke` function. A line cannot be filled,
   * therefore the `fill` function will not affect the color of a line. Lines are
   * drawn with a width of one pixel by default, but this can be changed with
   * the `strokeWeight` function.
   *
   * This is the same as `linef`, but converts all its integer arguments to floats
   * as a convenience.
 ")
let line: (~p1: (int, int), ~p2: (int, int), Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Draws an ellipse (oval) to the screen. An ellipse with equal width and
   * height is a circle.
 ")
let ellipsef: (
  ~center: (float, float),
  ~radx: float,
  ~rady: float,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Draws an ellipse (oval) to the screen. An ellipse with equal width and
   * height is a circle.
   *
   * This is the same as `ellipsef`, but converts all its integer arguments to
   * floats as a convenience.
 ")
let ellipse: (~center: (int, int), ~radx: int, ~rady: int, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc("  A quad is a quadrilateral, a four sided polygon. It is similar to a
   * rectangle, but the angles between its edges are not constrained to ninety
   * degrees. The parameter p1 sets the first vertex and the subsequest points
   * should proceed clockwise or counter-clockwise around the defined shape.
 ")
let quadf: (
  ~p1: (float, float),
  ~p2: (float, float),
  ~p3: (float, float),
  ~p4: (float, float),
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc("  A quad is a quadrilateral, a four sided polygon. It is similar to a
   * rectangle, but the angles between its edges are not constrained to ninety
   * degrees. The parameter p1 sets the first vertex and the subsequest points
   * should proceed clockwise or counter-clockwise around the defined shape.
   *
   * This is the same as `quadf`, but converts all its integer arguments to
   * floats as a convenience.
 ")
let quad: (
  ~p1: (int, int),
  ~p2: (int, int),
  ~p3: (int, int),
  ~p4: (int, int),
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Adds a single point with a radius defined by strokeWeight ")
let pixelf: (
  ~pos: (float, float),
  ~color: Reprocessing_Types.Types.colorT,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Adds a single point with a radius defined by strokeWeight
   *
   * This is the same as `pixelf`, but converts all its integer arguments to
   * floats as a convenience.
 ")
let pixel: (
  ~pos: (int, int),
  ~color: Reprocessing_Types.Types.colorT,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" A triangle is a plane created by connecting three points. ")
let trianglef: (
  ~p1: (float, float),
  ~p2: (float, float),
  ~p3: (float, float),
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" A triangle is a plane created by connecting three points.
   *
   * This is the same as `trianglef`, but converts all its integer arguments to
   * floats as a convenience.
 ")
let triangle: (
  ~p1: (int, int),
  ~p2: (int, int),
  ~p3: (int, int),
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Draws a Bezier curve on the screen. These curves are defined by a
   * series of anchor and control points. The parameter p1 specifies the
   * first anchor point and the last parameter specifies the other anchor
   * point. The middle parameters p2 and p3 specify the control points
   * which define the shape of the curve. Bezier curves were developed
   * by French engineer Pierre Bezier.
 ")
let bezier: (
  ~p1: (float, float),
  ~p2: (float, float),
  ~p3: (float, float),
  ~p4: (float, float),
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Draws an arc to the screen. Arcs are drawn along the outer edge of an
   * ellipse defined by the center, radx, and rady parameters. Use the
   * start and stop parameters to specify the angles (in radians) at which
   * to draw the arc. isPie defines whether or not lines should be drawn to
   * the center at the start and stop points of the arc rather than simply
   * connecting the points.  If isOpen is true, no line will be drawn other
   * than the arc between start and stop.
 ")
let arcf: (
  ~center: (float, float),
  ~radx: float,
  ~rady: float,
  ~start: float,
  ~stop: float,
  ~isOpen: bool,
  ~isPie: bool,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Draws an arc to the screen. Arcs are drawn along the outer edge of an
   * ellipse defined by the center, radx, and rady parameters. Use the
   * start and stop parameters to specify the angles (in radians) at which
   * to draw the arc. isPie defines whether or not lines should be drawn to
   * the center at the start and stop points of the arc rather than simply
   * connecting the points.  If isOpen is true, no line will be drawn other
   * than the arc between start and stop.
   *
   * This is the same as `arcf`, but converts all its integer arguments to
   * floats as a convenience.
 ")
let arc: (
  ~center: (int, int),
  ~radx: int,
  ~rady: int,
  ~start: float,
  ~stop: float,
  ~isOpen: bool,
  ~isPie: bool,
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Loads a font and returns a handle to it. This will lazily load and
   * attempting to draw an font that has not finished loading will result
   * in nothing being drawn.
   * In general, all fonts should be loaded in `setup` to preload them at
   * the start of the program.
   * If isPixel is set to true, then when scaling the font, it will use
   * GL_NEAREST (you want this setting if your font is meant to look
   * pixelated)
 ")
let loadFont: (
  ~filename: string,
  ~isPixel: bool=?,
  Reprocessing_Types.Types.glEnvT,
) => Reprocessing_Font.fontT

@ocaml.doc(" Draws text to the screen.
 *
 * To use a font, use [loadFont](#value-loadFont) in your `setup()` function. If you don't specify a font, the default font is used.
 *
 * ```canvas;use(draw)
 * Draw.background(Constants.white, env);
 * text(~body=\"Hello folks\", ~pos=(5, 40), env);
 * ```
 ")
let text: (
  ~font: Reprocessing_Font.fontT=?,
  ~body: string,
  ~pos: (int, int),
  Reprocessing_Types.Types.glEnvT,
) => unit

@ocaml.doc(" Calculates width of text using a specific font.
  * The font should be loaded using the `loadFont` function.
 ")
let textWidth: (
  ~font: Reprocessing_Font.fontT=?,
  ~body: string,
  Reprocessing_Types.Types.glEnvT,
) => int

@ocaml.doc(" Clears the entire screen. Normally, background is used for this purpose,
  * clear will have different results in web and native.
 ")
let clear: Reprocessing_Types.Types.glEnvT => unit

@ocaml.doc(" The `background` function sets the color used for the background of the
   * Processing window. The default background is black. This function is
   * typically used within `draw` to clear the display window at the beginning of
   * each frame, but it can be used inside `setup` to set the background on the
   * first frame of animation or if the backgound need only be set once.
 ")
let background: (Reprocessing_Types.Types.colorT, Reprocessing_Types.Types.glEnvT) => unit

@ocaml.doc(" Makes draw calls inside the callback draw to the given image instead of to the screen.
  * The callback is called with a new env which will make all draw calls done inside the callback
  * draw on the image instead of the main canvas.
  * This is useful to basically cache draw calls onto an image which can then be drawn to the
  * screen very cheaply at any point after.
 ")
let withImage: (
  Reprocessing_Types.Types.imageT,
  Reprocessing_Types.Types.glEnvT,
  Reprocessing_Types.Types.glEnvT => unit,
) => unit

@ocaml.doc(" Returns a new image which can be drawn to.
 ")
let createImage: (
  ~width: int,
  ~height: int,
  Reprocessing_Types.Types.glEnvT,
) => Reprocessing_Types.Types.imageT

@ocaml.doc(" Checks whether the given image has been drawn to since created or since last time clearImage
  * was called. This is useful when using images as a caching mechanism, to check if the image is
  * up to date.
 ")
let isImageDrawnTo: Reprocessing_Types.Types.imageT => bool

@ocaml.doc(" Clears image such that `isImageDrawnTo` returns false.
 ")
let clearImage: (Reprocessing_Types.Types.imageT, Reprocessing_Types.Types.glEnvT) => unit
