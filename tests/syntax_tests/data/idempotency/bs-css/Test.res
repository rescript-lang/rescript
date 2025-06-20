let text = React.string

let fontItem = {
  open Css
  list{marginLeft(px(10)), paddingRight(px(10)), borderRight(px(1), solid, black)}
}

//let spin =
//  Css.(
//    keyframes([
//      (0, [transform(rotate(deg(0.)))]),
//      (100, [transform(rotate(deg(360.)))]),
//    ])
//  );

//let scaleAnimation =
//  Css.(
//    keyframes([
//      (0, [transform(scale(0.3, 0.3))]),
//      (100, [transform(scale(1.0, 1.0))]),
//    ])
//  );

let redBox = {
  open Css
  list{
    background(red),
    borderBottom(px(5), solid, black),
    width(px(50)),
    height(px(50)),
    margin(px(10)),
  }
}

let miniBox = {
  open Css
  list{border(px(2), solid, black), width(px(15)), height(px(15)), margin(px(1))}
}

// https://github.com/SentiaAnalytics/bs-css/issues/86
//let mergedStyles =
//  Css.(
//    merge([
//      style([padding(px(0)), fontSize(px(1))]),
//      style([padding(px(20)), fontSize(px(24)), color(blue)]),
//      style([media("(max-width: 768px)", [padding(px(10))])]),
//      style([
//        media("(max-width: 768px)", [fontSize(px(16)), color(red)]),
//      ]),
//    ])
//  );

let differentHeightLengths = {
  open Css
  [
    cm(1.),
    em(1.2),
    ex(1.2),
    mm(10.),
    pct(50.),
    pt(14),
    px(20),
    rem(2.0),
    vh(1.),
    vw(1.0),
    vmax(1.0),
    vmin(1.0),
    zero,
  ]->Array.map(x => {
    let className = style(\"@"(redBox, list{height(x)}))
    <div style=className key="x" />
  })
}->React.array

@react.component
let make = () =>
  <div
    style={
      open Css
      style(list{background(hex("f5f5f5"))})
    }>
    <Section name="angles">
      <div
        style={
          open Css
          style(\"@"(redBox, list{transform(rotate(deg(45.)))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{transform(rotate(rad(3.1415)))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{transform(rotate(grad(50.)))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{transform(rotate(turn(1. /. 3.)))}))
        }
      />
    </Section>
    <Section name="colors">
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(red)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(rgb(255, 0, 0))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(rgba(255, 0, 0, #num(0.5)))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(hsl(deg(255.), pct(100.), pct(50.)))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(hsla(deg(255.), pct(100.), pct(50.), #num(0.5)))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(hex("FF0000"))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(transparent)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{background(currentColor), color(blue)}))
        }
      />
    </Section>
    <Section name="Named colors">
      {React.array(
        {
          open Css
          [
            aliceblue,
            antiquewhite,
            aqua,
            aquamarine,
            azure,
            beige,
            bisque,
            black,
            blanchedalmond,
            blue,
            blueviolet,
            brown,
            burlywood,
            cadetblue,
            chartreuse,
            chocolate,
            coral,
            cornflowerblue,
            cornsilk,
            crimson,
            cyan,
            darkblue,
            darkcyan,
            darkgoldenrod,
            darkgray,
            darkgreen,
            darkgrey,
            darkkhaki,
            darkmagenta,
            darkolivegreen,
            darkorange,
            darkorchid,
            darkred,
            darksalmon,
            darkseagreen,
            darkslateblue,
            darkslategray,
            darkslategrey,
            darkturquoise,
            darkviolet,
            deeppink,
            deepskyblue,
            dimgray,
            dimgrey,
            dodgerblue,
            firebrick,
            floralwhite,
            forestgreen,
            fuchsia,
            gainsboro,
            ghostwhite,
            gold,
            goldenrod,
            gray,
            green,
            greenyellow,
            grey,
            honeydew,
            hotpink,
            indianred,
            indigo,
            ivory,
            khaki,
            lavender,
            lavenderblush,
            lawngreen,
            lemonchiffon,
            lightblue,
            lightcoral,
            lightcyan,
            lightgoldenrodyellow,
            lightgray,
            lightgreen,
            lightgrey,
            lightpink,
            lightsalmon,
            lightseagreen,
            lightskyblue,
            lightslategray,
            lightslategrey,
            lightsteelblue,
            lightyellow,
            lime,
            limegreen,
            linen,
            magenta,
            maroon,
            mediumaquamarine,
            mediumblue,
            mediumorchid,
            mediumpurple,
            mediumseagreen,
            mediumslateblue,
            mediumspringgreen,
            mediumturquoise,
            mediumvioletred,
            midnightblue,
            mintcream,
            mistyrose,
            moccasin,
            navajowhite,
            navy,
            oldlace,
            olive,
            olivedrab,
            orange,
            orangered,
            orchid,
            palegoldenrod,
            palegreen,
            paleturquoise,
            palevioletred,
            papayawhip,
            peachpuff,
            peru,
            pink,
            plum,
            powderblue,
            purple,
            rebeccapurple,
            red,
            rosybrown,
            royalblue,
            saddlebrown,
            salmon,
            sandybrown,
            seagreen,
            seashell,
            sienna,
            silver,
            skyblue,
            slateblue,
            slategray,
            slategrey,
            snow,
            springgreen,
            steelblue,
            tan,
            teal,
            thistle,
            tomato,
            transparent,
            turquoise,
            violet,
            wheat,
            white,
            whitesmoke,
            yellow,
            yellowgreen,
          ]
        }->Belt.Array.map(c =>
          <div
            style={
              open Css
              style(list{background(c), ...miniBox})
            }
          />
        ),
      )}
    </Section>
    <Section name="gradients">
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{background(linearGradient(deg(45.), list{(zero, red), (pct(100.), blue)}))},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                background(repeatingLinearGradient(deg(45.), list{(zero, red), (pct(10.), blue)})),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(redBox, list{background(radialGradient(list{(zero, red), (pct(100.), blue)}))}),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{background(repeatingRadialGradient(list{(zero, red), (pct(10.), blue)}))},
            ),
          )
        }
      />
    </Section>
    <Section name="lengths">
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{height(ch(1.2)), width(px(10)), maxHeight(pct(50.)), maxWidth(pct(100.))},
            ),
          )
        }
      />
      differentHeightLengths
    </Section>
    <Section name="calc">
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                height({
                  open Calc
                  pt(14) - px(10)
                }),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                height({
                  open Calc
                  cm(0.2) + mm(10.)
                }),
              },
            ),
          )
        }
      />
    </Section>
    <Section name="display">
      <div
        style={
          open Css
          style(\"@"(redBox, list{display(block)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{display(inline)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{display(inlineBlock)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{display(none)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{display(flexBox)}))
        }
      />
    </Section>
    <Section name="position">
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{position(absolute), top(zero), left(zero), right(zero), bottom(zero)},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{position(relative)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{position(fixed), bottom(px(10)), right(px(10))}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{position(static)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{position(sticky)}))
        }
      />
    </Section>
    <Section name="Padding & Margin">
      <div
        style={
          open Css
          style(\"@"(redBox, list{padding(px(10)), margin(px(10))}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                paddingLeft(px(10)),
                paddingRight(px(10)),
                paddingTop(px(10)),
                paddingBottom(px(10)),
                marginLeft(px(10)),
                marginRight(px(10)),
                marginTop(px(10)),
                marginBottom(px(10)),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{padding2(~v=px(10), ~h=px(20)), margin2(~v=px(10), ~h=px(20))}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                padding3(~top=px(10), ~h=px(20), ~bottom=px(1)),
                margin3(~top=px(10), ~h=px(20), ~bottom=px(2)),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                padding4(~top=px(10), ~bottom=px(1), ~left=px(5), ~right=px(15)),
                margin4(~top=px(10), ~bottom=px(1), ~left=px(5), ~right=px(15)),
              },
            ),
          )
        }
      />
    </Section>
    <Section name="grid">
      <div
        style={
          open Css
          style(list{
            width(pct(100.)),
            height(px(500)),
            display(grid),
            gridTemplateColumns(list{px(150), auto, px(150)}),
            gridTemplateRows(list{px(60), auto}),
          })
        }>
        <div
          style={
            open Css
            style(list{
              gridColumnStart(1),
              gridColumnEnd(4),
              background(red),
              gridRowStart(1),
              gridRowEnd(1),
            })
          }
        />
        <div
          style={
            open Css
            style(list{background(blue), gridColumn(1, 1), gridRow(2, 2)})
          }
        />
        <div
          style={
            open Css
            style(list{
              background(green),
              gridColumn(2, 2),
              gridRow(2, 2),
              display(inlineGrid),
              gridTemplateColumns(list{px(50), auto}),
              gridTemplateRows(list{px(40), auto}),
            })
          }>
          <div
            style={
              open Css
              style(list{background(yellow), gridRow(1, 1), gridColumn(2, 2)})
            }
          />
          <div
            style={
              open Css
              style(list{background(green), gridRow(1, 2), gridColumn(1, 1)})
            }
          />
          <div
            style={
              open Css
              style(list{background(purple), gridRow(2, 2), gridColumn(2, 2)})
            }
          />
        </div>
        <div
          style={
            open Css
            style(list{
              gridColumnStart(3),
              gridColumnEnd(3),
              background(blue),
              gridRowStart(2),
              gridRowEnd(2),
            })
          }
        />
      </div>
      <div
        style={
          open Css
          style(list{display(#grid), gridAutoFlow(#row)})
        }>
        <div
          style={
            open Css
            style(list{background(purple)})
          }>
          {text("grid auto direction row 1")}
        </div>
        <div
          style={
            open Css
            style(list{background(green)})
          }>
          {text("grid auto direction row 2")}
        </div>
      </div>
      <div
        style={
          open Css
          style(list{display(#grid), gridTemplateColumns(list{100->px, #repeat(#num(2), 60->px)})})
        }>
        <div
          style={
            open Css
            style(list{background(purple)})
          }>
          {text("Grid track repeat")}
        </div>
        <div
          style={
            open Css
            style(list{background(green)})
          }>
          {text("two times")}
        </div>
        <div
          style={
            open Css
            style(list{background(red)})
          }>
          {text("three times")}
        </div>
      </div>
      <div
        style={
          open Css
          style(list{display(#grid), gridAutoColumns(100->px)})
        }>
        <div
          style={
            open Css
            style(list{background(purple)})
          }>
          {text("Grid auto columns (100px)")}
        </div>
        <div
          style={
            open Css
            style(list{background(green)})
          }>
          {text("100px")}
        </div>
        <div
          style={
            open Css
            style(list{background(blue)})
          }>
          {text("100px")}
        </div>
      </div>
    </Section>
    <Section name="flexbox">
      <div
        style={
          open Css
          style(list{
            flexDirection(column),
            flexGrow(1.),
            alignItems(stretch),
            selector("& > *", list{marginBottom(px(10)), width(pct(100.))}),
          })
        }>
        <div
          style={
            open Css
            style(list{
              display(flexBox),
              flexDirection(row),
              background(gray),
              alignItems(flexStart),
              justifyContent(flexEnd),
            })
          }>
          <div
            style={
              open Css
              style(\"@"(redBox, list{order(1), flexGrow(1.), flexShrink(1.), flexBasis(auto)}))
            }
          />
          <div
            style={
              open Css
              style(\"@"(redBox, list{flex(none)}))
            }
          />
          <div
            style={
              open Css
              style(\"@"(redBox, list{order(1), flex3(~grow=1.5, ~shrink=0.8, ~basis=100->px)}))
            }
          />
          <div
            style={
              open Css
              style(\"@"(redBox, list{alignSelf(flexEnd)}))
            }
          />
          <div style={Css.style(redBox)} />
        </div>
        <div
          style={
            open Css
            style(list{
              display(flexBox),
              flexDirection(column),
              background(gray),
              alignItems(baseline),
              justifyContent(flexStart),
            })
          }>
          <div style={Css.style(redBox)} />
          <div style={Css.style(redBox)} />
          <div style={Css.style(redBox)} />
        </div>
        <div
          style={
            open Css
            style(list{
              display(flexBox),
              flexDirection(rowReverse),
              background(gray),
              alignItems(center),
              justifyContent(spaceBetween),
            })
          }>
          <div style={Css.style(redBox)} />
          <div
            style={
              open Css
              style(\"@"(redBox, list{height(px(50)), width(px(50))}))
            }
          />
          <div style={Css.style(redBox)} />
        </div>
        <div
          style={
            open Css
            style(list{
              display(flexBox),
              flexDirection(columnReverse),
              background(gray),
              alignItems(flexEnd),
              justifyContent(flexEnd),
            })
          }>
          <div style={Css.style(redBox)} />
          <div
            style={
              open Css
              style(\"@"(redBox, list{height(px(50)), width(px(50))}))
            }
          />
          <div style={Css.style(redBox)} />
        </div>
        <div
          style={
            open Css
            style(list{
              display(flexBox),
              flexDirection(row),
              background(gray),
              alignItems(stretch),
              justifyContent(spaceAround),
            })
          }>
          <div style={Css.style(redBox)} />
          <div
            style={
              open Css
              style(\"@"(redBox, list{height(px(50)), width(px(50))}))
            }
          />
          <div style={Css.style(redBox)} />
        </div>
        <div
          style={
            open Css
            style(list{
              display(flexBox),
              flexDirection(row),
              background(gray),
              alignItems(stretch),
              justifyContent(spaceEvenly),
            })
          }>
          <div style={Css.style(redBox)} />
          <div
            style={
              open Css
              style(\"@"(redBox, list{height(px(50)), width(px(50))}))
            }
          />
          <div style={Css.style(redBox)} />
        </div>
      </div>
    </Section>
    <Section name="float">
      <div
        style={
          open Css
          style(\"@"(redBox, list{float(#left), clear(#right)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{float(#right), clear(#left)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{float(none), clear(both)}))
        }
      />
    </Section>
    <Section name="overflow">
      <div
        style={
          open Css
          style(\"@"(redBox, list{overflow(hidden)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{overflow(visible)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{overflow(auto)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{overflow(scroll)}))
        }
      />
    </Section>
    <Section name="border">
      <div
        style={
          open Css
          style(\"@"(redBox, list{border(px(5), solid, blue), borderRadius(px(1000))}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                border(px(5), solid, green),
                borderTopRightRadius(px(1000)),
                borderTopLeftRadius(px(1000)),
                borderBottomRightRadius(px(1000)),
                borderBottomLeftRadius(px(1000)),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                borderTop(px(5), dashed, hex("FFF")),
                borderRight(px(5), dotted, rgb(0, 0, 0)),
                borderBottom(px(5), none, green),
                borderLeft(px(5), solid, blue),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{borderWidth(px(5)), borderStyle(solid), borderColor(blue)}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(redBox, list{borderTopWidth(px(5)), borderTopStyle(solid), borderTopColor(blue)}),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{borderBottomWidth(px(5)), borderBottomStyle(solid), borderBottomColor(blue)},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{borderLeftWidth(px(5)), borderLeftStyle(solid), borderLeftColor(blue)},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{borderRightWidth(px(5)), borderRightStyle(solid), borderRightColor(blue)},
            ),
          )
        }
      />
    </Section>
    <Section name="background">
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                background(red),
                backgroundAttachment(scroll),
                backgroundClip(borderBox),
                backgroundOrigin(borderBox),
                backgroundPosition(#hv(pct(50.), pct(50.))),
                backgroundRepeat(noRepeat),
                backgroundSize(size(px(100), px(100))),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                background(radialGradient(list{(zero, red), (pct(10.), blue)})),
                backgroundAttachment(fixed),
                backgroundClip(contentBox),
                backgroundOrigin(contentBox),
                backgroundRepeat(repeat),
                backgroundSize(auto),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                background(url("./img-29.jpg")),
                backgroundAttachment(local),
                backgroundClip(paddingBox),
                backgroundOrigin(paddingBox),
                backgroundRepeat(repeatX),
                backgroundSize(cover),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                backgroundColor(rgb(0, 0, 255)),
                backgroundImage(
                  linearGradient(
                    deg(45.),
                    list{(zero, green), (pct(50.), red), (pct(100.), yellow)},
                  ),
                ),
                backgroundRepeat(repeatY),
                backgroundSize(contain),
              },
            ),
          )
        }
      />
    </Section>
    <Section name="cursor">
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#auto)}))
        }>
        {text("auto")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#default)}))
        }>
        {text("default")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#none)}))
        }>
        {text("none")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#contextMenu)}))
        }>
        {text("context menu")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#help)}))
        }>
        {text("help")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#pointer)}))
        }>
        {text("pointer")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#progress)}))
        }>
        {text("progress")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#wait)}))
        }>
        {text("wait")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#cell)}))
        }>
        {text("cell")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#crosshair)}))
        }>
        {text("crosshair")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#text)}))
        }>
        {text("text")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#verticalText)}))
        }>
        {text("vert text")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#alias)}))
        }>
        {text("alias")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#copy)}))
        }>
        {text("copy")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#move)}))
        }>
        {text("move")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#noDrop)}))
        }>
        {text("no drop")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#notAllowed)}))
        }>
        {text("not allowed")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#grab)}))
        }>
        {text("grab")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#grabbing)}))
        }>
        {text("grabbing")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#allScroll)}))
        }>
        {text("all scroll")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#colResize)}))
        }>
        {text("col resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#rowResize)}))
        }>
        {text("row resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#nResize)}))
        }>
        {text("n resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#eResize)}))
        }>
        {text("e resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#sResize)}))
        }>
        {text("s resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#wResize)}))
        }>
        {text("w resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#neResize)}))
        }>
        {text("ne resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#nwResize)}))
        }>
        {text("nw resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#seResize)}))
        }>
        {text("se resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#swResize)}))
        }>
        {text("sw resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#ewResize)}))
        }>
        {text("ew resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#nsResize)}))
        }>
        {text("ns resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#neswResize)}))
        }>
        {text("nesw resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#nwseResize)}))
        }>
        {text("nwse resize")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#zoomIn)}))
        }>
        {text("zoom in")}
      </div>
      <div
        style={
          open Css
          style(\"@"(redBox, list{cursor(#zoomOut)}))
        }>
        {text("zoom out")}
      </div>
    </Section>
    <Section name="list">
      <ul>
        <li
          style={
            open Css
            style(list{listStyle(#disc, inside, none)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#circle)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#square)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#decimal)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#lowerAlpha)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#upperAlpha)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#lowerGreek)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#lowerLatin)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#upperLatin)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#lowerRoman)})
          }
        />
        <li
          style={
            open Css
            style(list{listStyleType(#upperRoman)})
          }
        />
        <li
          style={
            open Css
            style(list{
              listStyleType(#disc),
              listStylePosition(inside),
              listStyleImage(url("./facebook.png")),
            })
          }
        />
      </ul>
    </Section>
    <Section name="outline">
      <div
        style={
          open Css
          style(\"@"(redBox, list{outline(px(5), #double, green)}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                outlineStyle(solid),
                outlineWidth(px(5)),
                outlineColor(green),
                outlineOffset(px(5)),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{outline(px(5), #double, red)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{outline(px(5), #ridge, red)}))
        }
      />
    </Section>
    <Section name="transform">
      <div
        style={
          open Css
          style(\"@"(redBox, list{opacity(0.5)}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{perspective(px(500)), transform(rotate(deg(10.)))}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{transforms(list{translate(px(10), pct(10.)), skew(deg(10.), deg(10.))})},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                transform(rotate(deg(19.))),
                transformOrigin(pct(50.), pct(50.)),
                transformStyle(#preserve3d),
                perspective(px(900)),
                perspectiveOrigin(pct(10.), pct(10.)),
              },
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                transform(translate(px(10), pct(10.))),
                transformOrigin3d(px(10), px(10), px(10)),
              },
            ),
          )
        }
      />
    </Section>
    <Section name="transition">
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{transition(~duration=300, ~delay=300, ~timingFunction=easeInOut, "transform")},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                transitionProperty("height"),
                transitionDelay(300),
                transitionDuration(300),
                transitionTimingFunction(linear),
              },
            ),
          )
        }
      />
    </Section>
    <Section name="text">
      <p
        style={
          open Css
          style(list{
            color(black),
            fontFamilies(list{#custom("Helvetica"), #sansSerif}),
            fontSize(pt(18)),
            fontVariant(#smallCaps),
            fontStyle(italic),
            fontWeight(#num(300)),
            letterSpacing(px(3)),
            lineHeight(#abs(2.)),
            textAlign(#left),
            textDecoration(underline),
            textDecorationColor(pink),
            textDecorationStyle(wavy),
            textIndent(px(10)),
            textOverflow(clip),
            textShadow(Shadow.text(~y=px(3), ~blur=px(2), black)),
            textTransform(capitalize),
            verticalAlign(sub),
            whiteSpace(normal),
            wordBreak(breakAll),
            wordSpacing(px(20)),
            wordWrap(breakWord),
          })
        }>
        {text(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        )}
      </p>
      <h2
        style={
          open Css
          style(list{width(pct(100.))})
        }>
        {text("Named Font weights")}
      </h2>
      <span
        style={
          open Css
          style(list{fontWeight(thin), paddingRight(px(10)), borderRight(px(1), solid, black)})
        }>
        {text("thin")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(extraLight), ...fontItem})
        }>
        {text("extra light")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(light), ...fontItem})
        }>
        {text("light")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(normal), ...fontItem})
        }>
        {text("normal")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(medium), ...fontItem})
        }>
        {text("medium")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(semiBold), ...fontItem})
        }>
        {text("semiBold")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(bold), ...fontItem})
        }>
        {text("bold")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(extraBold), ...fontItem})
        }>
        {text("extra bold")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(#black), ...fontItem})
        }>
        {text("black")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(lighter), ...fontItem})
        }>
        {text("lighter")}
      </span>
      <span
        style={
          open Css
          style(list{fontWeight(bolder), ...fontItem})
        }>
        {text("bolder")}
      </span>
    </Section>
    //    <Section name="animation">
    //      <div
    //        style=Css.(
    //          style(
    //            redBox
    //            @ [
    //              animation(
    //                ~duration=300,
    //                ~delay=300,
    //                ~direction=reverse,
    //                ~timingFunction=linear,
    //                ~fillMode=forwards,
    //                ~playState=running,
    //                ~iterationCount=infinite,
    //                spin,
    //              ),
    //            ],
    //          )
    //        )
    //      />
    //      <div
    //        style=Css.(
    //          style(
    //            redBox
    //            @ [
    //              animations([
    //                Animation.shorthand(
    //                  ~duration=300,
    //                  ~iterationCount=infinite,
    //                  spin,
    //                ),
    //                Animation.shorthand(
    //                  ~duration=300,
    //                  ~iterationCount=infinite,
    //                  scaleAnimation,
    //                ),
    //              ]),
    //            ],
    //          )
    //        )
    //      />
    //      <div
    //        style=Css.(
    //          style(
    //            redBox
    //            @ [
    //              animationName(spin),
    //              animationTimingFunction(easeIn),
    //              animationDuration(300),
    //              animationDelay(300),
    //              animationDirection(normal),
    //              animationFillMode(backwards),
    //              animationPlayState(paused),
    //              animationIterationCount(count(5)),
    //            ],
    //          )
    //        )
    //      />
    //    </Section>
    <Section name="cascading">
      {text("inherit")}
      <div
        style={
          open Css
          style(list{
            display(inherit_),
            position(inherit_),
            fontSize(inherit_),
            fontStyle(inherit_),
            lineHeight(inherit_),
          })
        }
      />
      {text("unset")}
      <div
        style={
          open Css
          style(list{
            display(unset),
            position(unset),
            fontSize(unset),
            fontStyle(unset),
            lineHeight(unset),
          })
        }
      />
    </Section>
    <Section name="columns">
      <p
        style={
          open Css
          style(list{columnCount(count(10))})
        }>
        {text("This is a bunch of text split into columns
             using the CSS `column-count` property. The text
             is equally distributed over the columns.")}
      </p>
    </Section>
    <Section name="resize">
      <textarea
        style={
          open Css
          style(list{resize(none)})
        }>
        {"Can't resize textarea"->text}
      </textarea>
      <div
        style={
          open Css
          style(list{marginLeft(px(20)), overflow(scroll), resize(horizontal)})
        }>
        {"Resizable div (horizontal)"->text}
      </div>
      <div
        style={
          open Css
          style(list{marginLeft(px(20)), overflow(scroll), resize(vertical)})
        }>
        {"Resizable div (vertical)"->text}
      </div>
    </Section>
    <Section name="content">
      <div
        style={
          open Css
          style(list{
            position(relative),
            after(list{
              contentRule(#none),
              position(absolute),
              top(zero),
              left(zero),
              width(pct(100.)),
              height(pct(100.)),
              border(px(1), solid, black),
            }),
          })
        }>
        {text("none")}
      </div>
      <div
        style={
          open Css
          style(list{
            position(relative),
            after(list{
              contentRule(#normal),
              position(absolute),
              top(zero),
              left(zero),
              width(pct(100.)),
              height(pct(100.)),
              border(px(1), solid, black),
            }),
          })
        }>
        {text("normal")}
      </div>
      <div
        style={
          open Css
          style(list{position(relative), marginLeft(px(20))})
        }>
        <a
          href="https://github.com/SentiaAnalytics/bs-css"
          style={
            open Css
            style(list{
              before(list{
                contentRule(#text("external ")),
                backgroundColor(red),
                display(inlineBlock),
                flexBasis(content /* for test */),
              }),
            })
          }>
          {text("link")}
        </a>
      </div>
      <div
        style={
          open Css
          style(list{
            position(relative),
            marginLeft(px(20)),
            after(list{
              contentRule(#text("")),
              position(absolute),
              top(zero),
              left(zero),
              width(pct(100.)),
              height(pct(100.)),
              border(px(1), solid, black),
            }),
          })
        }>
        {text("empty content")}
      </div>
      <div
        style={
          open Css
          style(list{
            position(relative),
            marginLeft(px(20)),
            paddingLeft(px(20)),
            after(list{
              contentRule(#url("https://via.placeholder.com/18")),
              position(absolute),
              top(zero),
              left(zero),
              width(px(18)),
              height(px(18)),
              border(px(1), solid, black),
            }),
          })
        }>
        {text("url")}
      </div>
      <div
        style={
          open Css
          style(list{
            marginLeft(px(20)),
            counterReset(Types.CounterReset.reset("foo", ~value=1)),
            before(list{contentRule(Types.Counter.counter("foo")), border(px(1), solid, black)}),
          })
        }>
        {text("counter")}
      </div>
      <div
        style={
          open Css
          style(list{counterReset(Types.CounterReset.reset("foo", ~value=1)), marginLeft(px(20))})
        }>
        <div
          style={
            open Css
            style(list{
              counterReset(Types.CounterReset.reset("foo", ~value=2)),
              before(list{
                contentRule(Types.Counters.counters("foo", ~separator="@", ~style=#upperRoman)),
                border(px(1), solid, black),
              }),
            })
          }>
          {text("counters")}
        </div>
      </div>
      <div
        style={
          open Css
          style(list{
            marginLeft(px(20)),
            before(list{contentRule(#attr("class")), border(px(1), solid, black)}),
          })
        }>
        {text("attr")}
      </div>
      <div
        style={
          open Css
          style(list{
            marginLeft(px(20)),
            before(list{
              contentRule(
                Types.Gradient.linearGradient(deg(45.), list{(zero, red), (pct(100.), blue)}),
              ),
              border(px(1), solid, black),
              display(#inlineBlock),
              height(px(18)),
              width(px(18)),
            }),
          })
        }>
        {text("linear gradient")}
      </div>
      <div
        style={
          open Css
          style(list{
            marginLeft(px(20)),
            before(list{
              contentRules(list{#openQuote, #text("foo"), #closeQuote}),
              border(px(1), solid, black),
            }),
          })
        }>
        {text("contents (quotes)")}
      </div>
    </Section>
    <Section name="insertRule, the ultimate escape hatch"> <div className="raw-css" /> </Section>
    //    <Section name="merging style names">
    //      <button className=mergedStyles> {text("Merged")} </button>
    //    </Section>
    <Section name="filter">
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#blur(#px(10))})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#brightness(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#contrast(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{filter(list{#dropShadow(#px(3), #px(3), #px(3), #rgb(200, 100, 100))})},
            ),
          )
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#grayscale(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#hueRotate(#deg(180.))})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#invert(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#opacity(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#saturate(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#sepia(50.)})}))
        }
      />
      <div
        style={
          open Css
          style(
            \"@"(
              redBox,
              list{
                filter(list{
                  #sepia(50.),
                  #saturate(50.),
                  #dropShadow(#px(3), #px(3), #px(3), #rgb(200, 100, 100)),
                }),
              },
            ),
          )
        }
      />
      <svg
        height="0"
        style={
          open Css
          style(list{display(#none)})
        }>
        <filter id="f1"> <feGaussianBlur stdDeviation="3" /> </filter>
      </svg>
      <div
        style={
          open Css
          style(\"@"(redBox, list{filter(list{#url("#f1")})}))
        }
      />
    </Section>
    <Section name="direction">
      <Section name="ltr">
        <div
          style={
            open Css
            style(list{direction(#ltr), display(#flex)})
          }>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"1"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"2"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"3"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"4"->text}
          </div>
        </div>
      </Section>
      <Section name="rtl">
        <div
          style={
            open Css
            style(list{direction(#rtl), display(#flex)})
          }>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"1"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"2"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"3"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"4"->text}
          </div>
        </div>
      </Section>
      <Section name="unset">
        <div
          style={
            open Css
            style(list{direction(#unset), display(#flex)})
          }>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"1"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"2"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"3"->text}
          </div>
          <div
            style={
              open Css
              style(redBox)
            }>
            {"4"->text}
          </div>
        </div>
      </Section>
    </Section>
    <Section name="object-fit">
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#fill)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#contain)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#cover)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#none)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#scaleDown)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#inherit_)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#initial)}))
        }
        src="./img-29.jpg"
      />
      <img
        style={
          open Css
          style(\"@"(redBox, list{objectFit(#unset)}))
        }
        src="./img-29.jpg"
      />
    </Section>
  </div>
