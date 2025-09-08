open Css

insertRule(".raw-css { display:block; background-color: green; width: 50px; height: 50px; }")

let fontItem = list{marginLeft(px(10)), paddingRight(px(10)), borderRight(px(1), solid, black)}

let spin = keyframes(list{
  (0, list{transform(rotate(deg(0.)))}),
  (100, list{transform(rotate(deg(360.)))}),
})

let scaleAnimation = keyframes(list{
  (0, list{transform(scale(0.3, 0.3))}),
  (100, list{transform(scale(1.0, 1.0))}),
})

let redBox = list{
  background(red),
  borderBottom(px(5), solid, black),
  width(px(50)),
  height(px(50)),
  margin(px(10)),
}

let miniBox = list{border(px(2), solid, black), width(px(15)), height(px(15)), margin(px(1))}

// https://github.com/SentiaAnalytics/bs-css/issues/86
let mergedStyles = merge(list{
  style(list{padding(px(0)), fontSize(px(1))}),
  style(list{padding(px(20)), fontSize(px(24)), color(blue)}),
  style(list{media("(max-width: 768px)", list{padding(px(10))})}),
  style(list{media("(max-width: 768px)", list{fontSize(px(16)), color(red)})}),
})

let differentHeightLengths =
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
  ]
  ->Belt.Array.map(x => {
    let className = style(\"@"(redBox, list{height(x)}))
    <div className key=className />
  })
  ->React.array

@react.component
let make = () =>
  <div className={style(list{background(hex("f5f5f5"))})}>
    <Section name="angles">
      <div className={style(\"@"(redBox, list{transform(rotate(deg(45.)))}))} />
      <div className={style(\"@"(redBox, list{transform(rotate(rad(3.1415)))}))} />
      <div className={style(\"@"(redBox, list{transform(rotate(grad(50.)))}))} />
      <div className={style(\"@"(redBox, list{transform(rotate(turn(1. /. 3.)))}))} />
    </Section>
    <Section name="colors">
      <div className={style(\"@"(redBox, list{background(red)}))} />
      <div className={style(\"@"(redBox, list{background(rgb(255, 0, 0))}))} />
      <div className={style(\"@"(redBox, list{background(rgba(255, 0, 0, #num(0.5)))}))} />
      <div className={style(\"@"(redBox, list{background(hsl(deg(255.), pct(100.), pct(50.)))}))} />
      <div
        className={style(
          \"@"(redBox, list{background(hsla(deg(255.), pct(100.), pct(50.), #num(0.5)))}),
        )}
      />
      <div className={style(\"@"(redBox, list{background(hex("FF0000"))}))} />
      <div className={style(\"@"(redBox, list{background(transparent)}))} />
      <div className={style(\"@"(redBox, list{background(currentColor), color(blue)}))} />
    </Section>
    <Section name="Named colors">
      {React.array(
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
          Css.tan,
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
        ]->Belt.Array.map(c => <div className={style(list{background(c), ...miniBox})} />),
      )}
    </Section>
    <Section name="gradients">
      <div
        className={style(
          \"@"(
            redBox,
            list{background(linearGradient(deg(45.), list{(zero, red), (pct(100.), blue)}))},
          ),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              background(repeatingLinearGradient(deg(45.), list{(zero, red), (pct(10.), blue)})),
            },
          ),
        )}
      />
      <div
        className={style(
          \"@"(redBox, list{background(radialGradient(list{(zero, red), (pct(100.), blue)}))}),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{background(repeatingRadialGradient(list{(zero, red), (pct(10.), blue)}))},
          ),
        )}
      />
    </Section>
    <Section name="lengths">
      <div
        className={style(
          \"@"(
            redBox,
            list{height(ch(1.2)), width(px(10)), maxHeight(pct(50.)), maxWidth(pct(100.))},
          ),
        )}
      />
      differentHeightLengths
    </Section>
    <Section name="calc">
      <div
        className={style(
          \"@"(
            redBox,
            list{
              height({
                open Calc
                pt(14) - px(10)
              }),
            },
          ),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              height({
                open Calc
                cm(0.2) + mm(10.)
              }),
            },
          ),
        )}
      />
    </Section>
    <Section name="display">
      <div className={style(\"@"(redBox, list{display(block)}))} />
      <div className={style(\"@"(redBox, list{display(inline)}))} />
      <div className={style(\"@"(redBox, list{display(inlineBlock)}))} />
      <div className={style(\"@"(redBox, list{display(none)}))} />
      <div className={style(\"@"(redBox, list{display(flexBox)}))} />
    </Section>
    <Section name="position">
      <div
        className={style(
          \"@"(redBox, list{position(absolute), top(zero), left(zero), right(zero), bottom(zero)}),
        )}
      />
      <div className={style(\"@"(redBox, list{position(relative)}))} />
      <div className={style(\"@"(redBox, list{position(fixed), bottom(px(10)), right(px(10))}))} />
      <div className={style(\"@"(redBox, list{position(static)}))} />
      <div className={style(\"@"(redBox, list{position(sticky)}))} />
    </Section>
    <Section name="Padding & Margin">
      <div className={style(\"@"(redBox, list{padding(px(10)), margin(px(10))}))} />
      <div
        className={style(
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
        )}
      />
      <div
        className={style(
          \"@"(redBox, list{padding2(~v=px(10), ~h=px(20)), margin2(~v=px(10), ~h=px(20))}),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              padding3(~top=px(10), ~h=px(20), ~bottom=px(1)),
              margin3(~top=px(10), ~h=px(20), ~bottom=px(2)),
            },
          ),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              padding4(~top=px(10), ~bottom=px(1), ~left=px(5), ~right=px(15)),
              margin4(~top=px(10), ~bottom=px(1), ~left=px(5), ~right=px(15)),
            },
          ),
        )}
      />
    </Section>
    <Section name="grid">
      <div
        className={style(list{
          width(pct(100.)),
          height(px(500)),
          display(grid),
          gridTemplateColumns(list{px(150), auto, px(150)}),
          gridTemplateRows(list{px(60), auto}),
        })}>
        <div
          className={style(list{
            gridColumnStart(1),
            gridColumnEnd(4),
            background(red),
            gridRowStart(1),
            gridRowEnd(1),
          })}
        />
        <div className={style(list{background(blue), gridColumn(1, 1), gridRow(2, 2)})} />
        <div
          className={style(list{
            background(green),
            gridColumn(2, 2),
            gridRow(2, 2),
            display(inlineGrid),
            gridTemplateColumns(list{px(50), auto}),
            gridTemplateRows(list{px(40), auto}),
          })}>
          <div className={style(list{background(yellow), gridRow(1, 1), gridColumn(2, 2)})} />
          <div className={style(list{background(green), gridRow(1, 2), gridColumn(1, 1)})} />
          <div className={style(list{background(purple), gridRow(2, 2), gridColumn(2, 2)})} />
        </div>
        <div
          className={style(list{
            gridColumnStart(3),
            gridColumnEnd(3),
            background(blue),
            gridRowStart(2),
            gridRowEnd(2),
          })}
        />
      </div>
      <div className={style(list{display(#grid), gridAutoFlow(#row)})}>
        <div className={style(list{background(purple)})}>
          {"grid auto direction row 1"->React.string}
        </div>
        <div className={style(list{background(green)})}>
          {"grid auto direction row 2"->React.string}
        </div>
      </div>
      <div
        className={style(list{
          display(#grid),
          gridTemplateColumns(list{100->px, #repeat(#num(2), 60->px)}),
        })}>
        <div className={style(list{background(purple)})}> {"Grid track repeat"->React.string} </div>
        <div className={style(list{background(green)})}> {"two times"->React.string} </div>
        <div className={style(list{background(red)})}> {"three times"->React.string} </div>
      </div>
      <div className={style(list{display(#grid), gridAutoColumns(100->px)})}>
        <div className={style(list{background(purple)})}>
          {"Grid auto columns (100px)"->React.string}
        </div>
        <div className={style(list{background(green)})}> {"100px"->React.string} </div>
        <div className={style(list{background(blue)})}> {"100px"->React.string} </div>
      </div>
    </Section>
    <Section name="flexbox">
      <div
        className={style(list{
          flexDirection(column),
          flexGrow(1.),
          alignItems(stretch),
          selector("& > *", list{marginBottom(px(10)), width(pct(100.))}),
        })}>
        <div
          className={style(list{
            display(flexBox),
            flexDirection(row),
            background(gray),
            alignItems(flexStart),
            justifyContent(flexEnd),
          })}>
          <div
            className={style(
              \"@"(redBox, list{order(1), flexGrow(1.), flexShrink(1.), flexBasis(auto)}),
            )}
          />
          <div className={style(\"@"(redBox, list{flex(none)}))} />
          <div
            className={style(
              \"@"(redBox, list{order(1), flex3(~grow=1.5, ~shrink=0.8, ~basis=100->px)}),
            )}
          />
          <div className={style(\"@"(redBox, list{alignSelf(flexEnd)}))} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style(list{
            display(flexBox),
            flexDirection(column),
            background(gray),
            alignItems(baseline),
            justifyContent(flexStart),
          })}>
          <div className={style(redBox)} />
          <div className={style(redBox)} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style(list{
            display(flexBox),
            flexDirection(rowReverse),
            background(gray),
            alignItems(center),
            justifyContent(spaceBetween),
          })}>
          <div className={style(redBox)} />
          <div className={style(\"@"(redBox, list{height(px(50)), width(px(50))}))} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style(list{
            display(flexBox),
            flexDirection(columnReverse),
            background(gray),
            alignItems(flexEnd),
            justifyContent(flexEnd),
          })}>
          <div className={style(redBox)} />
          <div className={style(\"@"(redBox, list{height(px(50)), width(px(50))}))} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style(list{
            display(flexBox),
            flexDirection(row),
            background(gray),
            alignItems(stretch),
            justifyContent(spaceAround),
          })}>
          <div className={style(redBox)} />
          <div className={style(\"@"(redBox, list{height(px(50)), width(px(50))}))} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style(list{
            display(flexBox),
            flexDirection(row),
            background(gray),
            alignItems(stretch),
            justifyContent(spaceEvenly),
          })}>
          <div className={style(redBox)} />
          <div className={style(\"@"(redBox, list{height(px(50)), width(px(50))}))} />
          <div className={style(redBox)} />
        </div>
      </div>
    </Section>
    <Section name="float">
      <div className={style(\"@"(redBox, list{Css.float(#left), clear(#right)}))} />
      <div className={style(\"@"(redBox, list{Css.float(#right), clear(#left)}))} />
      <div className={style(\"@"(redBox, list{Css.float(none), clear(both)}))} />
    </Section>
    <Section name="overflow">
      <div className={style(\"@"(redBox, list{overflow(hidden)}))} />
      <div className={style(\"@"(redBox, list{overflow(visible)}))} />
      <div className={style(\"@"(redBox, list{overflow(auto)}))} />
      <div className={style(\"@"(redBox, list{overflow(scroll)}))} />
    </Section>
    <Section name="border">
      <div
        className={style(\"@"(redBox, list{border(px(5), solid, blue), borderRadius(px(1000))}))}
      />
      <div
        className={style(
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
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              borderTop(px(5), dashed, hex("FFF")),
              borderRight(px(5), dotted, rgb(0, 0, 0)),
              borderBottom(px(5), none, green),
              borderLeft(px(5), solid, blue),
            },
          ),
        )}
      />
      <div
        className={style(
          \"@"(redBox, list{borderWidth(px(5)), borderStyle(solid), borderColor(blue)}),
        )}
      />
      <div
        className={style(
          \"@"(redBox, list{borderTopWidth(px(5)), borderTopStyle(solid), borderTopColor(blue)}),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{borderBottomWidth(px(5)), borderBottomStyle(solid), borderBottomColor(blue)},
          ),
        )}
      />
      <div
        className={style(
          \"@"(redBox, list{borderLeftWidth(px(5)), borderLeftStyle(solid), borderLeftColor(blue)}),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{borderRightWidth(px(5)), borderRightStyle(solid), borderRightColor(blue)},
          ),
        )}
      />
    </Section>
    <Section name="background">
      <div
        className={style(
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
        )}
      />
      <div
        className={style(
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
        )}
      />
      <div
        className={style(
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
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              backgroundColor(rgb(0, 0, 255)),
              backgroundImage(
                linearGradient(deg(45.), list{(zero, green), (pct(50.), red), (pct(100.), yellow)}),
              ),
              backgroundRepeat(repeatY),
              backgroundSize(contain),
            },
          ),
        )}
      />
    </Section>
    <Section name="cursor">
      <div className={style(\"@"(redBox, list{cursor(#auto)}))}> {"auto"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#default)}))}> {"default"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#none)}))}> {"none"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#contextMenu)}))}>
        {"context menu"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#help)}))}> {"help"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#pointer)}))}> {"pointer"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#progress)}))}>
        {"progress"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#wait)}))}> {"wait"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#cell)}))}> {"cell"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#crosshair)}))}>
        {"crosshair"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#text)}))}> {"text"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#verticalText)}))}>
        {"vert text"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#alias)}))}> {"alias"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#copy)}))}> {"copy"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#move)}))}> {"move"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#noDrop)}))}> {"no drop"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#notAllowed)}))}>
        {"not allowed"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#grab)}))}> {"grab"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#grabbing)}))}>
        {"grabbing"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#allScroll)}))}>
        {"all scroll"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#colResize)}))}>
        {"col resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#rowResize)}))}>
        {"row resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#nResize)}))}>
        {"n resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#eResize)}))}>
        {"e resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#sResize)}))}>
        {"s resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#wResize)}))}>
        {"w resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#neResize)}))}>
        {"ne resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#nwResize)}))}>
        {"nw resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#seResize)}))}>
        {"se resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#swResize)}))}>
        {"sw resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#ewResize)}))}>
        {"ew resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#nsResize)}))}>
        {"ns resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#neswResize)}))}>
        {"nesw resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#nwseResize)}))}>
        {"nwse resize"->React.string}
      </div>
      <div className={style(\"@"(redBox, list{cursor(#zoomIn)}))}> {"zoom in"->React.string} </div>
      <div className={style(\"@"(redBox, list{cursor(#zoomOut)}))}>
        {"zoom out"->React.string}
      </div>
    </Section>
    <Section name="list">
      <ul>
        <li className={style(list{listStyle(#disc, inside, none)})} />
        <li className={style(list{listStyleType(#circle)})} />
        <li className={style(list{listStyleType(#square)})} />
        <li className={style(list{listStyleType(#decimal)})} />
        <li className={style(list{listStyleType(#lowerAlpha)})} />
        <li className={style(list{listStyleType(#upperAlpha)})} />
        <li className={style(list{listStyleType(#lowerGreek)})} />
        <li className={style(list{listStyleType(#lowerLatin)})} />
        <li className={style(list{listStyleType(#upperLatin)})} />
        <li className={style(list{listStyleType(#lowerRoman)})} />
        <li className={style(list{listStyleType(#upperRoman)})} />
        <li
          className={style(list{
            listStyleType(#disc),
            listStylePosition(inside),
            listStyleImage(url("./facebook.png")),
          })}
        />
      </ul>
    </Section>
    <Section name="outline">
      <div className={style(\"@"(redBox, list{outline(px(5), #double, green)}))} />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              outlineStyle(solid),
              outlineWidth(px(5)),
              outlineColor(green),
              outlineOffset(px(5)),
            },
          ),
        )}
      />
      <div className={style(\"@"(redBox, list{outline(px(5), #double, red)}))} />
      <div className={style(\"@"(redBox, list{outline(px(5), #ridge, red)}))} />
    </Section>
    <Section name="transform">
      <div className={style(\"@"(redBox, list{opacity(0.5)}))} />
      <div
        className={style(\"@"(redBox, list{perspective(px(500)), transform(rotate(deg(10.)))}))}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{transforms(list{translate(px(10), pct(10.)), skew(deg(10.), deg(10.))})},
          ),
        )}
      />
      <div
        className={style(
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
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{transform(translate(px(10), pct(10.))), transformOrigin3d(px(10), px(10), px(10))},
          ),
        )}
      />
    </Section>
    <Section name="transition">
      <div
        className={style(
          \"@"(
            redBox,
            list{transition(~duration=300, ~delay=300, ~timingFunction=easeInOut, "transform")},
          ),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              transitionProperty("height"),
              transitionDelay(300),
              transitionDuration(300),
              transitionTimingFunction(linear),
            },
          ),
        )}
      />
    </Section>
    <Section name="text">
      <p
        className={style(list{
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
        })}>
        {"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."->React.string}
      </p>
      <h2 className={style(list{width(pct(100.))})}> {"Named Font weights"->React.string} </h2>
      <span
        className={style(list{
          fontWeight(thin),
          paddingRight(px(10)),
          borderRight(px(1), solid, black),
        })}>
        {"thin"->React.string}
      </span>
      <span className={style(list{fontWeight(extraLight), ...fontItem})}>
        {"extra light"->React.string}
      </span>
      <span className={style(list{fontWeight(light), ...fontItem})}> {"light"->React.string} </span>
      <span className={style(list{fontWeight(normal), ...fontItem})}>
        {"normal"->React.string}
      </span>
      <span className={style(list{fontWeight(medium), ...fontItem})}>
        {"medium"->React.string}
      </span>
      <span className={style(list{fontWeight(semiBold), ...fontItem})}>
        {"semiBold"->React.string}
      </span>
      <span className={style(list{fontWeight(bold), ...fontItem})}> {"bold"->React.string} </span>
      <span className={style(list{fontWeight(extraBold), ...fontItem})}>
        {"extra bold"->React.string}
      </span>
      <span className={style(list{fontWeight(#black), ...fontItem})}>
        {"black"->React.string}
      </span>
      <span className={style(list{fontWeight(lighter), ...fontItem})}>
        {"lighter"->React.string}
      </span>
      <span className={style(list{fontWeight(bolder), ...fontItem})}>
        {"bolder"->React.string}
      </span>
    </Section>
    <Section name="animation">
      <div
        className={style(
          \"@"(
            redBox,
            list{
              animation(
                ~duration=300,
                ~delay=300,
                ~direction=reverse,
                ~timingFunction=linear,
                ~fillMode=forwards,
                ~playState=running,
                ~iterationCount=infinite,
                spin,
              ),
            },
          ),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              animations(list{
                Animation.shorthand(~duration=300, ~iterationCount=infinite, spin),
                Animation.shorthand(~duration=300, ~iterationCount=infinite, scaleAnimation),
              }),
            },
          ),
        )}
      />
      <div
        className={style(
          \"@"(
            redBox,
            list{
              animationName(spin),
              animationTimingFunction(easeIn),
              animationDuration(300),
              animationDelay(300),
              animationDirection(normal),
              animationFillMode(backwards),
              animationPlayState(paused),
              animationIterationCount(count(5)),
            },
          ),
        )}
      />
    </Section>
    <Section name="cascading">
      {"inherit"->React.string}
      <div
        className={style(list{
          display(inherit_),
          position(inherit_),
          fontSize(inherit_),
          fontStyle(inherit_),
          lineHeight(inherit_),
        })}
      />
      {"unset"->React.string}
      <div
        className={style(list{
          display(unset),
          position(unset),
          fontSize(unset),
          fontStyle(unset),
          lineHeight(unset),
        })}
      />
    </Section>
    <Section name="columns">
      <p className={style(list{columnCount(count(10))})}>
        {"This is a bunch of text split into columns
             using the CSS `column-count` property. The text
             is equally distributed over the columns."->React.string}
      </p>
    </Section>
    <Section name="resize">
      <textarea className={style(list{resize(none)})}>
        {"Can't resize textarea"->React.string}
      </textarea>
      <div className={style(list{marginLeft(px(20)), overflow(scroll), resize(horizontal)})}>
        {"Resizable div (horizontal)"->React.string}
      </div>
      <div className={style(list{marginLeft(px(20)), overflow(scroll), resize(vertical)})}>
        {"Resizable div (vertical)"->React.string}
      </div>
    </Section>
    <Section name="content">
      <div
        className={style(list{
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
        })}>
        {"none"->React.string}
      </div>
      <div
        className={style(list{
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
        })}>
        {"normal"->React.string}
      </div>
      <div className={style(list{position(relative), marginLeft(px(20))})}>
        <a
          href="https://github.com/SentiaAnalytics/bs-css"
          className={style(list{
            before(list{
              contentRule(#text("external ")),
              backgroundColor(red),
              display(inlineBlock),
              flexBasis(content /* for test */),
            }),
          })}>
          {"link"->React.string}
        </a>
      </div>
      <div
        className={style(list{
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
        })}>
        {"empty content"->React.string}
      </div>
      <div
        className={style(list{
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
        })}>
        {"url"->React.string}
      </div>
      <div
        className={style(list{
          marginLeft(px(20)),
          counterReset(Types.CounterReset.reset("foo", ~value=1)),
          before(list{contentRule(Types.Counter.counter("foo")), border(px(1), solid, black)}),
        })}>
        {"counter"->React.string}
      </div>
      <div
        className={style(list{
          counterReset(Types.CounterReset.reset("foo", ~value=1)),
          marginLeft(px(20)),
        })}>
        <div
          className={style(list{
            counterReset(Types.CounterReset.reset("foo", ~value=2)),
            before(list{
              contentRule(Types.Counters.counters("foo", ~separator="@", ~style=#upperRoman)),
              border(px(1), solid, black),
            }),
          })}>
          {"counters"->React.string}
        </div>
      </div>
      <div
        className={style(list{
          marginLeft(px(20)),
          before(list{contentRule(#attr("class")), border(px(1), solid, black)}),
        })}>
        {"attr"->React.string}
      </div>
      <div
        className={style(list{
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
        })}>
        {"linear gradient"->React.string}
      </div>
      <div
        className={style(list{
          marginLeft(px(20)),
          before(list{
            contentRules(list{#openQuote, #text("foo"), #closeQuote}),
            border(px(1), solid, black),
          }),
        })}>
        {"contents (quotes)"->React.string}
      </div>
    </Section>
    <Section name="insertRule, the ultimate escape hatch"> <div className="raw-css" /> </Section>
    <Section name="merging style names">
      <button className=mergedStyles> {"Merged"->React.string} </button>
    </Section>
    <Section name="filter">
      <div className={style(\"@"(redBox, list{filter(list{#blur(#px(10))})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#brightness(50.)})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#contrast(50.)})}))} />
      <div
        className={style(
          \"@"(
            redBox,
            list{filter(list{#dropShadow(#px(3), #px(3), #px(3), #rgb(200, 100, 100))})},
          ),
        )}
      />
      <div className={style(\"@"(redBox, list{filter(list{#grayscale(50.)})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#hueRotate(#deg(180.))})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#invert(50.)})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#opacity(50.)})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#saturate(50.)})}))} />
      <div className={style(\"@"(redBox, list{filter(list{#sepia(50.)})}))} />
      <div
        className={style(
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
        )}
      />
      <svg height="0" className={style(list{display(#none)})}>
        <filter id="f1"> <feGaussianBlur stdDeviation="3" /> </filter>
      </svg>
      <div className={style(\"@"(redBox, list{filter(list{#url("#f1")})}))} />
    </Section>
    <Section name="direction">
      <Section name="ltr">
        <div className={style(list{direction(#ltr), display(#flex)})}>
          <div className={style(redBox)}> {"1"->React.string} </div>
          <div className={style(redBox)}> {"2"->React.string} </div>
          <div className={style(redBox)}> {"3"->React.string} </div>
          <div className={style(redBox)}> {"4"->React.string} </div>
        </div>
      </Section>
      <Section name="rtl">
        <div className={style(list{direction(#rtl), display(#flex)})}>
          <div className={style(redBox)}> {"1"->React.string} </div>
          <div className={style(redBox)}> {"2"->React.string} </div>
          <div className={style(redBox)}> {"3"->React.string} </div>
          <div className={style(redBox)}> {"4"->React.string} </div>
        </div>
      </Section>
      <Section name="unset">
        <div className={style(list{direction(#unset), display(#flex)})}>
          <div className={style(redBox)}> {"1"->React.string} </div>
          <div className={style(redBox)}> {"2"->React.string} </div>
          <div className={style(redBox)}> {"3"->React.string} </div>
          <div className={style(redBox)}> {"4"->React.string} </div>
        </div>
      </Section>
    </Section>
    <Section name="object-fit">
      <img className={style(\"@"(redBox, list{objectFit(#fill)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#contain)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#cover)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#none)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#scaleDown)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#inherit_)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#initial)}))} src="./img-29.jpg" />
      <img className={style(\"@"(redBox, list{objectFit(#unset)}))} src="./img-29.jpg" />
    </Section>
  </div>
