module Styles = {
  open Css
  let root = style(list{display(flexBox), alignItems(stretch), justifyContent(center)})
  let mainImageWrapper = style(list{position(relative)})
  let mainImageWrapperRecipe = style(list{marginBottom(px(16))})
  let image = style(list{display(block), height(px(128)), width(px(128))})
  let recipeIcon = style(list{
    display(block),
    height(px(64)),
    width(px(64)),
    position(absolute),
    right(px(-16)),
    bottom(px(-16)),
    opacity(0.95),
    transition(~duration=200, "all"),
    hover(list{opacity(1.)}),
  })
  let variantButton = style(list{
    background(transparent),
    borderWidth(zero),
    padding(zero),
    cursor(pointer),
    flexGrow(1.),
    display(flexBox),
    alignItems(center),
    justifyContent(center),
    transition(~duration=200, "all"),
    outlineStyle(none),
    opacity(0.2),
    media("(hover: none)", list{opacity(0.5)}),
    media("(hover: hover)", list{hover(list{important(opacity(1.))})}),
    unsafe("touchAction", "manipulation"),
  })
  let variantButtonLeft = "variant-button-left"
  let variantButtonRight = "variant-button-right"
  @module("./assets/variant_left.png")
  external variantLeftIcon: string = "default"
  let variantArrow = style(list{
    display(inlineBlock),
    width(px(32)),
    height(px(32)),
    backgroundSize(cover),
  })
  let variantArrowLeft = style(list{backgroundImage(url(variantLeftIcon))})
  @module("./assets/variant_right.png")
  external variantRightIcon: string = "default"
  let variantArrowRight = style(list{backgroundImage(url(variantRightIcon))})
  let variantButtonNarrow = style(list{
    selector("& ." ++ variantArrow, list{width(px(16)), height(px(16))}),
  })
  let variantButtonDisabled = style(list{
    important(opacity(0.1)),
    cursor(#default),
    media("(hover: hover)", list{hover(list{important(opacity(0.1))})}),
  })
}

@react.component
let make = (
  ~item: Item.t,
  ~variant,
  ~narrow=false,
  ~forceTooltip=false,
  ~link=true,
  ~className=?,
  (),
) => {
  let numCollapsedVariants = if Item.isRecipe(~item) {
    1
  } else {
    switch item.variations {
    | Single
    | OneDimension(_) => 1
    | TwoDimensions(a, b) =>
      if item.bodyCustomizable {
        a * b
      } else {
        b
      }
    }
  }
  let (offset, setOffset) = React.useState(() => 0)
  <div className={Cn.make(list{Styles.root, Cn.unpack(className)})}>
    {numCollapsedVariants > 1
      ? <button
          onClick={_ => setOffset(offset => Js.Math.max_int(offset - 1, 0))}
          className={Cn.make(list{
            Styles.variantButton,
            Styles.variantButtonLeft,
            Cn.ifTrue(Styles.variantButtonNarrow, narrow),
            Cn.ifTrue(Styles.variantButtonDisabled, offset <= 0),
          })}>
          <span className={Cn.make(list{Styles.variantArrow, Styles.variantArrowLeft})} />
        </button>
      : React.null}
    <div
      className={Cn.make(list{
        Styles.mainImageWrapper,
        Cn.ifTrue(Styles.mainImageWrapperRecipe, Item.isRecipe(~item)),
        Cn.unpack(className),
      })}>
      {
        let image =
          <Link path={Utils.getItemDetailUrl(~itemId=item.id, ~variant=Some(variant + offset))}>
            <img src={Item.getImageUrl(~item, ~variant=variant + offset)} className=Styles.image />
          </Link>
        let variantName = if numCollapsedVariants > 1 || forceTooltip {
          Item.getVariantName(~item, ~variant=variant + offset, ())
        } else {
          None
        }
        switch (Utils.browserSupportsHover, variantName) {
        | (true, Some(variantName)) =>
          <ReactAtmosphere.Tooltip text={React.string(variantName)}>
            {({onMouseEnter, onMouseLeave, onFocus, onBlur, ref}) =>
              <div onMouseEnter onMouseLeave onFocus onBlur ref={ReactDOMRe.Ref.domRef(ref)}>
                image
              </div>}
          </ReactAtmosphere.Tooltip>
        | _ => image
        }
      }
      {Item.isRecipe(~item)
        ? <img src={Constants.cdnUrl ++ "/images/DIYRecipe.png"} className=Styles.recipeIcon />
        : React.null}
    </div>
    {numCollapsedVariants > 1
      ? <button
          onClick={_ => setOffset(offset => Js.Math.min_int(offset + 1, numCollapsedVariants - 1))}
          className={Cn.make(list{
            Styles.variantButton,
            Styles.variantButtonRight,
            Cn.ifTrue(Styles.variantButtonNarrow, narrow),
            Cn.ifTrue(Styles.variantButtonDisabled, offset >= numCollapsedVariants - 1),
          })}>
          <span className={Cn.make(list{Styles.variantArrow, Styles.variantArrowRight})} />
        </button>
      : React.null}
  </div>
}
