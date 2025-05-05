/* Copyright (C) 2022- Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

@@deprecated("Use `Jsx.DOMStyle` instead.")
type t = {
  /**
    See [`accent-color`](https://developer.mozilla.org/docs/Web/CSS/accent-color) on MDN.
    */
  accentColor?: string,
  /**
    See [`align-content`](https://developer.mozilla.org/docs/Web/CSS/align-content) on MDN.
    */
  alignContent?: string,
  /**
    See [`align-items`](https://developer.mozilla.org/docs/Web/CSS/align-items) on MDN.
    */
  alignItems?: string,
  /**
    See [`align-self`](https://developer.mozilla.org/docs/Web/CSS/align-self) on MDN.
    */
  alignSelf?: string,
  /**
    See [`all`](https://developer.mozilla.org/docs/Web/CSS/all) on MDN.
    */
  all?: string,
  /**
    See [`animation`](https://developer.mozilla.org/docs/Web/CSS/animation) on MDN.
    */
  animation?: string,
  /**
    See [`animation-composition`](https://developer.mozilla.org/docs/Web/CSS/animation-composition) on MDN.
    */
  animationComposition?: string,
  /**
    See [`animation-delay`](https://developer.mozilla.org/docs/Web/CSS/animation-delay) on MDN.
    */
  animationDelay?: string,
  /**
    See [`animation-direction`](https://developer.mozilla.org/docs/Web/CSS/animation-direction) on MDN.
    */
  animationDirection?: string,
  /**
    See [`animation-duration`](https://developer.mozilla.org/docs/Web/CSS/animation-duration) on MDN.
    */
  animationDuration?: string,
  /**
    See [`animation-fill-mode`](https://developer.mozilla.org/docs/Web/CSS/animation-fill-mode) on MDN.
    */
  animationFillMode?: string,
  /**
    See [`animation-iteration-count`](https://developer.mozilla.org/docs/Web/CSS/animation-iteration-count) on MDN.
    */
  animationIterationCount?: string,
  /**
    See [`animation-name`](https://developer.mozilla.org/docs/Web/CSS/animation-name) on MDN.
    */
  animationName?: string,
  /**
    See [`animation-play-state`](https://developer.mozilla.org/docs/Web/CSS/animation-play-state) on MDN.
    */
  animationPlayState?: string,
  /**
    See [`animation-timing-function`](https://developer.mozilla.org/docs/Web/CSS/animation-timing-function) on MDN.
    */
  animationTimingFunction?: string,
  /**
    See [`appearance`](https://developer.mozilla.org/docs/Web/CSS/appearance) on MDN.
    */
  appearance?: string,
  /**
    See [`aspect-ratio`](https://developer.mozilla.org/docs/Web/CSS/aspect-ratio) on MDN.
    */
  aspectRatio?: string,
  /**
    See [`backdrop-filter`](https://developer.mozilla.org/docs/Web/CSS/backdrop-filter) on MDN.
    */
  backdropFilter?: string,
  /**
    See [`backface-visibility`](https://developer.mozilla.org/docs/Web/CSS/backface-visibility) on MDN.
    */
  backfaceVisibility?: string,
  /**
    See [`background`](https://developer.mozilla.org/docs/Web/CSS/background) on MDN.
    */
  background?: string,
  /**
    See [`background-attachment`](https://developer.mozilla.org/docs/Web/CSS/background-attachment) on MDN.
    */
  backgroundAttachment?: string,
  /**
    See [`background-blend-mode`](https://developer.mozilla.org/docs/Web/CSS/background-blend-mode) on MDN.
    */
  backgroundBlendMode?: string,
  /**
    See [`background-clip`](https://developer.mozilla.org/docs/Web/CSS/background-clip) on MDN.
    */
  backgroundClip?: string,
  /**
    See [`background-color`](https://developer.mozilla.org/docs/Web/CSS/background-color) on MDN.
    */
  backgroundColor?: string,
  /**
    See [`background-image`](https://developer.mozilla.org/docs/Web/CSS/background-image) on MDN.
    */
  backgroundImage?: string,
  /**
    See [`background-origin`](https://developer.mozilla.org/docs/Web/CSS/background-origin) on MDN.
    */
  backgroundOrigin?: string,
  /**
    See [`background-position`](https://developer.mozilla.org/docs/Web/CSS/background-position) on MDN.
    */
  backgroundPosition?: string,
  /**
    See [`background-position-x`](https://developer.mozilla.org/docs/Web/CSS/background-position-x) on MDN.
    */
  backgroundPositionX?: string,
  /**
    See [`background-position-y`](https://developer.mozilla.org/docs/Web/CSS/background-position-y) on MDN.
    */
  backgroundPositionY?: string,
  /**
    See [`background-repeat`](https://developer.mozilla.org/docs/Web/CSS/background-repeat) on MDN.
    */
  backgroundRepeat?: string,
  /**
    See [`background-size`](https://developer.mozilla.org/docs/Web/CSS/background-size) on MDN.
    */
  backgroundSize?: string,
  /**
    See [`block-size`](https://developer.mozilla.org/docs/Web/CSS/block-size) on MDN.
    */
  blockSize?: string,
  /**
    See [`border`](https://developer.mozilla.org/docs/Web/CSS/border) on MDN.
    */
  border?: string,
  /**
    See [`border-block`](https://developer.mozilla.org/docs/Web/CSS/border-block) on MDN.
    */
  borderBlock?: string,
  /**
    See [`border-block-color`](https://developer.mozilla.org/docs/Web/CSS/border-block-color) on MDN.
    */
  borderBlockColor?: string,
  /**
    See [`border-block-end`](https://developer.mozilla.org/docs/Web/CSS/border-block-end) on MDN.
    */
  borderBlockEnd?: string,
  /**
    See [`border-block-end-color`](https://developer.mozilla.org/docs/Web/CSS/border-block-end-color) on MDN.
    */
  borderBlockEndColor?: string,
  /**
    See [`border-block-end-style`](https://developer.mozilla.org/docs/Web/CSS/border-block-end-style) on MDN.
    */
  borderBlockEndStyle?: string,
  /**
    See [`border-block-end-width`](https://developer.mozilla.org/docs/Web/CSS/border-block-end-width) on MDN.
    */
  borderBlockEndWidth?: string,
  /**
    See [`border-block-start`](https://developer.mozilla.org/docs/Web/CSS/border-block-start) on MDN.
    */
  borderBlockStart?: string,
  /**
    See [`border-block-start-color`](https://developer.mozilla.org/docs/Web/CSS/border-block-start-color) on MDN.
    */
  borderBlockStartColor?: string,
  /**
    See [`border-block-start-style`](https://developer.mozilla.org/docs/Web/CSS/border-block-start-style) on MDN.
    */
  borderBlockStartStyle?: string,
  /**
    See [`border-block-start-width`](https://developer.mozilla.org/docs/Web/CSS/border-block-start-width) on MDN.
    */
  borderBlockStartWidth?: string,
  /**
    See [`border-block-style`](https://developer.mozilla.org/docs/Web/CSS/border-block-style) on MDN.
    */
  borderBlockStyle?: string,
  /**
    See [`border-block-width`](https://developer.mozilla.org/docs/Web/CSS/border-block-width) on MDN.
    */
  borderBlockWidth?: string,
  /**
    See [`border-bottom`](https://developer.mozilla.org/docs/Web/CSS/border-bottom) on MDN.
    */
  borderBottom?: string,
  /**
    See [`border-bottom-color`](https://developer.mozilla.org/docs/Web/CSS/border-bottom-color) on MDN.
    */
  borderBottomColor?: string,
  /**
    See [`border-bottom-left-radius`](https://developer.mozilla.org/docs/Web/CSS/border-bottom-left-radius) on MDN.
    */
  borderBottomLeftRadius?: string,
  /**
    See [`border-bottom-right-radius`](https://developer.mozilla.org/docs/Web/CSS/border-bottom-right-radius) on MDN.
    */
  borderBottomRightRadius?: string,
  /**
    See [`border-bottom-style`](https://developer.mozilla.org/docs/Web/CSS/border-bottom-style) on MDN.
    */
  borderBottomStyle?: string,
  /**
    See [`border-bottom-width`](https://developer.mozilla.org/docs/Web/CSS/border-bottom-width) on MDN.
    */
  borderBottomWidth?: string,
  /**
    See [`border-collapse`](https://developer.mozilla.org/docs/Web/CSS/border-collapse) on MDN.
    */
  borderCollapse?: string,
  /**
    See [`border-color`](https://developer.mozilla.org/docs/Web/CSS/border-color) on MDN.
    */
  borderColor?: string,
  /**
    See [`border-end-end-radius`](https://developer.mozilla.org/docs/Web/CSS/border-end-end-radius) on MDN.
    */
  borderEndEndRadius?: string,
  /**
    See [`border-end-start-radius`](https://developer.mozilla.org/docs/Web/CSS/border-end-start-radius) on MDN.
    */
  borderEndStartRadius?: string,
  /**
    See [`border-image`](https://developer.mozilla.org/docs/Web/CSS/border-image) on MDN.
    */
  borderImage?: string,
  /**
    See [`border-image-outset`](https://developer.mozilla.org/docs/Web/CSS/border-image-outset) on MDN.
    */
  borderImageOutset?: string,
  /**
    See [`border-image-repeat`](https://developer.mozilla.org/docs/Web/CSS/border-image-repeat) on MDN.
    */
  borderImageRepeat?: string,
  /**
    See [`border-image-slice`](https://developer.mozilla.org/docs/Web/CSS/border-image-slice) on MDN.
    */
  borderImageSlice?: string,
  /**
    See [`border-image-source`](https://developer.mozilla.org/docs/Web/CSS/border-image-source) on MDN.
    */
  borderImageSource?: string,
  /**
    See [`border-image-width`](https://developer.mozilla.org/docs/Web/CSS/border-image-width) on MDN.
    */
  borderImageWidth?: string,
  /**
    See [`border-inline`](https://developer.mozilla.org/docs/Web/CSS/border-inline) on MDN.
    */
  borderInline?: string,
  /**
    See [`border-inline-color`](https://developer.mozilla.org/docs/Web/CSS/border-inline-color) on MDN.
    */
  borderInlineColor?: string,
  /**
    See [`border-inline-end`](https://developer.mozilla.org/docs/Web/CSS/border-inline-end) on MDN.
    */
  borderInlineEnd?: string,
  /**
    See [`border-inline-end-color`](https://developer.mozilla.org/docs/Web/CSS/border-inline-end-color) on MDN.
    */
  borderInlineEndColor?: string,
  /**
    See [`border-inline-end-style`](https://developer.mozilla.org/docs/Web/CSS/border-inline-end-style) on MDN.
    */
  borderInlineEndStyle?: string,
  /**
    See [`border-inline-end-width`](https://developer.mozilla.org/docs/Web/CSS/border-inline-end-width) on MDN.
    */
  borderInlineEndWidth?: string,
  /**
    See [`border-inline-start`](https://developer.mozilla.org/docs/Web/CSS/border-inline-start) on MDN.
    */
  borderInlineStart?: string,
  /**
    See [`border-inline-start-color`](https://developer.mozilla.org/docs/Web/CSS/border-inline-start-color) on MDN.
    */
  borderInlineStartColor?: string,
  /**
    See [`border-inline-start-style`](https://developer.mozilla.org/docs/Web/CSS/border-inline-start-style) on MDN.
    */
  borderInlineStartStyle?: string,
  /**
    See [`border-inline-start-width`](https://developer.mozilla.org/docs/Web/CSS/border-inline-start-width) on MDN.
    */
  borderInlineStartWidth?: string,
  /**
    See [`border-inline-style`](https://developer.mozilla.org/docs/Web/CSS/border-inline-style) on MDN.
    */
  borderInlineStyle?: string,
  /**
    See [`border-inline-width`](https://developer.mozilla.org/docs/Web/CSS/border-inline-width) on MDN.
    */
  borderInlineWidth?: string,
  /**
    See [`border-left`](https://developer.mozilla.org/docs/Web/CSS/border-left) on MDN.
    */
  borderLeft?: string,
  /**
    See [`border-left-color`](https://developer.mozilla.org/docs/Web/CSS/border-left-color) on MDN.
    */
  borderLeftColor?: string,
  /**
    See [`border-left-style`](https://developer.mozilla.org/docs/Web/CSS/border-left-style) on MDN.
    */
  borderLeftStyle?: string,
  /**
    See [`border-left-width`](https://developer.mozilla.org/docs/Web/CSS/border-left-width) on MDN.
    */
  borderLeftWidth?: string,
  /**
    See [`border-radius`](https://developer.mozilla.org/docs/Web/CSS/border-radius) on MDN.
    */
  borderRadius?: string,
  /**
    See [`border-right`](https://developer.mozilla.org/docs/Web/CSS/border-right) on MDN.
    */
  borderRight?: string,
  /**
    See [`border-right-color`](https://developer.mozilla.org/docs/Web/CSS/border-right-color) on MDN.
    */
  borderRightColor?: string,
  /**
    See [`border-right-style`](https://developer.mozilla.org/docs/Web/CSS/border-right-style) on MDN.
    */
  borderRightStyle?: string,
  /**
    See [`border-right-width`](https://developer.mozilla.org/docs/Web/CSS/border-right-width) on MDN.
    */
  borderRightWidth?: string,
  /**
    See [`border-spacing`](https://developer.mozilla.org/docs/Web/CSS/border-spacing) on MDN.
    */
  borderSpacing?: string,
  /**
    See [`border-start-end-radius`](https://developer.mozilla.org/docs/Web/CSS/border-start-end-radius) on MDN.
    */
  borderStartEndRadius?: string,
  /**
    See [`border-start-start-radius`](https://developer.mozilla.org/docs/Web/CSS/border-start-start-radius) on MDN.
    */
  borderStartStartRadius?: string,
  /**
    See [`border-style`](https://developer.mozilla.org/docs/Web/CSS/border-style) on MDN.
    */
  borderStyle?: string,
  /**
    See [`border-top`](https://developer.mozilla.org/docs/Web/CSS/border-top) on MDN.
    */
  borderTop?: string,
  /**
    See [`border-top-color`](https://developer.mozilla.org/docs/Web/CSS/border-top-color) on MDN.
    */
  borderTopColor?: string,
  /**
    See [`border-top-left-radius`](https://developer.mozilla.org/docs/Web/CSS/border-top-left-radius) on MDN.
    */
  borderTopLeftRadius?: string,
  /**
    See [`border-top-right-radius`](https://developer.mozilla.org/docs/Web/CSS/border-top-right-radius) on MDN.
    */
  borderTopRightRadius?: string,
  /**
    See [`border-top-style`](https://developer.mozilla.org/docs/Web/CSS/border-top-style) on MDN.
    */
  borderTopStyle?: string,
  /**
    See [`border-top-width`](https://developer.mozilla.org/docs/Web/CSS/border-top-width) on MDN.
    */
  borderTopWidth?: string,
  /**
    See [`border-width`](https://developer.mozilla.org/docs/Web/CSS/border-width) on MDN.
    */
  borderWidth?: string,
  /**
    See [`bottom`](https://developer.mozilla.org/docs/Web/CSS/bottom) on MDN.
    */
  bottom?: string,
  /**
    See [`box-decoration-break`](https://developer.mozilla.org/docs/Web/CSS/box-decoration-break) on MDN.
    */
  boxDecorationBreak?: string,
  /**
    See [`box-shadow`](https://developer.mozilla.org/docs/Web/CSS/box-shadow) on MDN.
    */
  boxShadow?: string,
  /**
    See [`box-sizing`](https://developer.mozilla.org/docs/Web/CSS/box-sizing) on MDN.
    */
  boxSizing?: string,
  /**
    See [`break-after`](https://developer.mozilla.org/docs/Web/CSS/break-after) on MDN.
    */
  breakAfter?: string,
  /**
    See [`break-before`](https://developer.mozilla.org/docs/Web/CSS/break-before) on MDN.
    */
  breakBefore?: string,
  /**
    See [`break-inside`](https://developer.mozilla.org/docs/Web/CSS/break-inside) on MDN.
    */
  breakInside?: string,
  /**
    See [`caption-side`](https://developer.mozilla.org/docs/Web/CSS/caption-side) on MDN.
    */
  captionSide?: string,
  /**
    */
  caret?: string,
  /**
    See [`caret-color`](https://developer.mozilla.org/docs/Web/CSS/caret-color) on MDN.
    */
  caretColor?: string,
  /**
    */
  caretShape?: string,
  /**
    See [`clear`](https://developer.mozilla.org/docs/Web/CSS/clear) on MDN.
    */
  clear?: string,
  /**
    See [`clip-path`](https://developer.mozilla.org/docs/Web/CSS/clip-path) on MDN.
    */
  clipPath?: string,
  /**
    See [`clip-rule`](https://developer.mozilla.org/docs/Web/CSS/clip-rule) on MDN.
    */
  clipRule?: string,
  /**
    See [`color`](https://developer.mozilla.org/docs/Web/CSS/color) on MDN.
    */
  color?: string,
  /**
    See [`color-interpolation-filters`](https://developer.mozilla.org/docs/Web/CSS/color-interpolation-filters) on MDN.
    */
  colorInterpolationFilters?: string,
  /**
    See [`color-scheme`](https://developer.mozilla.org/docs/Web/CSS/color-scheme) on MDN.
    */
  colorScheme?: string,
  /**
    See [`column-count`](https://developer.mozilla.org/docs/Web/CSS/column-count) on MDN.
    */
  columnCount?: string,
  /**
    See [`column-fill`](https://developer.mozilla.org/docs/Web/CSS/column-fill) on MDN.
    */
  columnFill?: string,
  /**
    See [`column-gap`](https://developer.mozilla.org/docs/Web/CSS/column-gap) on MDN.
    */
  columnGap?: string,
  /**
    See [`column-rule`](https://developer.mozilla.org/docs/Web/CSS/column-rule) on MDN.
    */
  columnRule?: string,
  /**
    See [`column-rule-color`](https://developer.mozilla.org/docs/Web/CSS/column-rule-color) on MDN.
    */
  columnRuleColor?: string,
  /**
    See [`column-rule-style`](https://developer.mozilla.org/docs/Web/CSS/column-rule-style) on MDN.
    */
  columnRuleStyle?: string,
  /**
    See [`column-rule-width`](https://developer.mozilla.org/docs/Web/CSS/column-rule-width) on MDN.
    */
  columnRuleWidth?: string,
  /**
    See [`column-span`](https://developer.mozilla.org/docs/Web/CSS/column-span) on MDN.
    */
  columnSpan?: string,
  /**
    See [`column-width`](https://developer.mozilla.org/docs/Web/CSS/column-width) on MDN.
    */
  columnWidth?: string,
  /**
    See [`columns`](https://developer.mozilla.org/docs/Web/CSS/columns) on MDN.
    */
  columns?: string,
  /**
    See [`contain`](https://developer.mozilla.org/docs/Web/CSS/contain) on MDN.
    */
  contain?: string,
  /**
    See [`contain-intrinsic-block-size`](https://developer.mozilla.org/docs/Web/CSS/contain-intrinsic-block-size) on MDN.
    */
  containIntrinsicBlockSize?: string,
  /**
    See [`contain-intrinsic-height`](https://developer.mozilla.org/docs/Web/CSS/contain-intrinsic-height) on MDN.
    */
  containIntrinsicHeight?: string,
  /**
    See [`contain-intrinsic-inline-size`](https://developer.mozilla.org/docs/Web/CSS/contain-intrinsic-inline-size) on MDN.
    */
  containIntrinsicInlineSize?: string,
  /**
    See [`contain-intrinsic-size`](https://developer.mozilla.org/docs/Web/CSS/contain-intrinsic-size) on MDN.
    */
  containIntrinsicSize?: string,
  /**
    See [`contain-intrinsic-width`](https://developer.mozilla.org/docs/Web/CSS/contain-intrinsic-width) on MDN.
    */
  containIntrinsicWidth?: string,
  /**
    See [`container`](https://developer.mozilla.org/docs/Web/CSS/container) on MDN.
    */
  container?: string,
  /**
    See [`container-name`](https://developer.mozilla.org/docs/Web/CSS/container-name) on MDN.
    */
  containerName?: string,
  /**
    See [`container-type`](https://developer.mozilla.org/docs/Web/CSS/container-type) on MDN.
    */
  containerType?: string,
  /**
    See [`content`](https://developer.mozilla.org/docs/Web/CSS/content) on MDN.
    */
  content?: string,
  /**
    See [`content-visibility`](https://developer.mozilla.org/docs/Web/CSS/content-visibility) on MDN.
    */
  contentVisibility?: string,
  /**
    See [`counter-increment`](https://developer.mozilla.org/docs/Web/CSS/counter-increment) on MDN.
    */
  counterIncrement?: string,
  /**
    See [`counter-reset`](https://developer.mozilla.org/docs/Web/CSS/counter-reset) on MDN.
    */
  counterReset?: string,
  /**
    See [`counter-set`](https://developer.mozilla.org/docs/Web/CSS/counter-set) on MDN.
    */
  counterSet?: string,
  /**
    See [`cursor`](https://developer.mozilla.org/docs/Web/CSS/cursor) on MDN.
    */
  cursor?: string,
  /**
    See [`cx`](https://developer.mozilla.org/docs/Web/CSS/cx) on MDN.
    */
  cx?: string,
  /**
    See [`cy`](https://developer.mozilla.org/docs/Web/CSS/cy) on MDN.
    */
  cy?: string,
  /**
    See [`d`](https://developer.mozilla.org/docs/Web/CSS/d) on MDN.
    */
  d?: string,
  /**
    See [`direction`](https://developer.mozilla.org/docs/Web/CSS/direction) on MDN.
    */
  direction?: string,
  /**
    See [`display`](https://developer.mozilla.org/docs/Web/CSS/display) on MDN.
    */
  display?: string,
  /**
    See [`dominant-baseline`](https://developer.mozilla.org/docs/Web/CSS/dominant-baseline) on MDN.
    */
  dominantBaseline?: string,
  /**
    See [`empty-cells`](https://developer.mozilla.org/docs/Web/CSS/empty-cells) on MDN.
    */
  emptyCells?: string,
  /**
    See [`fill`](https://developer.mozilla.org/docs/Web/CSS/fill) on MDN.
    */
  fill?: string,
  /**
    See [`fill-opacity`](https://developer.mozilla.org/docs/Web/CSS/fill-opacity) on MDN.
    */
  fillOpacity?: string,
  /**
    See [`fill-rule`](https://developer.mozilla.org/docs/Web/CSS/fill-rule) on MDN.
    */
  fillRule?: string,
  /**
    See [`filter`](https://developer.mozilla.org/docs/Web/CSS/filter) on MDN.
    */
  filter?: string,
  /**
    See [`flex`](https://developer.mozilla.org/docs/Web/CSS/flex) on MDN.
    */
  flex?: string,
  /**
    See [`flex-basis`](https://developer.mozilla.org/docs/Web/CSS/flex-basis) on MDN.
    */
  flexBasis?: string,
  /**
    See [`flex-direction`](https://developer.mozilla.org/docs/Web/CSS/flex-direction) on MDN.
    */
  flexDirection?: string,
  /**
    See [`flex-flow`](https://developer.mozilla.org/docs/Web/CSS/flex-flow) on MDN.
    */
  flexFlow?: string,
  /**
    See [`flex-grow`](https://developer.mozilla.org/docs/Web/CSS/flex-grow) on MDN.
    */
  flexGrow?: string,
  /**
    See [`flex-shrink`](https://developer.mozilla.org/docs/Web/CSS/flex-shrink) on MDN.
    */
  flexShrink?: string,
  /**
    See [`flex-wrap`](https://developer.mozilla.org/docs/Web/CSS/flex-wrap) on MDN.
    */
  flexWrap?: string,
  /**
    See [`float`](https://developer.mozilla.org/docs/Web/CSS/float) on MDN.
    */
  float?: string,
  /**
    See [`flood-color`](https://developer.mozilla.org/docs/Web/CSS/flood-color) on MDN.
    */
  floodColor?: string,
  /**
    See [`flood-opacity`](https://developer.mozilla.org/docs/Web/CSS/flood-opacity) on MDN.
    */
  floodOpacity?: string,
  /**
    See [`font`](https://developer.mozilla.org/docs/Web/CSS/font) on MDN.
    */
  font?: string,
  /**
    See [`font-family`](https://developer.mozilla.org/docs/Web/CSS/font-family) on MDN.
    */
  fontFamily?: string,
  /**
    See [`font-feature-settings`](https://developer.mozilla.org/docs/Web/CSS/font-feature-settings) on MDN.
    */
  fontFeatureSettings?: string,
  /**
    See [`font-kerning`](https://developer.mozilla.org/docs/Web/CSS/font-kerning) on MDN.
    */
  fontKerning?: string,
  /**
    See [`font-language-override`](https://developer.mozilla.org/docs/Web/CSS/font-language-override) on MDN.
    */
  fontLanguageOverride?: string,
  /**
    See [`font-optical-sizing`](https://developer.mozilla.org/docs/Web/CSS/font-optical-sizing) on MDN.
    */
  fontOpticalSizing?: string,
  /**
    See [`font-palette`](https://developer.mozilla.org/docs/Web/CSS/font-palette) on MDN.
    */
  fontPalette?: string,
  /**
    See [`font-size`](https://developer.mozilla.org/docs/Web/CSS/font-size) on MDN.
    */
  fontSize?: string,
  /**
    See [`font-size-adjust`](https://developer.mozilla.org/docs/Web/CSS/font-size-adjust) on MDN.
    */
  fontSizeAdjust?: string,
  /**
    See [`font-stretch`](https://developer.mozilla.org/docs/Web/CSS/font-stretch) on MDN.
    */
  fontStretch?: string,
  /**
    See [`font-style`](https://developer.mozilla.org/docs/Web/CSS/font-style) on MDN.
    */
  fontStyle?: string,
  /**
    See [`font-synthesis`](https://developer.mozilla.org/docs/Web/CSS/font-synthesis) on MDN.
    */
  fontSynthesis?: string,
  /**
    See [`font-synthesis-small-caps`](https://developer.mozilla.org/docs/Web/CSS/font-synthesis-small-caps) on MDN.
    */
  fontSynthesisSmallCaps?: string,
  /**
    See [`font-synthesis-style`](https://developer.mozilla.org/docs/Web/CSS/font-synthesis-style) on MDN.
    */
  fontSynthesisStyle?: string,
  /**
    See [`font-synthesis-weight`](https://developer.mozilla.org/docs/Web/CSS/font-synthesis-weight) on MDN.
    */
  fontSynthesisWeight?: string,
  /**
    See [`font-variant`](https://developer.mozilla.org/docs/Web/CSS/font-variant) on MDN.
    */
  fontVariant?: string,
  /**
    See [`font-variant-alternates`](https://developer.mozilla.org/docs/Web/CSS/font-variant-alternates) on MDN.
    */
  fontVariantAlternates?: string,
  /**
    See [`font-variant-caps`](https://developer.mozilla.org/docs/Web/CSS/font-variant-caps) on MDN.
    */
  fontVariantCaps?: string,
  /**
    See [`font-variant-east-asian`](https://developer.mozilla.org/docs/Web/CSS/font-variant-east-asian) on MDN.
    */
  fontVariantEastAsian?: string,
  /**
    See [`font-variant-emoji`](https://developer.mozilla.org/docs/Web/CSS/font-variant-emoji) on MDN.
    */
  fontVariantEmoji?: string,
  /**
    See [`font-variant-ligatures`](https://developer.mozilla.org/docs/Web/CSS/font-variant-ligatures) on MDN.
    */
  fontVariantLigatures?: string,
  /**
    See [`font-variant-numeric`](https://developer.mozilla.org/docs/Web/CSS/font-variant-numeric) on MDN.
    */
  fontVariantNumeric?: string,
  /**
    See [`font-variant-position`](https://developer.mozilla.org/docs/Web/CSS/font-variant-position) on MDN.
    */
  fontVariantPosition?: string,
  /**
    See [`font-variation-settings`](https://developer.mozilla.org/docs/Web/CSS/font-variation-settings) on MDN.
    */
  fontVariationSettings?: string,
  /**
    See [`font-weight`](https://developer.mozilla.org/docs/Web/CSS/font-weight) on MDN.
    */
  fontWeight?: string,
  /**
    See [`forced-color-adjust`](https://developer.mozilla.org/docs/Web/CSS/forced-color-adjust) on MDN.
    */
  forcedColorAdjust?: string,
  /**
    See [`gap`](https://developer.mozilla.org/docs/Web/CSS/gap) on MDN.
    */
  gap?: string,
  /**
    See [`grid`](https://developer.mozilla.org/docs/Web/CSS/grid) on MDN.
    */
  grid?: string,
  /**
    See [`grid-area`](https://developer.mozilla.org/docs/Web/CSS/grid-area) on MDN.
    */
  gridArea?: string,
  /**
    See [`grid-auto-columns`](https://developer.mozilla.org/docs/Web/CSS/grid-auto-columns) on MDN.
    */
  gridAutoColumns?: string,
  /**
    See [`grid-auto-flow`](https://developer.mozilla.org/docs/Web/CSS/grid-auto-flow) on MDN.
    */
  gridAutoFlow?: string,
  /**
    See [`grid-auto-rows`](https://developer.mozilla.org/docs/Web/CSS/grid-auto-rows) on MDN.
    */
  gridAutoRows?: string,
  /**
    See [`grid-column`](https://developer.mozilla.org/docs/Web/CSS/grid-column) on MDN.
    */
  gridColumn?: string,
  /**
    See [`grid-column-end`](https://developer.mozilla.org/docs/Web/CSS/grid-column-end) on MDN.
    */
  gridColumnEnd?: string,
  /**
    See [`grid-column-start`](https://developer.mozilla.org/docs/Web/CSS/grid-column-start) on MDN.
    */
  gridColumnStart?: string,
  /**
    See [`grid-row`](https://developer.mozilla.org/docs/Web/CSS/grid-row) on MDN.
    */
  gridRow?: string,
  /**
    See [`grid-row-end`](https://developer.mozilla.org/docs/Web/CSS/grid-row-end) on MDN.
    */
  gridRowEnd?: string,
  /**
    See [`grid-row-start`](https://developer.mozilla.org/docs/Web/CSS/grid-row-start) on MDN.
    */
  gridRowStart?: string,
  /**
    See [`grid-template`](https://developer.mozilla.org/docs/Web/CSS/grid-template) on MDN.
    */
  gridTemplate?: string,
  /**
    See [`grid-template-areas`](https://developer.mozilla.org/docs/Web/CSS/grid-template-areas) on MDN.
    */
  gridTemplateAreas?: string,
  /**
    See [`grid-template-columns`](https://developer.mozilla.org/docs/Web/CSS/grid-template-columns) on MDN.
    */
  gridTemplateColumns?: string,
  /**
    See [`grid-template-rows`](https://developer.mozilla.org/docs/Web/CSS/grid-template-rows) on MDN.
    */
  gridTemplateRows?: string,
  /**
    See [`hanging-punctuation`](https://developer.mozilla.org/docs/Web/CSS/hanging-punctuation) on MDN.
    */
  hangingPunctuation?: string,
  /**
    See [`height`](https://developer.mozilla.org/docs/Web/CSS/height) on MDN.
    */
  height?: string,
  /**
    See [`hyphenate-character`](https://developer.mozilla.org/docs/Web/CSS/hyphenate-character) on MDN.
    */
  hyphenateCharacter?: string,
  /**
    See [`hyphenate-limit-chars`](https://developer.mozilla.org/docs/Web/CSS/hyphenate-limit-chars) on MDN.
    */
  hyphenateLimitChars?: string,
  /**
    See [`hyphens`](https://developer.mozilla.org/docs/Web/CSS/hyphens) on MDN.
    */
  hyphens?: string,
  /**
    See [`image-orientation`](https://developer.mozilla.org/docs/Web/CSS/image-orientation) on MDN.
    */
  imageOrientation?: string,
  /**
    See [`image-rendering`](https://developer.mozilla.org/docs/Web/CSS/image-rendering) on MDN.
    */
  imageRendering?: string,
  /**
    See [`initial-letter`](https://developer.mozilla.org/docs/Web/CSS/initial-letter) on MDN.
    */
  initialLetter?: string,
  /**
    See [`inline-size`](https://developer.mozilla.org/docs/Web/CSS/inline-size) on MDN.
    */
  inlineSize?: string,
  /**
    */
  inputSecurity?: string,
  /**
    See [`inset`](https://developer.mozilla.org/docs/Web/CSS/inset) on MDN.
    */
  inset?: string,
  /**
    See [`inset-block`](https://developer.mozilla.org/docs/Web/CSS/inset-block) on MDN.
    */
  insetBlock?: string,
  /**
    See [`inset-block-end`](https://developer.mozilla.org/docs/Web/CSS/inset-block-end) on MDN.
    */
  insetBlockEnd?: string,
  /**
    See [`inset-block-start`](https://developer.mozilla.org/docs/Web/CSS/inset-block-start) on MDN.
    */
  insetBlockStart?: string,
  /**
    See [`inset-inline`](https://developer.mozilla.org/docs/Web/CSS/inset-inline) on MDN.
    */
  insetInline?: string,
  /**
    See [`inset-inline-end`](https://developer.mozilla.org/docs/Web/CSS/inset-inline-end) on MDN.
    */
  insetInlineEnd?: string,
  /**
    See [`inset-inline-start`](https://developer.mozilla.org/docs/Web/CSS/inset-inline-start) on MDN.
    */
  insetInlineStart?: string,
  /**
    See [`isolation`](https://developer.mozilla.org/docs/Web/CSS/isolation) on MDN.
    */
  isolation?: string,
  /**
    See [`justify-content`](https://developer.mozilla.org/docs/Web/CSS/justify-content) on MDN.
    */
  justifyContent?: string,
  /**
    See [`justify-items`](https://developer.mozilla.org/docs/Web/CSS/justify-items) on MDN.
    */
  justifyItems?: string,
  /**
    See [`justify-self`](https://developer.mozilla.org/docs/Web/CSS/justify-self) on MDN.
    */
  justifySelf?: string,
  /**
    See [`left`](https://developer.mozilla.org/docs/Web/CSS/left) on MDN.
    */
  left?: string,
  /**
    See [`letter-spacing`](https://developer.mozilla.org/docs/Web/CSS/letter-spacing) on MDN.
    */
  letterSpacing?: string,
  /**
    See [`lighting-color`](https://developer.mozilla.org/docs/Web/CSS/lighting-color) on MDN.
    */
  lightingColor?: string,
  /**
    See [`line-break`](https://developer.mozilla.org/docs/Web/CSS/line-break) on MDN.
    */
  lineBreak?: string,
  /**
    */
  lineClamp?: string,
  /**
    See [`line-height`](https://developer.mozilla.org/docs/Web/CSS/line-height) on MDN.
    */
  lineHeight?: string,
  /**
    See [`list-style`](https://developer.mozilla.org/docs/Web/CSS/list-style) on MDN.
    */
  listStyle?: string,
  /**
    See [`list-style-image`](https://developer.mozilla.org/docs/Web/CSS/list-style-image) on MDN.
    */
  listStyleImage?: string,
  /**
    See [`list-style-position`](https://developer.mozilla.org/docs/Web/CSS/list-style-position) on MDN.
    */
  listStylePosition?: string,
  /**
    See [`list-style-type`](https://developer.mozilla.org/docs/Web/CSS/list-style-type) on MDN.
    */
  listStyleType?: string,
  /**
    See [`margin`](https://developer.mozilla.org/docs/Web/CSS/margin) on MDN.
    */
  margin?: string,
  /**
    See [`margin-block`](https://developer.mozilla.org/docs/Web/CSS/margin-block) on MDN.
    */
  marginBlock?: string,
  /**
    See [`margin-block-end`](https://developer.mozilla.org/docs/Web/CSS/margin-block-end) on MDN.
    */
  marginBlockEnd?: string,
  /**
    See [`margin-block-start`](https://developer.mozilla.org/docs/Web/CSS/margin-block-start) on MDN.
    */
  marginBlockStart?: string,
  /**
    See [`margin-bottom`](https://developer.mozilla.org/docs/Web/CSS/margin-bottom) on MDN.
    */
  marginBottom?: string,
  /**
    See [`margin-inline`](https://developer.mozilla.org/docs/Web/CSS/margin-inline) on MDN.
    */
  marginInline?: string,
  /**
    See [`margin-inline-end`](https://developer.mozilla.org/docs/Web/CSS/margin-inline-end) on MDN.
    */
  marginInlineEnd?: string,
  /**
    See [`margin-inline-start`](https://developer.mozilla.org/docs/Web/CSS/margin-inline-start) on MDN.
    */
  marginInlineStart?: string,
  /**
    See [`margin-left`](https://developer.mozilla.org/docs/Web/CSS/margin-left) on MDN.
    */
  marginLeft?: string,
  /**
    See [`margin-right`](https://developer.mozilla.org/docs/Web/CSS/margin-right) on MDN.
    */
  marginRight?: string,
  /**
    See [`margin-top`](https://developer.mozilla.org/docs/Web/CSS/margin-top) on MDN.
    */
  marginTop?: string,
  /**
    See [`marker`](https://developer.mozilla.org/docs/Web/CSS/marker) on MDN.
    */
  marker?: string,
  /**
    See [`marker-end`](https://developer.mozilla.org/docs/Web/CSS/marker-end) on MDN.
    */
  markerEnd?: string,
  /**
    See [`marker-mid`](https://developer.mozilla.org/docs/Web/CSS/marker-mid) on MDN.
    */
  markerMid?: string,
  /**
    See [`marker-start`](https://developer.mozilla.org/docs/Web/CSS/marker-start) on MDN.
    */
  markerStart?: string,
  /**
    See [`mask`](https://developer.mozilla.org/docs/Web/CSS/mask) on MDN.
    */
  mask?: string,
  /**
    See [`mask-border`](https://developer.mozilla.org/docs/Web/CSS/mask-border) on MDN.
    */
  maskBorder?: string,
  /**
    See [`mask-border-mode`](https://developer.mozilla.org/docs/Web/CSS/mask-border-mode) on MDN.
    */
  maskBorderMode?: string,
  /**
    See [`mask-border-outset`](https://developer.mozilla.org/docs/Web/CSS/mask-border-outset) on MDN.
    */
  maskBorderOutset?: string,
  /**
    See [`mask-border-repeat`](https://developer.mozilla.org/docs/Web/CSS/mask-border-repeat) on MDN.
    */
  maskBorderRepeat?: string,
  /**
    See [`mask-border-slice`](https://developer.mozilla.org/docs/Web/CSS/mask-border-slice) on MDN.
    */
  maskBorderSlice?: string,
  /**
    See [`mask-border-source`](https://developer.mozilla.org/docs/Web/CSS/mask-border-source) on MDN.
    */
  maskBorderSource?: string,
  /**
    See [`mask-border-width`](https://developer.mozilla.org/docs/Web/CSS/mask-border-width) on MDN.
    */
  maskBorderWidth?: string,
  /**
    See [`mask-clip`](https://developer.mozilla.org/docs/Web/CSS/mask-clip) on MDN.
    */
  maskClip?: string,
  /**
    See [`mask-composite`](https://developer.mozilla.org/docs/Web/CSS/mask-composite) on MDN.
    */
  maskComposite?: string,
  /**
    See [`mask-image`](https://developer.mozilla.org/docs/Web/CSS/mask-image) on MDN.
    */
  maskImage?: string,
  /**
    See [`mask-mode`](https://developer.mozilla.org/docs/Web/CSS/mask-mode) on MDN.
    */
  maskMode?: string,
  /**
    See [`mask-origin`](https://developer.mozilla.org/docs/Web/CSS/mask-origin) on MDN.
    */
  maskOrigin?: string,
  /**
    See [`mask-position`](https://developer.mozilla.org/docs/Web/CSS/mask-position) on MDN.
    */
  maskPosition?: string,
  /**
    See [`mask-repeat`](https://developer.mozilla.org/docs/Web/CSS/mask-repeat) on MDN.
    */
  maskRepeat?: string,
  /**
    See [`mask-size`](https://developer.mozilla.org/docs/Web/CSS/mask-size) on MDN.
    */
  maskSize?: string,
  /**
    See [`mask-type`](https://developer.mozilla.org/docs/Web/CSS/mask-type) on MDN.
    */
  maskType?: string,
  /**
    See [`math-depth`](https://developer.mozilla.org/docs/Web/CSS/math-depth) on MDN.
    */
  mathDepth?: string,
  /**
    See [`math-style`](https://developer.mozilla.org/docs/Web/CSS/math-style) on MDN.
    */
  mathStyle?: string,
  /**
    See [`max-block-size`](https://developer.mozilla.org/docs/Web/CSS/max-block-size) on MDN.
    */
  maxBlockSize?: string,
  /**
    See [`max-height`](https://developer.mozilla.org/docs/Web/CSS/max-height) on MDN.
    */
  maxHeight?: string,
  /**
    See [`max-inline-size`](https://developer.mozilla.org/docs/Web/CSS/max-inline-size) on MDN.
    */
  maxInlineSize?: string,
  /**
    See [`max-width`](https://developer.mozilla.org/docs/Web/CSS/max-width) on MDN.
    */
  maxWidth?: string,
  /**
    See [`min-block-size`](https://developer.mozilla.org/docs/Web/CSS/min-block-size) on MDN.
    */
  minBlockSize?: string,
  /**
    See [`min-height`](https://developer.mozilla.org/docs/Web/CSS/min-height) on MDN.
    */
  minHeight?: string,
  /**
    See [`min-inline-size`](https://developer.mozilla.org/docs/Web/CSS/min-inline-size) on MDN.
    */
  minInlineSize?: string,
  /**
    See [`min-width`](https://developer.mozilla.org/docs/Web/CSS/min-width) on MDN.
    */
  minWidth?: string,
  /**
    See [`mix-blend-mode`](https://developer.mozilla.org/docs/Web/CSS/mix-blend-mode) on MDN.
    */
  mixBlendMode?: string,
  /**
    See [`object-fit`](https://developer.mozilla.org/docs/Web/CSS/object-fit) on MDN.
    */
  objectFit?: string,
  /**
    See [`object-position`](https://developer.mozilla.org/docs/Web/CSS/object-position) on MDN.
    */
  objectPosition?: string,
  /**
    See [`offset`](https://developer.mozilla.org/docs/Web/CSS/offset) on MDN.
    */
  offset?: string,
  /**
    See [`offset-anchor`](https://developer.mozilla.org/docs/Web/CSS/offset-anchor) on MDN.
    */
  offsetAnchor?: string,
  /**
    See [`offset-distance`](https://developer.mozilla.org/docs/Web/CSS/offset-distance) on MDN.
    */
  offsetDistance?: string,
  /**
    See [`offset-path`](https://developer.mozilla.org/docs/Web/CSS/offset-path) on MDN.
    */
  offsetPath?: string,
  /**
    See [`offset-position`](https://developer.mozilla.org/docs/Web/CSS/offset-position) on MDN.
    */
  offsetPosition?: string,
  /**
    See [`offset-rotate`](https://developer.mozilla.org/docs/Web/CSS/offset-rotate) on MDN.
    */
  offsetRotate?: string,
  /**
    See [`opacity`](https://developer.mozilla.org/docs/Web/CSS/opacity) on MDN.
    */
  opacity?: string,
  /**
    See [`order`](https://developer.mozilla.org/docs/Web/CSS/order) on MDN.
    */
  order?: string,
  /**
    See [`orphans`](https://developer.mozilla.org/docs/Web/CSS/orphans) on MDN.
    */
  orphans?: string,
  /**
    See [`outline`](https://developer.mozilla.org/docs/Web/CSS/outline) on MDN.
    */
  outline?: string,
  /**
    See [`outline-color`](https://developer.mozilla.org/docs/Web/CSS/outline-color) on MDN.
    */
  outlineColor?: string,
  /**
    See [`outline-offset`](https://developer.mozilla.org/docs/Web/CSS/outline-offset) on MDN.
    */
  outlineOffset?: string,
  /**
    See [`outline-style`](https://developer.mozilla.org/docs/Web/CSS/outline-style) on MDN.
    */
  outlineStyle?: string,
  /**
    See [`outline-width`](https://developer.mozilla.org/docs/Web/CSS/outline-width) on MDN.
    */
  outlineWidth?: string,
  /**
    See [`overflow`](https://developer.mozilla.org/docs/Web/CSS/overflow) on MDN.
    */
  overflow?: string,
  /**
    See [`overflow-anchor`](https://developer.mozilla.org/docs/Web/CSS/overflow-anchor) on MDN.
    */
  overflowAnchor?: string,
  /**
    See [`overflow-block`](https://developer.mozilla.org/docs/Web/CSS/overflow-block) on MDN.
    */
  overflowBlock?: string,
  /**
    See [`overflow-clip-margin`](https://developer.mozilla.org/docs/Web/CSS/overflow-clip-margin) on MDN.
    */
  overflowClipMargin?: string,
  /**
    See [`overflow-inline`](https://developer.mozilla.org/docs/Web/CSS/overflow-inline) on MDN.
    */
  overflowInline?: string,
  /**
    See [`overflow-wrap`](https://developer.mozilla.org/docs/Web/CSS/overflow-wrap) on MDN.
    */
  overflowWrap?: string,
  /**
    See [`overflow-x`](https://developer.mozilla.org/docs/Web/CSS/overflow-x) on MDN.
    */
  overflowX?: string,
  /**
    See [`overflow-y`](https://developer.mozilla.org/docs/Web/CSS/overflow-y) on MDN.
    */
  overflowY?: string,
  /**
    See [`overscroll-behavior`](https://developer.mozilla.org/docs/Web/CSS/overscroll-behavior) on MDN.
    */
  overscrollBehavior?: string,
  /**
    See [`overscroll-behavior-block`](https://developer.mozilla.org/docs/Web/CSS/overscroll-behavior-block) on MDN.
    */
  overscrollBehaviorBlock?: string,
  /**
    See [`overscroll-behavior-inline`](https://developer.mozilla.org/docs/Web/CSS/overscroll-behavior-inline) on MDN.
    */
  overscrollBehaviorInline?: string,
  /**
    See [`overscroll-behavior-x`](https://developer.mozilla.org/docs/Web/CSS/overscroll-behavior-x) on MDN.
    */
  overscrollBehaviorX?: string,
  /**
    See [`overscroll-behavior-y`](https://developer.mozilla.org/docs/Web/CSS/overscroll-behavior-y) on MDN.
    */
  overscrollBehaviorY?: string,
  /**
    See [`padding`](https://developer.mozilla.org/docs/Web/CSS/padding) on MDN.
    */
  padding?: string,
  /**
    See [`padding-block`](https://developer.mozilla.org/docs/Web/CSS/padding-block) on MDN.
    */
  paddingBlock?: string,
  /**
    See [`padding-block-end`](https://developer.mozilla.org/docs/Web/CSS/padding-block-end) on MDN.
    */
  paddingBlockEnd?: string,
  /**
    See [`padding-block-start`](https://developer.mozilla.org/docs/Web/CSS/padding-block-start) on MDN.
    */
  paddingBlockStart?: string,
  /**
    See [`padding-bottom`](https://developer.mozilla.org/docs/Web/CSS/padding-bottom) on MDN.
    */
  paddingBottom?: string,
  /**
    See [`padding-inline`](https://developer.mozilla.org/docs/Web/CSS/padding-inline) on MDN.
    */
  paddingInline?: string,
  /**
    See [`padding-inline-end`](https://developer.mozilla.org/docs/Web/CSS/padding-inline-end) on MDN.
    */
  paddingInlineEnd?: string,
  /**
    See [`padding-inline-start`](https://developer.mozilla.org/docs/Web/CSS/padding-inline-start) on MDN.
    */
  paddingInlineStart?: string,
  /**
    See [`padding-left`](https://developer.mozilla.org/docs/Web/CSS/padding-left) on MDN.
    */
  paddingLeft?: string,
  /**
    See [`padding-right`](https://developer.mozilla.org/docs/Web/CSS/padding-right) on MDN.
    */
  paddingRight?: string,
  /**
    See [`padding-top`](https://developer.mozilla.org/docs/Web/CSS/padding-top) on MDN.
    */
  paddingTop?: string,
  /**
    See [`page`](https://developer.mozilla.org/docs/Web/CSS/page) on MDN.
    */
  page?: string,
  /**
    See [`page-break-after`](https://developer.mozilla.org/docs/Web/CSS/page-break-after) on MDN.
    */
  pageBreakAfter?: string,
  /**
    See [`page-break-before`](https://developer.mozilla.org/docs/Web/CSS/page-break-before) on MDN.
    */
  pageBreakBefore?: string,
  /**
    See [`page-break-inside`](https://developer.mozilla.org/docs/Web/CSS/page-break-inside) on MDN.
    */
  pageBreakInside?: string,
  /**
    See [`paint-order`](https://developer.mozilla.org/docs/Web/CSS/paint-order) on MDN.
    */
  paintOrder?: string,
  /**
    See [`perspective`](https://developer.mozilla.org/docs/Web/CSS/perspective) on MDN.
    */
  perspective?: string,
  /**
    See [`perspective-origin`](https://developer.mozilla.org/docs/Web/CSS/perspective-origin) on MDN.
    */
  perspectiveOrigin?: string,
  /**
    See [`place-content`](https://developer.mozilla.org/docs/Web/CSS/place-content) on MDN.
    */
  placeContent?: string,
  /**
    See [`place-items`](https://developer.mozilla.org/docs/Web/CSS/place-items) on MDN.
    */
  placeItems?: string,
  /**
    See [`place-self`](https://developer.mozilla.org/docs/Web/CSS/place-self) on MDN.
    */
  placeSelf?: string,
  /**
    See [`pointer-events`](https://developer.mozilla.org/docs/Web/CSS/pointer-events) on MDN.
    */
  pointerEvents?: string,
  /**
    See [`position`](https://developer.mozilla.org/docs/Web/CSS/position) on MDN.
    */
  position?: string,
  /**
    See [`print-color-adjust`](https://developer.mozilla.org/docs/Web/CSS/print-color-adjust) on MDN.
    */
  printColorAdjust?: string,
  /**
    See [`quotes`](https://developer.mozilla.org/docs/Web/CSS/quotes) on MDN.
    */
  quotes?: string,
  /**
    See [`r`](https://developer.mozilla.org/docs/Web/CSS/r) on MDN.
    */
  r?: string,
  /**
    See [`resize`](https://developer.mozilla.org/docs/Web/CSS/resize) on MDN.
    */
  resize?: string,
  /**
    See [`right`](https://developer.mozilla.org/docs/Web/CSS/right) on MDN.
    */
  right?: string,
  /**
    See [`rotate`](https://developer.mozilla.org/docs/Web/CSS/rotate) on MDN.
    */
  rotate?: string,
  /**
    See [`row-gap`](https://developer.mozilla.org/docs/Web/CSS/row-gap) on MDN.
    */
  rowGap?: string,
  /**
    See [`ruby-align`](https://developer.mozilla.org/docs/Web/CSS/ruby-align) on MDN.
    */
  rubyAlign?: string,
  /**
    See [`ruby-position`](https://developer.mozilla.org/docs/Web/CSS/ruby-position) on MDN.
    */
  rubyPosition?: string,
  /**
    See [`rx`](https://developer.mozilla.org/docs/Web/CSS/rx) on MDN.
    */
  rx?: string,
  /**
    See [`ry`](https://developer.mozilla.org/docs/Web/CSS/ry) on MDN.
    */
  ry?: string,
  /**
    See [`scale`](https://developer.mozilla.org/docs/Web/CSS/scale) on MDN.
    */
  scale?: string,
  /**
    See [`scroll-behavior`](https://developer.mozilla.org/docs/Web/CSS/scroll-behavior) on MDN.
    */
  scrollBehavior?: string,
  /**
    See [`scroll-margin`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin) on MDN.
    */
  scrollMargin?: string,
  /**
    See [`scroll-margin-block`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-block) on MDN.
    */
  scrollMarginBlock?: string,
  /**
    See [`scroll-margin-block-end`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-block-end) on MDN.
    */
  scrollMarginBlockEnd?: string,
  /**
    See [`scroll-margin-block-start`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-block-start) on MDN.
    */
  scrollMarginBlockStart?: string,
  /**
    See [`scroll-margin-bottom`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-bottom) on MDN.
    */
  scrollMarginBottom?: string,
  /**
    See [`scroll-margin-inline`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-inline) on MDN.
    */
  scrollMarginInline?: string,
  /**
    See [`scroll-margin-inline-end`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-inline-end) on MDN.
    */
  scrollMarginInlineEnd?: string,
  /**
    See [`scroll-margin-inline-start`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-inline-start) on MDN.
    */
  scrollMarginInlineStart?: string,
  /**
    See [`scroll-margin-left`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-left) on MDN.
    */
  scrollMarginLeft?: string,
  /**
    See [`scroll-margin-right`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-right) on MDN.
    */
  scrollMarginRight?: string,
  /**
    See [`scroll-margin-top`](https://developer.mozilla.org/docs/Web/CSS/scroll-margin-top) on MDN.
    */
  scrollMarginTop?: string,
  /**
    See [`scroll-padding`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding) on MDN.
    */
  scrollPadding?: string,
  /**
    See [`scroll-padding-block`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-block) on MDN.
    */
  scrollPaddingBlock?: string,
  /**
    See [`scroll-padding-block-end`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-block-end) on MDN.
    */
  scrollPaddingBlockEnd?: string,
  /**
    See [`scroll-padding-block-start`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-block-start) on MDN.
    */
  scrollPaddingBlockStart?: string,
  /**
    See [`scroll-padding-bottom`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-bottom) on MDN.
    */
  scrollPaddingBottom?: string,
  /**
    See [`scroll-padding-inline`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-inline) on MDN.
    */
  scrollPaddingInline?: string,
  /**
    See [`scroll-padding-inline-end`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-inline-end) on MDN.
    */
  scrollPaddingInlineEnd?: string,
  /**
    See [`scroll-padding-inline-start`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-inline-start) on MDN.
    */
  scrollPaddingInlineStart?: string,
  /**
    See [`scroll-padding-left`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-left) on MDN.
    */
  scrollPaddingLeft?: string,
  /**
    See [`scroll-padding-right`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-right) on MDN.
    */
  scrollPaddingRight?: string,
  /**
    See [`scroll-padding-top`](https://developer.mozilla.org/docs/Web/CSS/scroll-padding-top) on MDN.
    */
  scrollPaddingTop?: string,
  /**
    See [`scroll-snap-align`](https://developer.mozilla.org/docs/Web/CSS/scroll-snap-align) on MDN.
    */
  scrollSnapAlign?: string,
  /**
    See [`scroll-snap-stop`](https://developer.mozilla.org/docs/Web/CSS/scroll-snap-stop) on MDN.
    */
  scrollSnapStop?: string,
  /**
    See [`scroll-snap-type`](https://developer.mozilla.org/docs/Web/CSS/scroll-snap-type) on MDN.
    */
  scrollSnapType?: string,
  /**
    See [`scrollbar-color`](https://developer.mozilla.org/docs/Web/CSS/scrollbar-color) on MDN.
    */
  scrollbarColor?: string,
  /**
    See [`scrollbar-gutter`](https://developer.mozilla.org/docs/Web/CSS/scrollbar-gutter) on MDN.
    */
  scrollbarGutter?: string,
  /**
    See [`scrollbar-width`](https://developer.mozilla.org/docs/Web/CSS/scrollbar-width) on MDN.
    */
  scrollbarWidth?: string,
  /**
    See [`shape-image-threshold`](https://developer.mozilla.org/docs/Web/CSS/shape-image-threshold) on MDN.
    */
  shapeImageThreshold?: string,
  /**
    See [`shape-margin`](https://developer.mozilla.org/docs/Web/CSS/shape-margin) on MDN.
    */
  shapeMargin?: string,
  /**
    See [`shape-outside`](https://developer.mozilla.org/docs/Web/CSS/shape-outside) on MDN.
    */
  shapeOutside?: string,
  /**
    See [`shape-rendering`](https://developer.mozilla.org/docs/Web/CSS/shape-rendering) on MDN.
    */
  shapeRendering?: string,
  /**
    See [`stop-color`](https://developer.mozilla.org/docs/Web/CSS/stop-color) on MDN.
    */
  stopColor?: string,
  /**
    See [`stop-opacity`](https://developer.mozilla.org/docs/Web/CSS/stop-opacity) on MDN.
    */
  stopOpacity?: string,
  /**
    See [`stroke`](https://developer.mozilla.org/docs/Web/CSS/stroke) on MDN.
    */
  stroke?: string,
  /**
    See [`stroke-dasharray`](https://developer.mozilla.org/docs/Web/CSS/stroke-dasharray) on MDN.
    */
  strokeDasharray?: string,
  /**
    See [`stroke-dashoffset`](https://developer.mozilla.org/docs/Web/CSS/stroke-dashoffset) on MDN.
    */
  strokeDashoffset?: string,
  /**
    See [`stroke-linecap`](https://developer.mozilla.org/docs/Web/CSS/stroke-linecap) on MDN.
    */
  strokeLinecap?: string,
  /**
    See [`stroke-linejoin`](https://developer.mozilla.org/docs/Web/CSS/stroke-linejoin) on MDN.
    */
  strokeLinejoin?: string,
  /**
    See [`stroke-miterlimit`](https://developer.mozilla.org/docs/Web/CSS/stroke-miterlimit) on MDN.
    */
  strokeMiterlimit?: string,
  /**
    See [`stroke-opacity`](https://developer.mozilla.org/docs/Web/CSS/stroke-opacity) on MDN.
    */
  strokeOpacity?: string,
  /**
    See [`stroke-width`](https://developer.mozilla.org/docs/Web/CSS/stroke-width) on MDN.
    */
  strokeWidth?: string,
  /**
    See [`tab-size`](https://developer.mozilla.org/docs/Web/CSS/tab-size) on MDN.
    */
  tabSize?: string,
  /**
    See [`table-layout`](https://developer.mozilla.org/docs/Web/CSS/table-layout) on MDN.
    */
  tableLayout?: string,
  /**
    See [`text-align`](https://developer.mozilla.org/docs/Web/CSS/text-align) on MDN.
    */
  textAlign?: string,
  /**
    See [`text-align-last`](https://developer.mozilla.org/docs/Web/CSS/text-align-last) on MDN.
    */
  textAlignLast?: string,
  /**
    See [`text-anchor`](https://developer.mozilla.org/docs/Web/CSS/text-anchor) on MDN.
    */
  textAnchor?: string,
  /**
    */
  textBox?: string,
  /**
    */
  textBoxEdge?: string,
  /**
    */
  textBoxTrim?: string,
  /**
    See [`text-combine-upright`](https://developer.mozilla.org/docs/Web/CSS/text-combine-upright) on MDN.
    */
  textCombineUpright?: string,
  /**
    See [`text-decoration`](https://developer.mozilla.org/docs/Web/CSS/text-decoration) on MDN.
    */
  textDecoration?: string,
  /**
    See [`text-decoration-color`](https://developer.mozilla.org/docs/Web/CSS/text-decoration-color) on MDN.
    */
  textDecorationColor?: string,
  /**
    See [`text-decoration-line`](https://developer.mozilla.org/docs/Web/CSS/text-decoration-line) on MDN.
    */
  textDecorationLine?: string,
  /**
    See [`text-decoration-skip-ink`](https://developer.mozilla.org/docs/Web/CSS/text-decoration-skip-ink) on MDN.
    */
  textDecorationSkipInk?: string,
  /**
    See [`text-decoration-style`](https://developer.mozilla.org/docs/Web/CSS/text-decoration-style) on MDN.
    */
  textDecorationStyle?: string,
  /**
    See [`text-decoration-thickness`](https://developer.mozilla.org/docs/Web/CSS/text-decoration-thickness) on MDN.
    */
  textDecorationThickness?: string,
  /**
    See [`text-emphasis`](https://developer.mozilla.org/docs/Web/CSS/text-emphasis) on MDN.
    */
  textEmphasis?: string,
  /**
    See [`text-emphasis-color`](https://developer.mozilla.org/docs/Web/CSS/text-emphasis-color) on MDN.
    */
  textEmphasisColor?: string,
  /**
    See [`text-emphasis-position`](https://developer.mozilla.org/docs/Web/CSS/text-emphasis-position) on MDN.
    */
  textEmphasisPosition?: string,
  /**
    See [`text-emphasis-style`](https://developer.mozilla.org/docs/Web/CSS/text-emphasis-style) on MDN.
    */
  textEmphasisStyle?: string,
  /**
    See [`text-indent`](https://developer.mozilla.org/docs/Web/CSS/text-indent) on MDN.
    */
  textIndent?: string,
  /**
    See [`text-justify`](https://developer.mozilla.org/docs/Web/CSS/text-justify) on MDN.
    */
  textJustify?: string,
  /**
    See [`text-orientation`](https://developer.mozilla.org/docs/Web/CSS/text-orientation) on MDN.
    */
  textOrientation?: string,
  /**
    See [`text-overflow`](https://developer.mozilla.org/docs/Web/CSS/text-overflow) on MDN.
    */
  textOverflow?: string,
  /**
    See [`text-rendering`](https://developer.mozilla.org/docs/Web/CSS/text-rendering) on MDN.
    */
  textRendering?: string,
  /**
    See [`text-shadow`](https://developer.mozilla.org/docs/Web/CSS/text-shadow) on MDN.
    */
  textShadow?: string,
  /**
    See [`text-transform`](https://developer.mozilla.org/docs/Web/CSS/text-transform) on MDN.
    */
  textTransform?: string,
  /**
    See [`text-underline-offset`](https://developer.mozilla.org/docs/Web/CSS/text-underline-offset) on MDN.
    */
  textUnderlineOffset?: string,
  /**
    See [`text-underline-position`](https://developer.mozilla.org/docs/Web/CSS/text-underline-position) on MDN.
    */
  textUnderlinePosition?: string,
  /**
    See [`text-wrap`](https://developer.mozilla.org/docs/Web/CSS/text-wrap) on MDN.
    */
  textWrap?: string,
  /**
    See [`text-wrap-mode`](https://developer.mozilla.org/docs/Web/CSS/text-wrap-mode) on MDN.
    */
  textWrapMode?: string,
  /**
    See [`text-wrap-style`](https://developer.mozilla.org/docs/Web/CSS/text-wrap-style) on MDN.
    */
  textWrapStyle?: string,
  /**
    See [`top`](https://developer.mozilla.org/docs/Web/CSS/top) on MDN.
    */
  top?: string,
  /**
    See [`touch-action`](https://developer.mozilla.org/docs/Web/CSS/touch-action) on MDN.
    */
  touchAction?: string,
  /**
    See [`transform`](https://developer.mozilla.org/docs/Web/CSS/transform) on MDN.
    */
  transform?: string,
  /**
    See [`transform-box`](https://developer.mozilla.org/docs/Web/CSS/transform-box) on MDN.
    */
  transformBox?: string,
  /**
    See [`transform-origin`](https://developer.mozilla.org/docs/Web/CSS/transform-origin) on MDN.
    */
  transformOrigin?: string,
  /**
    See [`transform-style`](https://developer.mozilla.org/docs/Web/CSS/transform-style) on MDN.
    */
  transformStyle?: string,
  /**
    See [`transition`](https://developer.mozilla.org/docs/Web/CSS/transition) on MDN.
    */
  transition?: string,
  /**
    See [`transition-behavior`](https://developer.mozilla.org/docs/Web/CSS/transition-behavior) on MDN.
    */
  transitionBehavior?: string,
  /**
    See [`transition-delay`](https://developer.mozilla.org/docs/Web/CSS/transition-delay) on MDN.
    */
  transitionDelay?: string,
  /**
    See [`transition-duration`](https://developer.mozilla.org/docs/Web/CSS/transition-duration) on MDN.
    */
  transitionDuration?: string,
  /**
    See [`transition-property`](https://developer.mozilla.org/docs/Web/CSS/transition-property) on MDN.
    */
  transitionProperty?: string,
  /**
    See [`transition-timing-function`](https://developer.mozilla.org/docs/Web/CSS/transition-timing-function) on MDN.
    */
  transitionTimingFunction?: string,
  /**
    See [`translate`](https://developer.mozilla.org/docs/Web/CSS/translate) on MDN.
    */
  translate?: string,
  /**
    See [`unicode-bidi`](https://developer.mozilla.org/docs/Web/CSS/unicode-bidi) on MDN.
    */
  unicodeBidi?: string,
  /**
    See [`user-select`](https://developer.mozilla.org/docs/Web/CSS/user-select) on MDN.
    */
  userSelect?: string,
  /**
    See [`vector-effect`](https://developer.mozilla.org/docs/Web/CSS/vector-effect) on MDN.
    */
  vectorEffect?: string,
  /**
    See [`vertical-align`](https://developer.mozilla.org/docs/Web/CSS/vertical-align) on MDN.
    */
  verticalAlign?: string,
  /**
    See [`view-transition-name`](https://developer.mozilla.org/docs/Web/CSS/view-transition-name) on MDN.
    */
  viewTransitionName?: string,
  /**
    See [`visibility`](https://developer.mozilla.org/docs/Web/CSS/visibility) on MDN.
    */
  visibility?: string,
  /**
    See [`white-space`](https://developer.mozilla.org/docs/Web/CSS/white-space) on MDN.
    */
  whiteSpace?: string,
  /**
    See [`white-space-collapse`](https://developer.mozilla.org/docs/Web/CSS/white-space-collapse) on MDN.
    */
  whiteSpaceCollapse?: string,
  /**
    See [`widows`](https://developer.mozilla.org/docs/Web/CSS/widows) on MDN.
    */
  widows?: string,
  /**
    See [`width`](https://developer.mozilla.org/docs/Web/CSS/width) on MDN.
    */
  width?: string,
  /**
    See [`will-change`](https://developer.mozilla.org/docs/Web/CSS/will-change) on MDN.
    */
  willChange?: string,
  /**
    See [`word-break`](https://developer.mozilla.org/docs/Web/CSS/word-break) on MDN.
    */
  wordBreak?: string,
  /**
    See [`word-spacing`](https://developer.mozilla.org/docs/Web/CSS/word-spacing) on MDN.
    */
  wordSpacing?: string,
  /**
    See [`word-wrap`](https://developer.mozilla.org/docs/Web/CSS/overflow-wrap) on MDN.
    */
  wordWrap?: string,
  /**
    See [`writing-mode`](https://developer.mozilla.org/docs/Web/CSS/writing-mode) on MDN.
    */
  writingMode?: string,
  /**
    See [`x`](https://developer.mozilla.org/docs/Web/CSS/x) on MDN.
    */
  x?: string,
  /**
    See [`y`](https://developer.mozilla.org/docs/Web/CSS/y) on MDN.
    */
  y?: string,
  /**
    See [`z-index`](https://developer.mozilla.org/docs/Web/CSS/z-index) on MDN.
    */
  zIndex?: string,
  /**
    See [`zoom`](https://developer.mozilla.org/docs/Web/CSS/zoom) on MDN.
    */
  zoom?: string,
  /* Experimental properties: */
  imageResolution?: string,
  textDecorationSkip?: string,
  maxLines?: string,
  rubyMerge?: string,
  /* Non-standard properties: */
  azimuth?: string,
  clip?: string,
  cue?: string,
  cueAfter?: string,
  cueBefore?: string,
  elevation?: string,
  markerOffset?: string,
  marks?: string,
  pause?: string,
  pauseAfter?: string,
  pauseBefore?: string,
  pitch?: string,
  pitchRange?: string,
  playDuring?: string,
  richness?: string,
  size?: string,
  speak?: string,
  speakHeader?: string,
  speakNumeral?: string,
  speakPunctuation?: string,
  speechRate?: string,
  stress?: string,
  voiceFamily?: string,
  volume?: string,
  rest?: string,
  restAfter?: string,
  restBefore?: string,
  speakAs?: string,
  voiceBalance?: string,
  voiceDuration?: string,
  voicePitch?: string,
  voiceRange?: string,
  voiceRate?: string,
  voiceStress?: string,
  voiceVolume?: string,
  forntVariantAlternates?: string,
  glyphOrientationVertical?: string,
  navDown?: string,
  navLeft?: string,
  navRight?: string,
  navUp?: string,
  gridColumnGap?: string,
  gridGap?: string,
  gridRowGap?: string,
  caretAnimation?: string,
  marqueeDirection?: string,
  marqueeLoop?: string,
  marqueeSpeed?: string,
  marqueeStyle?: string,
  overflowStyle?: string,
  rotation?: string,
  rotationPoint?: string,
  alignmentBaseline?: string,
  baselineShift?: string,
  colorInterpolation?: string,
  colorProfile?: string,
  colorRendering?: string,
  glyphOrientationHorizontal?: string,
  kerning?: string,
}
