module AnimationEvent = Webapi__Dom__AnimationEvent
module Attr = Webapi__Dom__Attr
module BeforeUnloadEvent = Webapi__Dom__BeforeUnloadEvent
module CdataSection = Webapi__Dom__CdataSection
module CharacterData = Webapi__Dom__CharacterData
module Comment = Webapi__Dom__Comment
module CssStyleDeclaration = Webapi__Dom__CssStyleDeclaration
module ClipboardEvent = Webapi__Dom__ClipboardEvent
module CloseEvent = Webapi__Dom__CloseEvent
module CompositionEvent = Webapi__Dom__CompositionEvent
module CustomEvent = Webapi__Dom__CustomEvent
module Document = Webapi__Dom__Document
module DocumentFragment = Webapi__Dom__DocumentFragment
module DocumentType = Webapi__Dom__DocumentType
module DomImplementation = Webapi__Dom__DomImplementation
module DomRect = Webapi__Dom__DomRect
module DomStringMap = Webapi__Dom__DomStringMap
module DomTokenList = Webapi__Dom__DomTokenList
module DragEvent = Webapi__Dom__DragEvent
module Element = Webapi__Dom__Element
module ErrorEvent = Webapi__Dom__ErrorEvent
module Event = Webapi__Dom__Event
module EventTarget = Webapi__Dom__EventTarget
module FocusEvent = Webapi__Dom__FocusEvent
module History = Webapi__Dom__History
module HtmlCollection = Webapi__Dom__HtmlCollection
module HtmlDocument = Webapi__Dom__HtmlDocument
module HtmlElement = Webapi__Dom__HtmlElement
module HtmlFormElement = Webapi__Dom__HtmlFormElement
module HtmlImageElement = Webapi__Dom__HtmlImageElement
module HtmlInputElement = Webapi__Dom__HtmlInputElement
module IdbVersionChangeEvent = Webapi__Dom__IdbVersionChangeEvent
module Image = Webapi__Dom__Image
module InputEvent = Webapi__Dom__InputEvent
module KeyboardEvent = Webapi__Dom__KeyboardEvent
module Location = Webapi__Dom__Location
module MouseEvent = Webapi__Dom__MouseEvent
module MutationObserver = Webapi__Dom__MutationObserver
module MutationRecord = Webapi__Dom__MutationRecord
module NamedNodeMap = Webapi__Dom__NamedNodeMap
module Node = Webapi__Dom__Node
module NodeFilter = Webapi__Dom__NodeFilter
module NodeIterator = Webapi__Dom__NodeIterator
module NodeList = Webapi__Dom__NodeList
module PageTransitionEvent = Webapi__Dom__PageTransitionEvent
module PointerEvent = Webapi__Dom__PointerEvent
module PopStateEvent = Webapi__Dom__PopStateEvent
module ProcessingInstruction = Webapi__Dom__ProcessingInstruction
module ProgressEvent = Webapi__Dom__ProgressEvent
module Range = Webapi__Dom__Range
module RelatedEvent = Webapi__Dom__RelatedEvent
module Selection = Webapi__Dom__Selection
module ShadowRoot = Webapi__Dom__ShadowRoot
module StorageEvent = Webapi__Dom__StorageEvent
module SvgZoomEvent = Webapi__Dom__SvgZoomEvent
module Text = Webapi__Dom__Text
module TimeEvent = Webapi__Dom__TimeEvent
module TouchEvent = Webapi__Dom__TouchEvent
module TrackEvent = Webapi__Dom__TrackEvent
module TransitionEvent = Webapi__Dom__TransitionEvent
module TreeWalker = Webapi__Dom__TreeWalker
module UiEvent = Webapi__Dom__UiEvent
module ValidityState = Webapi__Dom__ValidityState
module WebGlContextEvent = Webapi__Dom__WebGlContextEvent
module WheelEvent = Webapi__Dom__WheelEvent
module Window = Webapi__Dom__Window

include Webapi__Dom__Types

@val external window: Dom.window = "window"
@val external document: Dom.document = "document"
@val @scope("window") external history: Dom.history = "history"
@val @scope("window") external location: Dom.location = "location"

/* Unimplemented interfaces (aka. "The TODO list")

   Attr
   CharacterData
   ChildNode /* experimental */
   Comment
   DocumentFragment
   DocumentType
   DOMError
   DOMException
   DOMImplementation
   DOMTimeStamp
   DOMSettableTokenList /* deprecated, merged with DOMTokenList */
   DOMStringList
   MutationObserver
   MutationRecord
   NodeIterator
   ParentNode /* experimental */
   ProcessingInstruction
   Text
   TreeWalker
   URL
   Worker
   XMLDocument /* experimental */

   /* HTML Elements */
   HTMLAnchorElement
   HTMLAppletElement
   HTMLAreaElement
   HTMLAudioElement
   HTMLBaseElement
   HTMLBodyElement
   HTMLBRElement
   HTMLButtonELement
   HTMLCanvasElement
   HTMLDataElement
   HTMLDataListElement
   HTMLDialogElement
   HTMLDirectoryElement
   HTMLDivElement
   HTMLDListElement
   HTMLEmbedElement
   HTMLFieldSetElement
   HTMLFontElement
   HTMLFormElement
   HTMLFrameElement
   HTMLFrameSetElement
   HTMLHeadElement
   HTMLHeadingElement
   HTMLHtmlElement
   HTMLHRElement
   HTMLIFrameElement
   HTMLImageElement
   HTMLInputElement
   HTMLKeygenElement
   HTMLLabelElement
   HTMLLegendElement
   HTMLLIElement
   HTMLLinkElement
   HTMLMapElement
   HTMLMediaElement
   HTMLMenuElement
   HTMLMetaElement
   HTMLMeterElement
   HTMLModElement
   HTMLObjectElement
   HTMLOListElement
   HTMLOptGroupElement
   HTMLOptionElement
   HTMLOutputElement
   HTMLParagraphElement
   HTMLParamElement
   HTMLPreElement
   HTMLProgressElement
   HTMLQuoteElement
   HTMLScriptElement
   HTMLSelectElement
   HTMLSourceElement
   HTMLSpanElement
   HTMLStyleElement
   HTMLTableElement
   HTMLTableCaptionElement
   HTMLTableCellElement
   HTMLTableDataCellElement
   HTMLTableHeaderCellElement
   HTMLTableColElement
   HTMLTableRowElement
   HTMLTableSectionElement
   HTMLTextAreaElement
   HTMLTimeElement
   HTMLTitleElement
   HTMLTrackElement
   HTMLUListElement
   HTMLUnknownElement
   HTMLVideoElement

   /* Other interfaces */
   CanvasRenderingContext2D
   CanvasGradient
   CanvasPattern
   TextMetrics
   ImageData
   CanvasPixelArray
   NotifyAudioAvailableEvent
   HTMLAllCollection
   HTMLFormControlsCollection
   HTMLOptionsCollection
   HTMLPropertiesCollection
   DOMStringMap
   RadioNodeList
   MediaError

   /* SVG Element interfaces */
   SVGAElement
   SVGAltGlyphElement
   SVGAltGlyphDefElement
   SVGAltGlyphItemElement
   SVGAnimationElement
   SVGAnimateElement
   SVGAnimateColorElement
   SVGAnimateMotionElement
   SVGAnimateTransformElement
   SVGCircleElement
   SVGClipPathElement
   SVGColorProfileElement
   SVGComponentTransferFunctionElement
   SVGCursorElement
   SVGDefsElement
   SVGDescElement
   SVGElement
   SVGEllipseElement
   SVGFEBlendElement
   SVGFEColorMatrixElement
   SVGFEComponentTransferElement
   SVGFECompositeElement
   SVGFEConvolveMatrixElement
   SVGFEDiffuseLightingElement
   SVGFEDisplacementMapElement
   SVGFEDistantLightElement
   SVGFEFloodElement
   SVGFEGaussianBlurElement
   SVGFEImageElement
   SVGFEMergeElement
   SVGFEMergeNodeElement
   SVGFEMorphologyElement
   SVGFEOffsetElement
   SVGFEPointLightElement
   SVGFESpecularLightingElement
   SVGFESpotLightElement
   SVGFETileElement
   SVGFETurbulenceElement
   SVGFEFuncRElement
   SVGFEFuncGElement
   SVGFEFuncBElement
   SVGFEFuncAElement
   SVGFilterElement
   SVGFilterPrimitiveStandardAttributes
   SVGFontElement
   SVGFontFaceElement
   SVGFontFaceFormatElement
   SVGFontFaceNameElement
   SVGFontFaceSrcElement
   SVGFontFaceUriElement
   SVGForeignObjectElement
   SVGGElement
   SVGGlyphElement
   SVGGlyphRefElement
   SVGGradientElement
   SVGHKernElement
   SVGImageElement
   SVGLinearGradientElement
   SVGLineElement
   SVGMarkerElement
   SVGMaskElement
   SVGMetadataElement
   SVGMissingGlyphElement
   SVGMPathElement
   SVGPathElement
   SVGPatternElement
   SVGPolylineElement
   SVGPolygonElement
   SVGRadialGradientElement
   SVGRectElement
   SVGScriptElement
   SVGSetElement
   SVGStopElement
   SVGStyleElement
   SVGSVGElement
   SVGSwitchElement
   SVGSymbolElement
   SVGTextElement
   SVGTextPathElement
   SVGTitleElement
   SVGTRefElement
   SVGTSpanElement
   SVGUseElement
   SVGViewElement
   SVGVKernElement

   /* SVG data type interfaces */

   /* Static type */
   SVGAngle
   SVGColor
   SVGICCColor
   SVGElementInstance
   SVGElementInstanceList
   SVGLength
   SVGLengthList
   SVGMatrix
   SVGNumber
   SVGNumberList
   SVGPaint
   SVGPoint
   SVGPointList
   SVGPreserveAspectRatio
   SVGRect
   SVGStringList
   SVGTransform
   SVGTransformList

   /* Animated type */
   SVGAnimatedAngle
   SVGAnimatedBoolean
   SVGAnimatedEnumeration
   SVGAnimatedInteger
   SVGAnimatedLength
   SVGAnimatedLengthList
   SVGAnimatedNumber
   SVGAnimatedNumberList
   SVGAnimatedPreserveAspectRatio
   SVGAnimatedRect
   SVGAnimatedString
   SVGAnimatedTransformList

   /* SIML related interfaces */
   ElementTimeControl
   TimeEvent

   /* Other SVG interfaces */
   SVGAnimatedPathData
   SVGAnimatedPoints
   SVGColorProfileRule
   SVGCSSRule
   SVGExternalResourcesRequired
   SVGFitToViewBox
   SVGLangSpace
   SVGLocatable
   SVGRenderingIntent
   SVGStylable
   SVGTests
   SVGTextContentElement
   SVGTextPositioningElement
   SVGTransformable
   SVGUnitTypes
   SVGURIReference
   SVGViewSpec
   SVGZoomAndPan

   /* obsolete interfaces skipped */
 */
