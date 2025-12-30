import type * as rescript from "@rescript/runtime/types";

export type _baseClass = rescript.opaque<
  "Dom._baseClass",
  []
>;

export type animation = rescript.opaque<
  "Dom.animation",
  []
>;

export type cssStyleDeclaration = rescript.opaque<
  "Dom.cssStyleDeclaration",
  []
>;

export type cssStyleSheet = rescript.opaque<
  "Dom.cssStyleSheet",
  []
>;

export type eventTarget_like<A> = rescript.opaque<
  "Dom.eventTarget_like",
  [A]
>;

export type eventTarget = eventTarget_like<_baseClass>;

export type _node<A> = rescript.opaque<
  "Dom._node",
  [A]
>;

export type node_like<A> = eventTarget_like<_node<A>>;

export type node = node_like<_baseClass>;

export type _attr = rescript.opaque<
  "Dom._attr",
  []
>;

export type attr = node_like<_attr>;

export type _characterData<A> = rescript.opaque<
  "Dom._characterData",
  [A]
>;

export type characterData_like<A> = node_like<_characterData<A>>;

export type characterData = characterData_like<_baseClass>;

export type _cdataSection = rescript.opaque<
  "Dom._cdataSection",
  []
>;

export type cdataSection = characterData_like<_cdataSection>;

export type _comment = rescript.opaque<
  "Dom._comment",
  []
>;

export type comment = characterData_like<_comment>;

export type _document<A> = rescript.opaque<
  "Dom._document",
  [A]
>;

export type document_like<A> = node_like<_document<A>>;

export type document = document_like<_baseClass>;

export type _documentFragment = rescript.opaque<
  "Dom._documentFragment",
  []
>;

export type documentFragment = node_like<_documentFragment>;

export type _documentType = rescript.opaque<
  "Dom._documentType",
  []
>;

export type documentType = node_like<_documentType>;

export type domImplementation = rescript.opaque<
  "Dom.domImplementation",
  []
>;

export type _element<A> = rescript.opaque<
  "Dom._element",
  [A]
>;

export type element_like<A> = node_like<_element<A>>;

export type element = element_like<_baseClass>;

export type htmlCollection = rescript.opaque<
  "Dom.htmlCollection",
  []
>;

export type htmlFormControlsCollection = rescript.opaque<
  "Dom.htmlFormControlsCollection",
  []
>;

export type htmlOptionsCollection = rescript.opaque<
  "Dom.htmlOptionsCollection",
  []
>;

export type intersectionObserver = rescript.opaque<
  "Dom.intersectionObserver",
  []
>;

export type intersectionObserverEntry = rescript.opaque<
  "Dom.intersectionObserverEntry",
  []
>;

export type mutationObserver = rescript.opaque<
  "Dom.mutationObserver",
  []
>;

export type mutationRecord = rescript.opaque<
  "Dom.mutationRecord",
  []
>;

export type performanceObserver = rescript.opaque<
  "Dom.performanceObserver",
  []
>;

export type performanceObserverEntryList = rescript.opaque<
  "Dom.performanceObserverEntryList",
  []
>;

export type reportingObserver = rescript.opaque<
  "Dom.reportingObserver",
  []
>;

export type reportingObserverOptions = rescript.opaque<
  "Dom.reportingObserverOptions",
  []
>;

export type resizeObserver = rescript.opaque<
  "Dom.resizeObserver",
  []
>;

export type resizeObserverEntry = rescript.opaque<
  "Dom.resizeObserverEntry",
  []
>;

export type namedNodeMap = rescript.opaque<
  "Dom.namedNodeMap",
  []
>;

export type nodeList = rescript.opaque<
  "Dom.nodeList",
  []
>;

export type radioNodeList = rescript.opaque<
  "Dom.radioNodeList",
  []
>;

export type processingInstruction = rescript.opaque<
  "Dom.processingInstruction",
  []
>;

export type _shadowRoot = rescript.opaque<
  "Dom._shadowRoot",
  []
>;

export type shadowRoot = node_like<_shadowRoot>;

export type _text = rescript.opaque<
  "Dom._text",
  []
>;

export type text = characterData_like<_text>;

export type domRect = rescript.opaque<
  "Dom.domRect",
  []
>;

export type dataTransfer = rescript.opaque<
  "Dom.dataTransfer",
  []
>;

export type domStringMap = rescript.opaque<
  "Dom.domStringMap",
  []
>;

export type history = rescript.opaque<
  "Dom.history",
  []
>;

export type _htmlDocument = rescript.opaque<
  "Dom._htmlDocument",
  []
>;

export type htmlDocument = document_like<_htmlDocument>;

export type _htmlElement<A> = rescript.opaque<
  "Dom._htmlElement",
  [A]
>;

export type htmlElement_like<A> = element_like<_htmlElement<A>>;

export type htmlElement = htmlElement_like<_baseClass>;

export type _htmlAnchorElement = rescript.opaque<
  "Dom._htmlAnchorElement",
  []
>;

export type htmlAnchorElement = htmlElement_like<_htmlAnchorElement>;

export type _htmlAreaElement = rescript.opaque<
  "Dom._htmlAreaElement",
  []
>;

export type htmlAreaElement = htmlElement_like<_htmlAreaElement>;

export type _htmlAudioElement = rescript.opaque<
  "Dom._htmlAudioElement",
  []
>;

export type htmlAudioElement = htmlElement_like<_htmlAudioElement>;

export type _htmlBaseElement = rescript.opaque<
  "Dom._htmlBaseElement",
  []
>;

export type htmlBaseElement = htmlElement_like<_htmlBaseElement>;

export type _htmlBodyElement = rescript.opaque<
  "Dom._htmlBodyElement",
  []
>;

export type htmlBodyElement = htmlElement_like<_htmlBodyElement>;

export type _htmlBrElement = rescript.opaque<
  "Dom._htmlBrElement",
  []
>;

export type htmlBrElement = htmlElement_like<_htmlBrElement>;

export type _htmlButtonElement = rescript.opaque<
  "Dom._htmlButtonElement",
  []
>;

export type htmlButtonElement = htmlElement_like<_htmlButtonElement>;

export type _htmlCanvasElement = rescript.opaque<
  "Dom._htmlCanvasElement",
  []
>;

export type htmlCanvasElement = htmlElement_like<_htmlCanvasElement>;

export type _htmlDataElement = rescript.opaque<
  "Dom._htmlDataElement",
  []
>;

export type htmlDataElement = htmlElement_like<_htmlDataElement>;

export type _htmlDataListElement = rescript.opaque<
  "Dom._htmlDataListElement",
  []
>;

export type htmlDataListElement = htmlElement_like<_htmlDataListElement>;

export type _htmlDialogElement = rescript.opaque<
  "Dom._htmlDialogElement",
  []
>;

export type htmlDialogElement = htmlElement_like<_htmlDialogElement>;

export type _htmlDivElement = rescript.opaque<
  "Dom._htmlDivElement",
  []
>;

export type htmlDivElement = htmlElement_like<_htmlDivElement>;

export type _htmlDlistElement = rescript.opaque<
  "Dom._htmlDlistElement",
  []
>;

export type htmlDlistElement = htmlElement_like<_htmlDlistElement>;

export type _htmlEmbedElement = rescript.opaque<
  "Dom._htmlEmbedElement",
  []
>;

export type htmlEmbedElement = htmlElement_like<_htmlEmbedElement>;

export type _htmlFieldSetElement = rescript.opaque<
  "Dom._htmlFieldSetElement",
  []
>;

export type htmlFieldSetElement = htmlElement_like<_htmlFieldSetElement>;

export type _htmlFormElement = rescript.opaque<
  "Dom._htmlFormElement",
  []
>;

export type htmlFormElement = htmlElement_like<_htmlFormElement>;

export type _htmlHeadElement = rescript.opaque<
  "Dom._htmlHeadElement",
  []
>;

export type htmlHeadElement = htmlElement_like<_htmlHeadElement>;

export type _htmlHeadingElement = rescript.opaque<
  "Dom._htmlHeadingElement",
  []
>;

export type htmlHeadingElement = htmlElement_like<_htmlHeadingElement>;

export type _htmlHrElement = rescript.opaque<
  "Dom._htmlHrElement",
  []
>;

export type htmlHrElement = htmlElement_like<_htmlHrElement>;

export type _htmlHtmlElement = rescript.opaque<
  "Dom._htmlHtmlElement",
  []
>;

export type htmlHtmlElement = htmlElement_like<_htmlHtmlElement>;

export type _htmlIframeElement = rescript.opaque<
  "Dom._htmlIframeElement",
  []
>;

export type htmlIframeElement = htmlElement_like<_htmlIframeElement>;

export type _htmlImageElement = rescript.opaque<
  "Dom._htmlImageElement",
  []
>;

export type htmlImageElement = htmlElement_like<_htmlImageElement>;

export type _htmlInputElement = rescript.opaque<
  "Dom._htmlInputElement",
  []
>;

export type htmlInputElement = htmlElement_like<_htmlInputElement>;

export type _htmlLabelElement = rescript.opaque<
  "Dom._htmlLabelElement",
  []
>;

export type htmlLabelElement = htmlElement_like<_htmlLabelElement>;

export type _htmlLegendElement = rescript.opaque<
  "Dom._htmlLegendElement",
  []
>;

export type htmlLegendElement = htmlElement_like<_htmlLegendElement>;

export type _htmlLiElement = rescript.opaque<
  "Dom._htmlLiElement",
  []
>;

export type htmlLiElement = htmlElement_like<_htmlLiElement>;

export type _htmlLinkElement = rescript.opaque<
  "Dom._htmlLinkElement",
  []
>;

export type htmlLinkElement = htmlElement_like<_htmlLinkElement>;

export type _htmlMapElement = rescript.opaque<
  "Dom._htmlMapElement",
  []
>;

export type htmlMapElement = htmlElement_like<_htmlMapElement>;

export type _htmlMediaElement = rescript.opaque<
  "Dom._htmlMediaElement",
  []
>;

export type htmlMediaElement = htmlElement_like<_htmlMediaElement>;

export type _htmlMenuElement = rescript.opaque<
  "Dom._htmlMenuElement",
  []
>;

export type htmlMenuElement = htmlElement_like<_htmlMenuElement>;

export type _htmlMetaElement = rescript.opaque<
  "Dom._htmlMetaElement",
  []
>;

export type htmlMetaElement = htmlElement_like<_htmlMetaElement>;

export type _htmlMeterElement = rescript.opaque<
  "Dom._htmlMeterElement",
  []
>;

export type htmlMeterElement = htmlElement_like<_htmlMeterElement>;

export type _htmlModElement = rescript.opaque<
  "Dom._htmlModElement",
  []
>;

export type htmlModElement = htmlElement_like<_htmlModElement>;

export type _htmlOListElement = rescript.opaque<
  "Dom._htmlOListElement",
  []
>;

export type htmlOListElement = htmlElement_like<_htmlOListElement>;

export type _htmlObjectElement = rescript.opaque<
  "Dom._htmlObjectElement",
  []
>;

export type htmlObjectElement = htmlElement_like<_htmlObjectElement>;

export type _htmlOptGroupElement = rescript.opaque<
  "Dom._htmlOptGroupElement",
  []
>;

export type htmlOptGroupElement = htmlElement_like<_htmlOptGroupElement>;

export type _htmlOptionElement = rescript.opaque<
  "Dom._htmlOptionElement",
  []
>;

export type htmlOptionElement = htmlElement_like<_htmlOptionElement>;

export type _htmlOutputElement = rescript.opaque<
  "Dom._htmlOutputElement",
  []
>;

export type htmlOutputElement = htmlElement_like<_htmlOutputElement>;

export type _htmlParagraphElement = rescript.opaque<
  "Dom._htmlParagraphElement",
  []
>;

export type htmlParagraphElement = htmlElement_like<_htmlParagraphElement>;

export type _htmlParamElement = rescript.opaque<
  "Dom._htmlParamElement",
  []
>;

export type htmlParamElement = htmlElement_like<_htmlParamElement>;

export type _htmlPreElement = rescript.opaque<
  "Dom._htmlPreElement",
  []
>;

export type htmlPreElement = htmlElement_like<_htmlPreElement>;

export type _htmlProgressElement = rescript.opaque<
  "Dom._htmlProgressElement",
  []
>;

export type htmlProgressElement = htmlElement_like<_htmlProgressElement>;

export type _htmlQuoteElement = rescript.opaque<
  "Dom._htmlQuoteElement",
  []
>;

export type htmlQuoteElement = htmlElement_like<_htmlQuoteElement>;

export type _htmlScriptElement = rescript.opaque<
  "Dom._htmlScriptElement",
  []
>;

export type htmlScriptElement = htmlElement_like<_htmlScriptElement>;

export type _htmlSelectElement = rescript.opaque<
  "Dom._htmlSelectElement",
  []
>;

export type htmlSelectElement = htmlElement_like<_htmlSelectElement>;

export type _htmlSlotElement = rescript.opaque<
  "Dom._htmlSlotElement",
  []
>;

export type htmlSlotElement = htmlElement_like<_htmlSlotElement>;

export type _htmlSourceElement = rescript.opaque<
  "Dom._htmlSourceElement",
  []
>;

export type htmlSourceElement = htmlElement_like<_htmlSourceElement>;

export type _htmlSpanElement = rescript.opaque<
  "Dom._htmlSpanElement",
  []
>;

export type htmlSpanElement = htmlElement_like<_htmlSpanElement>;

export type _htmlStyleElement = rescript.opaque<
  "Dom._htmlStyleElement",
  []
>;

export type htmlStyleElement = htmlElement_like<_htmlStyleElement>;

export type _htmlTableCaptionElement = rescript.opaque<
  "Dom._htmlTableCaptionElement",
  []
>;

export type htmlTableCaptionElement = htmlElement_like<_htmlTableCaptionElement>;

export type _htmlTableCellElement = rescript.opaque<
  "Dom._htmlTableCellElement",
  []
>;

export type htmlTableCellElement = htmlElement_like<_htmlTableCellElement>;

export type _htmlTableColElement = rescript.opaque<
  "Dom._htmlTableColElement",
  []
>;

export type htmlTableColElement = htmlElement_like<_htmlTableColElement>;

export type _htmlTableDataCellElement = rescript.opaque<
  "Dom._htmlTableDataCellElement",
  []
>;

export type htmlTableDataCellElement = htmlElement_like<_htmlTableDataCellElement>;

export type _htmlTableElement = rescript.opaque<
  "Dom._htmlTableElement",
  []
>;

export type htmlTableElement = htmlElement_like<_htmlTableElement>;

export type _htmlTableHeaderCellElement = rescript.opaque<
  "Dom._htmlTableHeaderCellElement",
  []
>;

export type htmlTableHeaderCellElement = htmlElement_like<_htmlTableHeaderCellElement>;

export type _htmlTableRowElement = rescript.opaque<
  "Dom._htmlTableRowElement",
  []
>;

export type htmlTableRowElement = htmlElement_like<_htmlTableRowElement>;

export type _htmlTableSectionElement = rescript.opaque<
  "Dom._htmlTableSectionElement",
  []
>;

export type htmlTableSectionElement = htmlElement_like<_htmlTableSectionElement>;

export type _htmlTextAreaElement = rescript.opaque<
  "Dom._htmlTextAreaElement",
  []
>;

export type htmlTextAreaElement = htmlElement_like<_htmlTextAreaElement>;

export type _htmlTimeElement = rescript.opaque<
  "Dom._htmlTimeElement",
  []
>;

export type htmlTimeElement = htmlElement_like<_htmlTimeElement>;

export type _htmlTitleElement = rescript.opaque<
  "Dom._htmlTitleElement",
  []
>;

export type htmlTitleElement = htmlElement_like<_htmlTitleElement>;

export type _htmlTrackElement = rescript.opaque<
  "Dom._htmlTrackElement",
  []
>;

export type htmlTrackElement = htmlElement_like<_htmlTrackElement>;

export type _htmlUlistElement = rescript.opaque<
  "Dom._htmlUlistElement",
  []
>;

export type htmlUlistElement = htmlElement_like<_htmlUlistElement>;

export type _htmlUnknownElement = rescript.opaque<
  "Dom._htmlUnknownElement",
  []
>;

export type htmlUnknownElement = htmlElement_like<_htmlUnknownElement>;

export type _htmlVideoElement = rescript.opaque<
  "Dom._htmlVideoElement",
  []
>;

export type htmlVideoElement = htmlElement_like<_htmlVideoElement>;

export type location = rescript.opaque<
  "Dom.location",
  []
>;

export type window = rescript.opaque<
  "Dom.window",
  []
>;

export type _xmlDocument = rescript.opaque<
  "Dom._xmlDocument",
  []
>;

export type xmlDocument = document_like<_xmlDocument>;

export type event_like<A> = rescript.opaque<
  "Dom.event_like",
  [A]
>;

export type event = event_like<_baseClass>;

export type _uiEvent<A> = rescript.opaque<
  "Dom._uiEvent",
  [A]
>;

export type uiEvent_like<A> = event_like<_uiEvent<A>>;

export type uiEvent = uiEvent_like<_baseClass>;

export type _animationEvent = rescript.opaque<
  "Dom._animationEvent",
  []
>;

export type animationEvent = event_like<_animationEvent>;

export type _beforeUnloadEvent = rescript.opaque<
  "Dom._beforeUnloadEvent",
  []
>;

export type beforeUnloadEvent = event_like<_beforeUnloadEvent>;

export type _clipboardEvent = rescript.opaque<
  "Dom._clipboardEvent",
  []
>;

export type clipboardEvent = event_like<_clipboardEvent>;

export type _closeEvent = rescript.opaque<
  "Dom._closeEvent",
  []
>;

export type closeEvent = event_like<_closeEvent>;

export type _compositionEvent = rescript.opaque<
  "Dom._compositionEvent",
  []
>;

export type compositionEvent = uiEvent_like<_compositionEvent>;

export type _customEvent = rescript.opaque<
  "Dom._customEvent",
  []
>;

export type customEvent = event_like<_customEvent>;

export type _dragEvent = rescript.opaque<
  "Dom._dragEvent",
  []
>;

export type dragEvent = event_like<_dragEvent>;

export type _errorEvent = rescript.opaque<
  "Dom._errorEvent",
  []
>;

export type errorEvent = event_like<_errorEvent>;

export type _focusEvent = rescript.opaque<
  "Dom._focusEvent",
  []
>;

export type focusEvent = uiEvent_like<_focusEvent>;

export type _idbVersionChangeEvent = rescript.opaque<
  "Dom._idbVersionChangeEvent",
  []
>;

export type idbVersionChangeEvent = event_like<_idbVersionChangeEvent>;

export type _inputEvent = rescript.opaque<
  "Dom._inputEvent",
  []
>;

export type inputEvent = uiEvent_like<_inputEvent>;

export type _keyboardEvent = rescript.opaque<
  "Dom._keyboardEvent",
  []
>;

export type keyboardEvent = uiEvent_like<_keyboardEvent>;

export type _mouseEvent<A> = rescript.opaque<
  "Dom._mouseEvent",
  [A]
>;

export type mouseEvent_like<A> = uiEvent_like<_mouseEvent<A>>;

export type mouseEvent = mouseEvent_like<_baseClass>;

export type _pageTransitionEvent = rescript.opaque<
  "Dom._pageTransitionEvent",
  []
>;

export type pageTransitionEvent = event_like<_pageTransitionEvent>;

export type _pointerEvent = rescript.opaque<
  "Dom._pointerEvent",
  []
>;

export type pointerEvent = mouseEvent_like<_pointerEvent>;

export type _popStateEvent = rescript.opaque<
  "Dom._popStateEvent",
  []
>;

export type popStateEvent = event_like<_popStateEvent>;

export type _progressEvent = rescript.opaque<
  "Dom._progressEvent",
  []
>;

export type progressEvent = event_like<_progressEvent>;

export type _relatedEvent = rescript.opaque<
  "Dom._relatedEvent",
  []
>;

export type relatedEvent = event_like<_relatedEvent>;

export type _storageEvent = rescript.opaque<
  "Dom._storageEvent",
  []
>;

export type storageEvent = event_like<_storageEvent>;

export type _svgZoomEvent = rescript.opaque<
  "Dom._svgZoomEvent",
  []
>;

export type svgZoomEvent = event_like<_svgZoomEvent>;

export type _timeEvent = rescript.opaque<
  "Dom._timeEvent",
  []
>;

export type timeEvent = event_like<_timeEvent>;

export type _touchEvent = rescript.opaque<
  "Dom._touchEvent",
  []
>;

export type touchEvent = uiEvent_like<_touchEvent>;

export type _trackEvent = rescript.opaque<
  "Dom._trackEvent",
  []
>;

export type trackEvent = event_like<_trackEvent>;

export type _transitionEvent = rescript.opaque<
  "Dom._transitionEvent",
  []
>;

export type transitionEvent = event_like<_transitionEvent>;

export type _webGlContextEvent = rescript.opaque<
  "Dom._webGlContextEvent",
  []
>;

export type webGlContextEvent = event_like<_webGlContextEvent>;

export type _wheelEvent = rescript.opaque<
  "Dom._wheelEvent",
  []
>;

export type wheelEvent = uiEvent_like<_wheelEvent>;

export type range = rescript.opaque<
  "Dom.range",
  []
>;

export type selection = rescript.opaque<
  "Dom.selection",
  []
>;

export type domTokenList = rescript.opaque<
  "Dom.domTokenList",
  []
>;

export type domSettableTokenList = rescript.opaque<
  "Dom.domSettableTokenList",
  []
>;

export interface nodeFilter {
  readonly acceptNode: (arg0: element) => number;
}

export type nodeIterator = rescript.opaque<
  "Dom.nodeIterator",
  []
>;

export type treeWalker = rescript.opaque<
  "Dom.treeWalker",
  []
>;

export type svgRect = rescript.opaque<
  "Dom.svgRect",
  []
>;

export type svgPoint = rescript.opaque<
  "Dom.svgPoint",
  []
>;

export type eventPointerId = rescript.opaque<
  "Dom.eventPointerId",
  []
>;
