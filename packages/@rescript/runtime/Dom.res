type _baseClass

type animation /* Web Animations API */

/* TODO: Should we bother with this indirection?
(* core *)
type domString = string
type domTimestamp = float
*/

/* css */
type cssStyleDeclaration
type cssStyleSheet

/* events (early) */
type eventTarget_like<'a>
type eventTarget = eventTarget_like<_baseClass>

/* nodes */
type _node<'a>
type node_like<'a> = eventTarget_like<_node<'a>>
type node = node_like<_baseClass>
type _attr
type attr = node_like<_attr>
type _characterData<'a>
type characterData_like<'a> = node_like<_characterData<'a>>
type characterData = characterData_like<_baseClass>
type _cdataSection
type cdataSection = characterData_like<_cdataSection>
type _comment
type comment = characterData_like<_comment>
type _document<'a>
type document_like<'a> = node_like<_document<'a>>
type document = document_like<_baseClass>
type _documentFragment
type documentFragment = node_like<_documentFragment>
type _documentType
type documentType = node_like<_documentType>
type domImplementation
type _element<'a>
type element_like<'a> = node_like<_element<'a>>
type element = element_like<_baseClass>
type htmlCollection
type htmlFormControlsCollection
type htmlOptionsCollection
type intersectionObserver
type intersectionObserverEntry
type mutationObserver
type mutationRecord
type performanceObserver
type performanceObserverEntryList
type reportingObserver
type reportingObserverOptions
type resizeObserver
type resizeObserverEntry
type namedNodeMap
type nodeList
type radioNodeList
type processingInstruction
type _shadowRoot
type shadowRoot = node_like<_shadowRoot>
type _text
type text = characterData_like<_text>

/* geometry */
type domRect

/* html */
type dataTransfer /* Drag and Drop API */
type domStringMap
type history
type _htmlDocument
type htmlDocument = document_like<_htmlDocument>
type _htmlElement<'a>
type htmlElement_like<'a> = element_like<_htmlElement<'a>>
type htmlElement = htmlElement_like<_baseClass>
type _htmlAnchorElement
type htmlAnchorElement = htmlElement_like<_htmlAnchorElement>
type _htmlAreaElement
type htmlAreaElement = htmlElement_like<_htmlAreaElement>
type _htmlAudioElement
type htmlAudioElement = htmlElement_like<_htmlAudioElement>
type _htmlBaseElement
type htmlBaseElement = htmlElement_like<_htmlBaseElement>
type _htmlBodyElement
type htmlBodyElement = htmlElement_like<_htmlBodyElement>
type _htmlBrElement
type htmlBrElement = htmlElement_like<_htmlBrElement>
type _htmlButtonElement
type htmlButtonElement = htmlElement_like<_htmlButtonElement>
type _htmlCanvasElement
type htmlCanvasElement = htmlElement_like<_htmlCanvasElement>
type _htmlDataElement
type htmlDataElement = htmlElement_like<_htmlDataElement>
type _htmlDataListElement
type htmlDataListElement = htmlElement_like<_htmlDataListElement>
type _htmlDialogElement
type htmlDialogElement = htmlElement_like<_htmlDialogElement>
type _htmlDivElement
type htmlDivElement = htmlElement_like<_htmlDivElement>
type _htmlDlistElement
type htmlDlistElement = htmlElement_like<_htmlDlistElement>
type _htmlEmbedElement
type htmlEmbedElement = htmlElement_like<_htmlEmbedElement>
type _htmlFieldSetElement
type htmlFieldSetElement = htmlElement_like<_htmlFieldSetElement>
type _htmlFormElement
type htmlFormElement = htmlElement_like<_htmlFormElement>
type _htmlHeadElement
type htmlHeadElement = htmlElement_like<_htmlHeadElement>
type _htmlHeadingElement
type htmlHeadingElement = htmlElement_like<_htmlHeadingElement>
type _htmlHrElement
type htmlHrElement = htmlElement_like<_htmlHrElement>
type _htmlHtmlElement
type htmlHtmlElement = htmlElement_like<_htmlHtmlElement>
type _htmlIframeElement
type htmlIframeElement = htmlElement_like<_htmlIframeElement>
type _htmlImageElement
type htmlImageElement = htmlElement_like<_htmlImageElement>
type _htmlInputElement
type htmlInputElement = htmlElement_like<_htmlInputElement>
type _htmlLabelElement
type htmlLabelElement = htmlElement_like<_htmlLabelElement>
type _htmlLegendElement
type htmlLegendElement = htmlElement_like<_htmlLegendElement>
type _htmlLiElement
type htmlLiElement = htmlElement_like<_htmlLiElement>
type _htmlLinkElement
type htmlLinkElement = htmlElement_like<_htmlLinkElement>
type _htmlMapElement
type htmlMapElement = htmlElement_like<_htmlMapElement>
type _htmlMediaElement
type htmlMediaElement = htmlElement_like<_htmlMediaElement>
type _htmlMenuElement
type htmlMenuElement = htmlElement_like<_htmlMenuElement>
type _htmlMetaElement
type htmlMetaElement = htmlElement_like<_htmlMetaElement>
type _htmlMeterElement
type htmlMeterElement = htmlElement_like<_htmlMeterElement>
type _htmlModElement
type htmlModElement = htmlElement_like<_htmlModElement>
type _htmlOListElement
type htmlOListElement = htmlElement_like<_htmlOListElement>
type _htmlObjectElement
type htmlObjectElement = htmlElement_like<_htmlObjectElement>
type _htmlOptGroupElement
type htmlOptGroupElement = htmlElement_like<_htmlOptGroupElement>
type _htmlOptionElement
type htmlOptionElement = htmlElement_like<_htmlOptionElement>
type _htmlOutputElement
type htmlOutputElement = htmlElement_like<_htmlOutputElement>
type _htmlParagraphElement
type htmlParagraphElement = htmlElement_like<_htmlParagraphElement>
type _htmlParamElement
type htmlParamElement = htmlElement_like<_htmlParamElement>
type _htmlPreElement
type htmlPreElement = htmlElement_like<_htmlPreElement>
type _htmlProgressElement
type htmlProgressElement = htmlElement_like<_htmlProgressElement>
type _htmlQuoteElement
type htmlQuoteElement = htmlElement_like<_htmlQuoteElement>
type _htmlScriptElement
type htmlScriptElement = htmlElement_like<_htmlScriptElement>
type _htmlSelectElement
type htmlSelectElement = htmlElement_like<_htmlSelectElement>
type _htmlSlotElement
type htmlSlotElement = htmlElement_like<_htmlSlotElement>
type _htmlSourceElement
type htmlSourceElement = htmlElement_like<_htmlSourceElement>
type _htmlSpanElement
type htmlSpanElement = htmlElement_like<_htmlSpanElement>
type _htmlStyleElement
type htmlStyleElement = htmlElement_like<_htmlStyleElement>
type _htmlTableCaptionElement
type htmlTableCaptionElement = htmlElement_like<_htmlTableCaptionElement>
type _htmlTableCellElement
type htmlTableCellElement = htmlElement_like<_htmlTableCellElement>
type _htmlTableColElement
type htmlTableColElement = htmlElement_like<_htmlTableColElement>
type _htmlTableDataCellElement
type htmlTableDataCellElement = htmlElement_like<_htmlTableDataCellElement>
type _htmlTableElement
type htmlTableElement = htmlElement_like<_htmlTableElement>
type _htmlTableHeaderCellElement
type htmlTableHeaderCellElement = htmlElement_like<_htmlTableHeaderCellElement>
type _htmlTableRowElement
type htmlTableRowElement = htmlElement_like<_htmlTableRowElement>
type _htmlTableSectionElement
type htmlTableSectionElement = htmlElement_like<_htmlTableSectionElement>
type _htmlTextAreaElement
type htmlTextAreaElement = htmlElement_like<_htmlTextAreaElement>
type _htmlTimeElement
type htmlTimeElement = htmlElement_like<_htmlTimeElement>
type _htmlTitleElement
type htmlTitleElement = htmlElement_like<_htmlTitleElement>
type _htmlTrackElement
type htmlTrackElement = htmlElement_like<_htmlTrackElement>
type _htmlUlistElement
type htmlUlistElement = htmlElement_like<_htmlUlistElement>
type _htmlUnknownElement
type htmlUnknownElement = htmlElement_like<_htmlUnknownElement>
type _htmlVideoElement
type htmlVideoElement = htmlElement_like<_htmlVideoElement>
type location
type window
type _xmlDocument
type xmlDocument = document_like<_xmlDocument>

/* events */
type event_like<'a>
type event = event_like<_baseClass>
type _uiEvent<'a>
type uiEvent_like<'a> = event_like<_uiEvent<'a>>
type uiEvent = uiEvent_like<_baseClass>
type _animationEvent
type animationEvent = event_like<_animationEvent>
type _beforeUnloadEvent
type beforeUnloadEvent = event_like<_beforeUnloadEvent>
type _clipboardEvent
type clipboardEvent = event_like<_clipboardEvent>
type _closeEvent
type closeEvent = event_like<_closeEvent>
type _compositionEvent
type compositionEvent = uiEvent_like<_compositionEvent>
type _customEvent
type customEvent = event_like<_customEvent>
type _dragEvent
type dragEvent = event_like<_dragEvent>
type _errorEvent
type errorEvent = event_like<_errorEvent>
type _focusEvent
type focusEvent = uiEvent_like<_focusEvent>
type _idbVersionChangeEvent
type idbVersionChangeEvent = event_like<_idbVersionChangeEvent>
type _inputEvent
type inputEvent = uiEvent_like<_inputEvent>
type _keyboardEvent
type keyboardEvent = uiEvent_like<_keyboardEvent>
type _mouseEvent<'a>
type mouseEvent_like<'a> = uiEvent_like<_mouseEvent<'a>>
type mouseEvent = mouseEvent_like<_baseClass>
type _pageTransitionEvent
type pageTransitionEvent = event_like<_pageTransitionEvent>
type _pointerEvent
type pointerEvent = mouseEvent_like<_pointerEvent>
type _popStateEvent
type popStateEvent = event_like<_popStateEvent>
type _progressEvent
type progressEvent = event_like<_progressEvent>
type _relatedEvent
type relatedEvent = event_like<_relatedEvent>
type _storageEvent
type storageEvent = event_like<_storageEvent>
type _svgZoomEvent
type svgZoomEvent = event_like<_svgZoomEvent>
type _timeEvent
type timeEvent = event_like<_timeEvent>
type _touchEvent
type touchEvent = uiEvent_like<_touchEvent>
type _trackEvent
type trackEvent = event_like<_trackEvent>
type _transitionEvent
type transitionEvent = event_like<_transitionEvent>
type _webGlContextEvent
type webGlContextEvent = event_like<_webGlContextEvent>
type _wheelEvent
type wheelEvent = uiEvent_like<_wheelEvent>

/* ranges */
type range

/* selection (TODO: move out of dom?) */
type selection

/* sets */
type domTokenList
type domSettableTokenList

/* traversal */
type nodeFilter = {
  acceptNode: element => int /* return type should be NodeFilter.action, but that would create a cycle */,
}
type nodeIterator
type treeWalker

/* SVG */
type svgRect
type svgPoint

/* special */
type eventPointerId

module Storage = Dom_storage
module Storage2 = Dom_storage2
