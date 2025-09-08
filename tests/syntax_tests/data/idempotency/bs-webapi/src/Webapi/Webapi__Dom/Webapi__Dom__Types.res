/* ** enums, bitmaks and constants ** */
type compareHow =
  | StartToStart
  | StartToEnd
  | EndToEnd
  | EndToStart

let encodeCompareHow = x =>
  /* internal */
  switch x {
  | StartToStart => 0
  | StartToEnd => 1
  | EndToEnd => 2
  | EndToStart => 3
  }

type compareResult =
  | Before
  | Equal
  | After
  | Unknown

let decodeCompareResult = x =>
  /* internal */
  switch x {
  | -1 => Before
  | 0 => Equal
  | 1 => After
  | _ => Unknown
  }

type compatMode =
  | BackCompat
  | CSS1Compat
  | Unknown

let decodeCompatMode = x =>
  /* internal */
  switch x {
  | "BackCompat" => BackCompat
  | "CSS1Compat" => CSS1Compat
  | _ => Unknown
  }

type contentEditable =
  | True
  | False
  | Inherit
  | Unknown

let encodeContentEditable = x =>
  /* internal */
  switch x {
  | True => "true"
  | False => "false"
  | Inherit => "inherit"
  | Unknown => ""
  }

let decodeContentEditable = x =>
  /* internal */
  switch x {
  | "true" => True
  | "false" => False
  | "inherit" => Inherit
  | _ => Unknown
  }

type deltaMode =
  | Pixel
  | Line
  | Page

let decodeDeltaMode = x =>
  /* internal */
  switch x {
  | 0 => Pixel
  | 1 => Line
  | 2 => Page
  | _ => throw(Invalid_argument("invalid deltaMode"))
  }

type designMode =
  | On
  | Off
  | Unknown

let encodeDesignMode = x =>
  /* internal */
  switch x {
  | On => "on"
  | Off => "off"
  | Unknown => ""
  }

let decodeDesignMode = x =>
  /* internal */
  switch x {
  | "on" => On
  | "off" => Off
  | _ => Unknown
  }

type dir =
  | Ltr
  | Rtl
  | Unknown

let encodeDir = x =>
  /* internal */
  switch x {
  | Ltr => "ltr"
  | Rtl => "rtl"
  | Unknown => ""
  }

let decodeDir = x =>
  /* internal */
  switch x {
  | "ltr" => Ltr
  | "rtl" => Rtl
  | _ => Unknown
  }

type eventPhase =
  | None
  | CapturingPhase
  | AtTarget
  | BubblingPhase
  | Unknown

let decodeEventPhase = x =>
  /* internal */
  switch x {
  | 0 => None
  | 1 => CapturingPhase
  | 2 => AtTarget
  | 3 => BubblingPhase
  | _ => Unknown
  }

type filterAction =
  | Accept
  | Reject
  | Skip

let encodeFilterAction = x =>
  switch x {
  | Accept => 1
  | Reject => 2
  | Skip => 3
  }

type insertPosition =
  | BeforeBegin
  | AfterBegin
  | BeforeEnd
  | AfterEnd

let encodeInsertPosition = x =>
  /* internal */
  switch x {
  | BeforeBegin => "beforebegin"
  | AfterBegin => "afterbegin"
  | BeforeEnd => "beforeend"
  | AfterEnd => "afterend"
  }

type modifierKey =
  | Alt
  | AltGraph
  | CapsLock
  | Control
  | Fn
  | FnLock
  | Hyper
  | Meta
  | NumLock
  | ScrollLock
  | Shift
  | Super
  | Symbol
  | SymbolLock

let encodeModifierKey = x =>
  /* internal */
  switch x {
  | Alt => "Alt"
  | AltGraph => "AltGraph"
  | CapsLock => "CapsLock"
  | Control => "Control"
  | Fn => "Fn"
  | FnLock => "FnLock"
  | Hyper => "Hyper"
  | Meta => "Meta"
  | NumLock => "NumLock"
  | ScrollLock => "ScrollLock"
  | Shift => "Shift"
  | Super => "Super"
  | Symbol => "Symbol"
  | SymbolLock => "SymbolLock"
  }

type nodeType =
  | Element
  | Attribute /* deprecated */
  | Text
  | CDATASection /* deprecated */
  | EntityReference /* deprecated */
  | Entity /* deprecated */
  | ProcessingInstruction
  | Comment
  | Document
  | DocumentType
  | DocumentFragment
  | Notation /* deprecated */
  | Unknown

let decodeNodeType = x =>
  /* internal */
  switch x {
  | 1 => Element
  | 2 => Attribute
  | 3 => Text
  | 4 => CDATASection
  | 5 => EntityReference
  | 6 => Entity
  | 7 => ProcessingInstruction
  | 8 => Comment
  | 9 => Document
  | 10 => DocumentType
  | 11 => DocumentFragment
  | 12 => Notation
  | _ => Unknown
  }

type pointerType =
  | Mouse
  | Pen
  | Touch
  | Unknown

let decodePointerType = x =>
  /* itnernal */
  switch x {
  | "mouse" => Mouse
  | "pen" => Pen
  | "touch|" => Touch
  | _ => Unknown
  }

type readyState =
  | Loading
  | Interactive
  | Complete
  | Unknown

let decodeReadyState = x =>
  /* internal */
  switch x {
  | "loading" => Loading
  | "interactive" => Interactive
  | "complete" => Complete
  | _ => Unknown
  }

type shadowRootMode =
  | Open
  | Closed

let decodeShadowRootMode = x =>
  /* internal */
  switch x {
  | "open" => Open
  | "closed" => Closed
  | _ => throw(Invalid_argument("Unknown shadowRootMode"))
  }

type visibilityState =
  | Visible
  | Hidden
  | Prerender
  | Unloaded
  | Unknown

let decodeVisibilityState = x =>
  /* internal */
  switch x {
  | "visible" => Visible
  | "hidden" => Hidden
  | "prerender" => Prerender
  | "unloaded" => Unloaded
  | _ => Unknown
  }

type image

module type WhatToShowT = {
  type t

  let _All: t
  let _Element: t
  let _Attribute: t
  let _Text: t
  let _CDATASection: t
  let _EntityReference: t
  let _Entity: t
  let _ProcessingInstruction: t
  let _Comment: t
  let _Document: t
  let _DocumentType: t
  let _DocumentFragment: t
  let _Notation: t

  let many: list<t> => t
}

module WhatToShow: WhatToShowT = {
  type t = int

  let _All = -1
  let _Element = 1
  let _Attribute = 2
  let _Text = 4
  let _CDATASection = 8
  let _EntityReference = 16
  let _Entity = 32
  let _ProcessingInstruction = 64
  let _Comment = 128
  let _Document = 256
  let _DocumentType = 512
  let _DocumentFragment = 1024
  let _Notation = 2048

  let rec many = x =>
    switch x {
    | list{} => 0
    | list{hd, ...rest} => lor(hd, many(rest))
    }
}
