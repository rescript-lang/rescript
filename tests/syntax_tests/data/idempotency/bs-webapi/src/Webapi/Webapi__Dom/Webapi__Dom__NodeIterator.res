type t = Dom.nodeIterator

@get external root: t => Dom.node = ""
@get external referenceNode: t => Dom.node = ""
@get external pointerBeforeReferenceNode: t => bool = ""
@get external whatToShow: t => Webapi__Dom__Types.WhatToShow.t = ""
@get @return(nullable) external filter: t => option<Dom.nodeFilter> = ""

