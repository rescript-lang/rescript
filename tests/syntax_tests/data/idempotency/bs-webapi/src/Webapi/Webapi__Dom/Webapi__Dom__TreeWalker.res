type t = Dom.treeWalker

@get external root: t => Dom.node = ""
@get external whatToShow: t => Webapi__Dom__Types.WhatToShow.t = ""
@get @return(nullable) external filter: t => option<Dom.nodeFilter> = ""
@get external currentNode: t => Dom.node = ""
@set external setCurrentNode: (t, Dom.node) => unit = ""
