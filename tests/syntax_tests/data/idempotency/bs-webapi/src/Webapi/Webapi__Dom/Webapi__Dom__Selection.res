type t = Dom.selection

@get @return(nullable) external anchorNode: t => option<Dom.node> = ""
@get external anchorOffset: t => int = ""
@get @return(nullable) external focusNode: t => option<Dom.node> = ""
@get external focusOffset: t => int = ""
@get external isCollapsed: t => bool = ""
@get external rangeCount: t => int = ""
