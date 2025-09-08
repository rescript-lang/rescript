type t = Dom.mutationRecord

@get external type_: t => string = "type"
@get external target: t => Dom.node = ""
@get external addedNodes: t => Dom.nodeList = ""
@get external removedNodes: t => Dom.nodeList = ""
@get @return(nullable) external previousSibling: t => option<Dom.node> = ""
@get @return(nullable) external pnextSibling: t => option<Dom.node> = ""
@get external attributeName: t => string = ""
@get external attributeNamespace: t => string = ""
@get external oldValue: t => string = ""
