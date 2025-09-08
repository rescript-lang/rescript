type t = Dom.mutationObserver

@new external make: ((array<Dom.mutationRecord>, t) => unit) => t = "MutationObserver"
