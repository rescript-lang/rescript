let doc1: (/** ddd */ ~x: int) => int = (~x) => x + 1
let doc2: @res.doc(" ddd ") int => int = x => x + 1
let doc3: /** ddd */ int => int = x => x + 1
let doc4: (/** ddd */ ~x: int, /** eee */ n) => int = (~x) => x + 1
