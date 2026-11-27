/* A closed immutable field cannot be promoted. Assignment may promote an
   immutable field only while its object row remains open; a closed row cannot
   acquire write capability. The compiling open-row case is covered in the
   end-to-end object mutability tests, where assignment strengthens the
   function parameter instead. */
let g = (o: {"x": int}) => o["x"] = 1
