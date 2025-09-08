type f_obj = {"x": {"y": {"z": int}}}
let f: f_obj = {"x": {"y": {"z": 3}}}

type x<'a> = {x: 'a}
type y<'a> = {y: 'a}
type z<'a> = {z: 'a}
let f_record = {x: {y: {z: 3}}}

let f: f_obj = {"x": {"y": {"z": 3}}}

let f2: (list<{"x": {"y": {"z": int}}}>, array<{"x": {"y": {"z": int}}}>) = (
  list{{"x": {"y": {"z": 3}}}, {"x": {"y": {"z": 31}}}},
  [{"x": {"y": {"z": 3}}}, {"x": {"y": {"z": 31}}}],
)

let f3: {"x": {"y": {"z": int}}} = {"x": {"y": {"z": 3}}}

/* how about 
let f x = [%obj (x : < x : int > ) ] 
*/
/*
advantage of extension point
: robust, *control the entry point*, entry point can be more flexible easy to write , 
disdvantage
: more intrusive, does not work with ocamldep
*/
