let structural_obj = {"x": {"y": {"z": 3}}}
/* compiler inferred type :
 val structural_obj : < x : < y : < z : int >  >  > [@obj] */

type x<'a> = {x: 'a}
type y<'a> = {y: 'a}
type z<'a> = {z: 'a}
let f_record = {x: {y: {z: 3}}}
/* compiler inferred type :
 val f_record : int z y x */
