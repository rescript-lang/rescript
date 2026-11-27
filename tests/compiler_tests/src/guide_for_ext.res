/*
[%js{ text : 32 ; label : \"hel\" }]
Attention: also ok for nested case
*/

let mk = () => {
  module N = {
    @obj external mk: (~text: 'b, ~label: 'a) => {"text": 'b, "label": 'a} = ""
  }

  N.mk(~text=32, ~label="hel")
}
