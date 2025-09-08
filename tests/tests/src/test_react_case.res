/**
var CommentBox = React.createClass({
  render: function() {
    return (
      <div className=\"commentBox\">
        Hello, world! I am a CommentBox.
      </div>
    );
    }
});
*/
type obj_spec
type react_dom_element
@val
external react_create_class: obj_spec => react_dom_element /* "createClass" "React" */ =
  "react_create_class"
type react_dom_component
@obj
external mk_obj_spec: (
  ~display_name: string=?,
  ~render: unit => react_dom_component,
  unit,
) => obj_spec = ""

@obj external empty_obj: unit => _ = ""

let v = empty_obj
