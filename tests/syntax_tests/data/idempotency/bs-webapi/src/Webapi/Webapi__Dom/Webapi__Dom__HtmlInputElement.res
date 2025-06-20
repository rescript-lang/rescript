/*
 * Spec: https://html.spec.whatwg.org/multipage/input.html#the-input-element
 * MDN: https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement
 */

module Impl = (
  T: {
    type t
  },
) => {
  type t_htmlInputElement = T.t

  @get @return(nullable) external form: t_htmlInputElement => option<Dom.htmlFormElement> = ""
  @get external formAction: t_htmlInputElement => string = ""
  @set external setFormAction: (t_htmlInputElement, string) => unit = "formAction"
  @get external formEncType: t_htmlInputElement => string = ""
  @set external setFormEncType: (t_htmlInputElement, string) => unit = "formEncType"
  @get external formMethod: t_htmlInputElement => string = ""
  @set external setFormMethod: (t_htmlInputElement, string) => unit = "formMethod"
  @get external formNoValidate: t_htmlInputElement => bool = ""
  @set external setFormNoValidate: (t_htmlInputElement, bool) => unit = "formNoValidate"
  @get external formTarget: t_htmlInputElement => string = ""
  @set external setFormTarget: (t_htmlInputElement, string) => unit = "formTarget"

  /* Properties that apply to any type of input element that is not hidden */
  @get external name: t_htmlInputElement => string = ""
  @set external setName: (t_htmlInputElement, string) => unit = "name"
  @get external type_: t_htmlInputElement => string = "type"
  @set external setType: (t_htmlInputElement, string) => unit = "type"
  @get external disabled: t_htmlInputElement => bool = ""
  @set external setDisabled: (t_htmlInputElement, bool) => unit = "disabled"
  @get external autofocus: t_htmlInputElement => bool = ""
  @set external setAutofocus: (t_htmlInputElement, bool) => unit = "autofocus"
  @get external required: t_htmlInputElement => bool = ""
  @set external setRequired: (t_htmlInputElement, bool) => unit = "required"
  @get external value: t_htmlInputElement => string = ""
  @set external setValue: (t_htmlInputElement, string) => unit = "value"
  @get external validity: t_htmlInputElement => Webapi__Dom__ValidityState.t = ""
  @get external validationMessage: t_htmlInputElement => string = ""
  @get external willValidate: t_htmlInputElement => bool = ""

  /* Properties that apply only to elements of type "checkbox" or "radio" */
  @get external checked: t_htmlInputElement => bool = ""
  @set external setChecked: (t_htmlInputElement, bool) => unit = "checked"
  @get external defaultChecked: t_htmlInputElement => bool = ""
  @set external setDefaultChecked: (t_htmlInputElement, bool) => unit = "defaultChecked"
  @get external indeterminate: t_htmlInputElement => bool = ""
  @set external setIndeterminate: (t_htmlInputElement, bool) => unit = "indeterminate"

  /* Properties that apply only to elements of type "image" */
  @get external alt: t_htmlInputElement => string = ""
  @set external setAlt: (t_htmlInputElement, string) => unit = "alt"
  @get external height: t_htmlInputElement => string = ""
  @set external setHeight: (t_htmlInputElement, string) => unit = "height"
  @get external src: t_htmlInputElement => string = ""
  @set external setSrc: (t_htmlInputElement, string) => unit = "src"
  @get external width: t_htmlInputElement => string = ""
  @set external setWidth: (t_htmlInputElement, string) => unit = "width"

  /* Properties that apply only to elements of type "file" */
  @get external accept: t_htmlInputElement => string = ""
  @set external setAccept: (t_htmlInputElement, string) => unit = "accept"
  /* TODO: files: Returns/accepts a FileList object. */

  /* Properties that apply only to text/number-containing or elements */
  @get external autocomplete: t_htmlInputElement => string = ""
  @set external setAutocomplete: (t_htmlInputElement, string) => unit = "autocomplete"
  @get external maxLength: t_htmlInputElement => int = ""
  @set external setMaxLength: (t_htmlInputElement, int) => unit = "maxLength"
  @get external minLength: t_htmlInputElement => int = ""
  @set external setMinLength: (t_htmlInputElement, int) => unit = "minLength"
  @get external size: t_htmlInputElement => int = ""
  @set external setSize: (t_htmlInputElement, int) => unit = "size"
  @get external pattern: t_htmlInputElement => string = ""
  @set external setPattern: (t_htmlInputElement, string) => unit = "pattern"
  @get external placeholder: t_htmlInputElement => string = ""
  @set external setPlaceholder: (t_htmlInputElement, string) => unit = "placeholder"
  @get external readOnly: t_htmlInputElement => bool = ""
  @set external setReadOnly: (t_htmlInputElement, bool) => unit = "readOnly"
  @get external min: t_htmlInputElement => string = ""
  @set external setMin: (t_htmlInputElement, string) => unit = "min"
  @get external max: t_htmlInputElement => string = ""
  @set external setMax: (t_htmlInputElement, string) => unit = "max"
  @get external selectionStart: t_htmlInputElement => int = ""
  @set external setSelectionStart: (t_htmlInputElement, int) => unit = "selectionStart"
  @get external selectionEnd: t_htmlInputElement => int = ""
  @set external setSelectionEnd: (t_htmlInputElement, int) => unit = "selectionEnd"
  @get external selectionDirection: t_htmlInputElement => string = ""
  @set external setSelectionDirection: (t_htmlInputElement, string) => unit = "selectionDirection"

  /* Properties not yet categorized */
  @get external defaultValue: t_htmlInputElement => string = ""
  @set external setDefaultValue: (t_htmlInputElement, string) => unit = "defaultValue"
  @get external dirName: t_htmlInputElement => string = ""
  @set external setDirName: (t_htmlInputElement, string) => unit = "dirName"
  @get external accessKey: t_htmlInputElement => string = ""
  @set external setAccessKey: (t_htmlInputElement, string) => unit = "accessKey"
  @get @return(nullable) external list: t_htmlInputElement => option<Dom.htmlElement> = ""
  @get external multiple: t_htmlInputElement => bool = ""
  @set external setMultiple: (t_htmlInputElement, bool) => unit = "multiple"
  /* TODO: files: FileList array. Returns the list of selected files. */
  @get external labels: t_htmlInputElement => array<Dom.nodeList> = ""
  @get external step: t_htmlInputElement => string = ""
  @set external setStep: (t_htmlInputElement, string) => unit = "step"
  @get @return(nullable) external valueAsDate: t_htmlInputElement => option<Js.Date.t> = ""
  @set external setValueAsDate: (t_htmlInputElement, Js.Date.t) => unit = "valueAsDate"
  @get external valueAsNumber: t_htmlInputElement => float = ""

 
  module SelectionDirection = {
    type t =
      | Forward
      | Backward
      | None

    let toString = x =>
      switch x {
      | Forward => "forward"
      | Backward => "backward"
      | None => "none"
      }
  }

  let setSelectionRangeWithDirection = (
    selectionStart,
    selectionEnd,
    selectionDirection,
    element,
  ) =>
    element->setSelectionRangeWithDirection_(
      selectionStart,
      selectionEnd,
      selectionDirection->SelectionDirection.toString,
    )

  module SelectionMode = {
    type t =
      | Select
      | Start
      | End
      | Preserve

    let toString = x =>
      switch x {
      | Select => "select"
      | Start => "start"
      | End => "end"
      | Preserve => "preserve"
      }
  }

  let setRangeTextWithinIntervalWithSelectionMode = (
    text,
    selectionStart,
    selectionEnd,
    selectionMode,
    element,
  ) =>
    element->setRangeTextWithinIntervalWithSelectionMode_(
      text,
      selectionStart,
      selectionEnd,
      selectionMode->SelectionMode.toString,
    )

}

type t = Dom.htmlInputElement

include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__Element.Impl({
  type t = t
})
include Webapi__Dom__HtmlElement.Impl({
  type t = t
})
include Impl({
  type t = t
})
