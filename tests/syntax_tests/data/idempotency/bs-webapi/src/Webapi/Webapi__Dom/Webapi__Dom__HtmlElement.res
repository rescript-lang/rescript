module Impl = (
  T: {
    type t
  },
) => {
  type t_htmlElement = T.t

  let ofElement: Dom.element => Js.null<t_htmlElement> = %raw(`
    function (element) {
      // BEWARE: Assumes "contentEditable" uniquely identifies an HTMLELement
      return element.contentEditable !== undefined ?  element : null;
    }
  `)
  @deprecated("Consider using Element.asHtmlElement or Element.unsafeAsHtmlElement instead")
  let ofElement: Dom.element => option<t_htmlElement> = self => Js.Null.toOption(ofElement(self))

  @get external accessKey: t_htmlElement => string = ""
  @set external setAccessKey: (t_htmlElement, string) => unit = "accessKey"
  @get external accessKeyLabel: t_htmlElement => string = ""
  @get external contentEditable: t_htmlElement => string /* enum */ = ""
  let contentEditable: t_htmlElement => Webapi__Dom__Types.contentEditable = self =>
    Webapi__Dom__Types.decodeContentEditable(contentEditable(self))
  @set external setContentEditable: (t_htmlElement, string /* enum */) => unit = "contentEditable"
  let setContentEditable: (t_htmlElement, Webapi__Dom__Types.contentEditable) => unit = (
    self,
    value,
  ) => setContentEditable(self, Webapi__Dom__Types.encodeContentEditable(value))
  @get external isContentEditable: t_htmlElement => bool = ""
  @get external contextMenu: t_htmlElement => Dom.htmlElement = "" /* returns HTMLMenuElement */
  @set
  external setContextMenu: (t_htmlElement, Dom.htmlElement) => unit =
    "contextMenu" /* accepts and returns HTMLMenuElement */
  @get external dataset: t_htmlElement => Dom.domStringMap = ""
  @get external dir: t_htmlElement => string /* enum */ = ""
  let dir: t_htmlElement => Webapi__Dom__Types.dir = self => Webapi__Dom__Types.decodeDir(dir(self))
  @set external setDir: (t_htmlElement, string /* enum */) => unit = "dir"
  let setDir: (t_htmlElement, Webapi__Dom__Types.dir) => unit = (self, value) =>
    setDir(self, Webapi__Dom__Types.encodeDir(value))
  @get external draggable: t_htmlElement => bool = ""
  @set external setDraggable: (t_htmlElement, bool) => unit = "draggable"
  /* let setDraggable : t_htmlElement => bool => unit = fun self value => setDraggable self (Js.Boolean.to_js_boolean value); */ /* temproarily removed to reduce codegen size */
  @get external dropzone: t_htmlElement => Dom.domSettableTokenList = ""
  @get external hidden: t_htmlElement => bool = ""
  @set external setHidden: (t_htmlElement, bool) => unit = "hidden"
  /* let setHidden : t_htmlElement => bool => unit = fun self value => setHidden self (Js.Boolean.to_js_boolean value); */ /* temproarily removed to reduce codegen size */
  @get external itemScope: t_htmlElement => bool = "" /* experimental */
  @set external setItemScope: (t_htmlElement, bool) => unit = "itemScope"
  /* let setItemScope : t_htmlElement => bool => unit = fun self value => setItemScope self (Js.Boolean.to_js_boolean value); */ /* experimental */ /* temproarily removed to reduce codegen size */
  @get external itemType: t_htmlElement => Dom.domSettableTokenList = "" /* experimental */
  @get external itemId: t_htmlElement => string = "" /* experimental */
  @set external setItemId: (t_htmlElement, string) => unit = "itemId" /* experimental */
  @get external itemRef: t_htmlElement => Dom.domSettableTokenList = "" /* experimental */
  @get external itemProp: t_htmlElement => Dom.domSettableTokenList = "" /* experimental */
  @get external itemValue: t_htmlElement => {..} = "" /* experimental */
  @set external setItemValue: (t_htmlElement, {..}) => unit = "itemValue" /* experimental */
  @get external lang: t_htmlElement => string = ""
  @set external setLang: (t_htmlElement, string) => unit = "lang"
  @get external offsetHeight: t_htmlElement => int = "" /* experimental */
  @get external offsetLeft: t_htmlElement => int = "" /* experimental */
  @get @return(nullable)
  external offsetParent: t_htmlElement => option<Dom.element> = "" /* experimental */
  @get external offsetTop: t_htmlElement => int = "" /* experimental, but widely supported */
  @get external offsetWidth: t_htmlElement => int = "" /* experimental */
  /* external properties : r => HTMLPropertiesCollection.t = "properties" [@@get]; /* experimental */ */
  @get external spellcheck: t_htmlElement => bool = ""
  @set external setSpellcheck: (t_htmlElement, bool) => unit = "spellcheck"
  /* let setSpellcheck : t_htmlElement => bool => unit = fun self value => setSpellcheck self (Js.Boolean.to_js_boolean value); */ /* temproarily removed to reduce codegen size */
  @get external style: t_htmlElement => Dom.cssStyleDeclaration = ""
  @set external setStyle: (t_htmlElement, Dom.cssStyleDeclaration) => unit = "style"
  @get external tabIndex: t_htmlElement => int = ""
  @set external setTabIndex: (t_htmlElement, int) => unit = "tabIndex"
  @get external title: t_htmlElement => string = ""
  @set external setTitle: (t_htmlElement, string) => unit = "title"
  @get external translate: t_htmlElement => bool = "" /* experimental */
  @set external setTranslate: (t_htmlElement, bool) => unit = "translate" /* experimental */
  /* let setTranslate : t_htmlElement => bool => unit = fun self value => setTranslate self (Js.Boolean.to_js_boolean value); */ /* temproarily removed to reduce codegen size */


  /* TODO: element-spcific, should be pulled out */
  @get external value: t_htmlElement => string = "" /* HTMLInputElement */
  @get external checked: t_htmlElement => bool = "" /* HTMLInputElement */
  @get external type_: t_htmlElement => string = "type" /* HTMLStyleElement */
  @set external setType: (t_htmlElement, string) => unit = "type" /* HTMLStyleElement */
  @get external rel: t_htmlElement => string = "" /* HTMLLinkElement */
  @set external setRel: (t_htmlElement, string) => unit = "rel" /* HTMLLinkElement */
  @get external href: t_htmlElement => string = "" /* HTMLLinkElement, HTMLAnchorElement */
  @set
  external setHref: (t_htmlElement, string) => unit =
    "href" /* HTMLLinkElement, HTMLAnchorElement */
}

/* TODO
module Tree (T: { type t; }) => {
  include ElementRe.Tree { type t = Type };
  include Impl { type t = Type };
};

include Tree { type t = Dom.htmlElement };
*/

type t = Dom.htmlElement

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__GlobalEventHandlers.Impl({
  type t = t
})
include Webapi__Dom__Element.Impl({
  type t = t
})
include Impl({
  type t = t
})
