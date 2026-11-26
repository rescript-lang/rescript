declare const reactElementMarker: unique symbol;
export type ElementHandle = { readonly $$typeof: typeof reactElementMarker };
declare const mouseEventMarker: unique symbol;
export type MouseEventHandle = { readonly marker: typeof mouseEventMarker };
type Node = ElementHandle | string | readonly ElementHandle[];
export function jsx<P>(tag: (props: P) => ElementHandle, props: P, key?: string): ElementHandle;
export function jsx<P>(tag: "span" | "h1", props: { readonly children: string }): ElementHandle;
type ButtonProps = { readonly onClick: (event: MouseEventHandle) => void; readonly children: string };
export function jsx<P>(tag: "button", props: ButtonProps): ElementHandle;
export function jsx<P>(tag: "button", props: { readonly onClick: (event: MouseEventHandle) => void; readonly children: string; readonly "aria-pressed": "true" | "false" | "mixed" }): ElementHandle;
export function jsx<P>(tag: "ul", props: { readonly children: readonly (ElementHandle | undefined)[] }): ElementHandle;
export function jsxs(tag: "li" | "div", props: { readonly className: string; readonly children: readonly Node[] }): ElementHandle;
export function jsxs(tag: "main" | "label", props: { readonly children: readonly Node[] }): ElementHandle;

declare global {
  namespace React {
    namespace JSX {
      type Element = ElementHandle;
    }
    type MouseEvent = MouseEventHandle;
  }
}
