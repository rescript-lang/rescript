/* TypeScript file generated from MyInput.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {default as defaultNotChecked} from './MyInput';

// In case of type error, check the type of 'default' in 'MyInput.res' and './MyInput'.
export const defaultTypeChecked: React.ComponentType<{ readonly onFocus?: (_1:inputFocusEvent) => void }> = defaultNotChecked as any;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as React.ComponentType<{ readonly onFocus?: (_1:inputFocusEvent) => void }> as any;

import type {inputFocusEvent as $$inputFocusEvent} from './shims/JsxEvent.shim';

export type inputFocusEvent = $$inputFocusEvent;

export type props<onFocus> = { readonly onFocus?: onFocus };

export default $$default;
