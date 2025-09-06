/* TypeScript file generated from MyInput.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {default as defaultNotChecked} from './MyInput';

type inputFocusEvent$ReScript = ReactEvent_Focus_t;

// In case of type error, check the type of 'default' in 'MyInput.res' and './MyInput'.
export const defaultTypeChecked: React.ComponentType<{ readonly onFocus?: (_1:inputFocusEvent) => void }> = defaultNotChecked as any;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as React.ComponentType<{ readonly onFocus?: (_1:inputFocusEvent) => void }> as any;

type $GenTypeImport<Expected, T extends Expected> = T;

import type {ReactEvent_Focus_t} from './shims/JsxEvent.shim';

import type {inputFocusEvent as inputFocusEvent$TypeScript} from './shims/JsxEvent.shim';

export type inputFocusEvent = $GenTypeImport<inputFocusEvent$ReScript,inputFocusEvent$TypeScript>;

export type props<onFocus> = { readonly onFocus?: onFocus };

export default $$default;
