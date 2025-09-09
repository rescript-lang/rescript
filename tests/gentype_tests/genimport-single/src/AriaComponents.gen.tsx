/* TypeScript file generated from AriaComponents.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {useTableOptions as useTableOptionsNotChecked} from 'react-aria-components';

export type $RescriptTypeSatisfiesTypeScriptType<RescriptType, TypeScriptType extends RescriptType> = TypeScriptType;

// Check imported TypeScript value conforms to ReScript type
const _useTableOptionsTypeChecked = useTableOptionsNotChecked satisfies () => tableOptionsContextValue;

export type groupRenderProps = $RescriptTypeSatisfiesTypeScriptType<
  {
    readonly isHovered: boolean; 
    readonly isFocusWithin: boolean; 
    readonly isFocusVisible: boolean; 
    readonly isDisabled: boolean; 
    readonly isInvalid: boolean
  },
  import("react-aria-components").GroupRenderProps
>;

export type selectionBehavior = $RescriptTypeSatisfiesTypeScriptType<
  
    "toggle"
  | "replace",
  import("react-stately").SelectionBehavior
>;

export type selectionMode = $RescriptTypeSatisfiesTypeScriptType<
  
    "none"
  | "single"
  | "multiple",
  import("react-stately").SelectionMode
>;

export type tableOptionsContextValue = {
  readonly selectionMode: selectionMode; 
  readonly selectionBehavior: (null | selectionBehavior); 
  readonly disallowEmptySelection: boolean; 
  readonly allowsDragging: boolean
};
