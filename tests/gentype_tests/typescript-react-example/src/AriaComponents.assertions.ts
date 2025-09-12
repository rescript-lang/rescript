export {};

type $RescriptTypeSatisfiesTypeScriptType<RescriptType extends TypeScriptType, TypeScriptType> = RescriptType;

type groupRenderProps = $RescriptTypeSatisfiesTypeScriptType<{
  readonly isHovered: boolean; 
  readonly isFocusWithin: boolean; 
  readonly isFocusVisible: boolean; 
  readonly isDisabled: boolean; 
  readonly isInvalid: boolean
}, import("react-aria-components").GroupRenderProps>;
type selectionBehavior = $RescriptTypeSatisfiesTypeScriptType<"toggle" | "replace", import("react-stately").SelectionBehavior>;
type selectionMode = $RescriptTypeSatisfiesTypeScriptType<"none" | "single" | "multiple", import("react-stately").SelectionMode>;

const useTableOptions = undefined as unknown as typeof import("react-aria-components").useTableOptions satisfies () => tableOptionsContextValue;

type tableOptionsContextValue = {
  readonly selectionMode: selectionMode; 
  readonly selectionBehavior: (null | selectionBehavior); 
  readonly disallowEmptySelection: boolean; 
  readonly allowsDragging: boolean
};
