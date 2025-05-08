/* TypeScript file generated from ImportHookDefault.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {default as makeNotChecked} from './hookExample';

import {default as defaultNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHookDefault.res' and './hookExample'.
export const makeTypeChecked: React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}> = makeNotChecked as any;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}> as any;

// In case of type error, check the type of 'default' in 'ImportHookDefault.res' and './hookExample'.
export const defaultTypeChecked: React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}> = defaultNotChecked as any;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}> as any;

import type {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

export type person = { readonly name: string; readonly age: number };

export type props<person,children,renderMe> = {
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

export type MM_props<person,children,renderMe> = {
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

export default $$default;
