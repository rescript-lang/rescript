/* TypeScript file generated from ImportHooks.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {makeRenamed as makeRenamedNotChecked} from './hookExample';

import {foo as fooNotChecked} from './hookExample';

// In case of type error, check the type of 'makeRenamed' in 'ImportHooks.res' and './hookExample'.
export const makeRenamedTypeChecked: React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}> = makeRenamedNotChecked as any;

// Export 'makeRenamed' early to allow circular import from the '.bs.js' file.
export const makeRenamed: unknown = makeRenamedTypeChecked as React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}> as any;

// In case of type error, check the type of 'foo' in 'ImportHooks.res' and './hookExample'.
export const fooTypeChecked: (person:person) => string = fooNotChecked as any;

// Export 'foo' early to allow circular import from the '.bs.js' file.
export const foo: unknown = fooTypeChecked as (person:person) => string as any;

export type person = { readonly name: string; readonly age: number };

export type renderMe<a> = React.ComponentType<{ readonly randomString: string; readonly poly: a }>;

export type props<actions,person,children,renderMe> = {
  readonly actions?: actions; 
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};
