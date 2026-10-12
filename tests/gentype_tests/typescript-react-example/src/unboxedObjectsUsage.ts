import type {domains, inline, mixed, named, precedence} from './UnboxedObjects.gen';

export const readNamed = (value: named): number =>
  value.kind === 'created' ? value.id : value.reason.length;

export const readInline = (value: inline): number =>
  value.kind === 'created' ? value.id : value.reason.length;

export const readMixed = (value: mixed): number =>
  value.kind === 'created' ? value.id : value.reason.length;

export const readDomains = (value: domains): number => {
  if ('value' in value) return value.value;
  if ('amount' in value) return value.amount;
  return value.enabled ? 1 : 0;
};

export const readPrecedence = (value: precedence): number => value.value;
