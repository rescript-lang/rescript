/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

@notUndefined
type element

@val external null: element = "null"

external float: float => element = "%identity"
external int: int => element = "%identity"
external string: string => element = "%identity"
external array: array<element> => element = "%identity"
external promise: promise<element> => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return

/* Components consume props. If one component can accept broader props, it can
   safely stand in for a component that only needs narrower props, just like a
   function argument type. That makes the props parameter contravariant. */
type component<-'props>

/* this function exists to prepare for making `component` abstract */
external component: componentLike<'props, element> => component<'props> = "%component_identity"
