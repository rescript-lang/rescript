/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/***
Functions for interacting with DataView.

See [`DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) on MDN.
*/

/**
Type representing a DataView.
*/
@notUndefined
type t

/**
`fromBuffer(buffer)` creates a DataView for the entire ArrayBuffer.

See [`DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) on MDN.

## Examples

```rescript
let dv = DataView.fromBuffer(buffer)
```
*/
@new external fromBuffer: Stdlib_ArrayBuffer.t => t = "DataView"

/**
`fromBufferToEnd(buffer, ~byteOffset)` creates a DataView starting at a given byte offset.

See [`DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) on MDN.

## Examples

```rescript
let dv = DataView.fromBufferToEnd(buffer, ~byteOffset=4)
```
*/
@new external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "DataView"

/**
`fromBufferWithRange(buffer, ~byteOffset, ~length)` creates a DataView for a specific byte range.

See [`DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) on MDN.

## Examples

```rescript
let dv = DataView.fromBufferWithRange(buffer, ~byteOffset=2, ~length=8)
```
*/
@new external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t = "DataView"

/**
`buffer(dataView)` returns the underlying ArrayBuffer referenced by this DataView.

See [`DataView.buffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/buffer) on MDN.

## Examples

```rescript
let arrBuf = DataView.buffer(dv)
```
*/
@get external buffer: t => Stdlib_ArrayBuffer.t = "buffer"

/**
`byteLength(dataView)` returns the length (in bytes) of this DataView.

See [`DataView.byteLength`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/byteLength) on MDN.

## Examples

```rescript
let len = DataView.byteLength(dv)
```
*/
@get external byteLength: t => int = "byteLength"

/**
`byteOffset(dataView)` returns the byte offset of this DataView from the start of its ArrayBuffer.

See [`DataView.byteOffset`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/byteOffset) on MDN.

## Examples

```rescript
let offset = DataView.byteOffset(dv)
```
*/
@get external byteOffset: t => int = "byteOffset"

/**
`getInt8(dataView, offset)` reads an 8-bit signed integer at the specified byte offset.

See [`DataView.getInt8`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getInt8) on MDN.

## Examples

```rescript
let v = DataView.getInt8(dv, 0)
```
*/
@send external getInt8: (t, int, ~littleEndian: bool=?) => int = "getInt8"

/**
`getUint8(dataView, offset)` reads an 8-bit unsigned integer at the specified byte offset.

See [`DataView.getUint8`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getUint8) on MDN.

## Examples

```rescript
let v = DataView.getUint8(dv, 1)
```
*/
@send external getUint8: (t, int, ~littleEndian: bool=?) => int = "getUint8"

/**
`getInt16(dataView, offset, ~littleEndian=?)` reads a 16-bit signed integer at the specified byte offset, with optional endianness.

See [`DataView.getInt16`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getInt16) on MDN.

## Examples

```rescript
let v1 = DataView.getInt16(dv, 2)
let v2 = DataView.getInt16(dv, 2, ~littleEndian=true)
```
*/
@send external getInt16: (t, int, ~littleEndian: bool=?) => int = "getInt16"

/**
`getUint16(dataView, offset, ~littleEndian=?)` reads a 16-bit unsigned integer at the specified byte offset, with optional endianness.

See [`DataView.getUint16`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getUint16) on MDN.

## Examples

```rescript
let v1 = DataView.getUint16(dv, 2)
let v2 = DataView.getUint16(dv, 2, ~littleEndian=true)
```
*/
@send external getUint16: (t, int, ~littleEndian: bool=?) => int = "getUint16"

/**
`getInt32(dataView, offset, ~littleEndian=?)` reads a 32-bit signed integer at the specified byte offset, with optional endianness.

See [`DataView.getInt32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getInt32) on MDN.

## Examples

```rescript
let v1 = DataView.getInt32(dv, 4)
let v2 = DataView.getInt32(dv, 4, ~littleEndian=true)
```
*/
@send external getInt32: (t, int, ~littleEndian: bool=?) => int = "getInt32"

/**
`getUint32(dataView, offset, ~littleEndian=?)` reads a 32-bit unsigned integer at the specified byte offset, with optional endianness.

See [`DataView.getUint32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getUint32) on MDN.

## Examples

```rescript
let v1 = DataView.getUint32(dv, 4)
let v2 = DataView.getUint32(dv, 4, ~littleEndian=true)
```
*/
@send external getUint32: (t, int, ~littleEndian: bool=?) => int = "getUint32"

/**
`getFloat16(dataView, offset, ~littleEndian=?)` reads a 16-bit floating point number (if supported) at the specified byte offset, with optional endianness.

See [`DataView.getFloat16`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getFloat16) on MDN.

## Examples
```rescript
let v1 = DataView.getFloat16(dv, 6)
let v2 = DataView.getFloat16(dv, 6, ~littleEndian=true)
```
*/
@send external getFloat16: (t, int, ~littleEndian: bool=?) => float = "getFloat16"

/**
`getFloat32(dataView, offset, ~littleEndian=?)` reads a 32-bit floating point number at the specified byte offset, with optional endianness.

See [`DataView.getFloat32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getFloat32) on MDN.

## Examples

```rescript
let v1 = DataView.getFloat32(dv, 8)
let v2 = DataView.getFloat32(dv, 8, ~littleEndian=true)
```
*/
@send external getFloat32: (t, int, ~littleEndian: bool=?) => float = "getFloat32"

/**
`getFloat64(dataView, offset, ~littleEndian=?)` reads a 64-bit floating point number at the specified byte offset, with optional endianness.

See [`DataView.getFloat64`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getFloat64) on MDN.

## Examples

```rescript
let v1 = DataView.getFloat64(dv, 10)
let v2 = DataView.getFloat64(dv, 10, ~littleEndian=true)
```
*/
@send external getFloat64: (t, int, ~littleEndian: bool=?) => float = "getFloat64"

/**
`getBigInt64(dataView, offset, ~littleEndian=?)` reads a 64-bit signed integer (BigInt) at the specified byte offset, with optional endianness.

See [`DataView.getBigInt64`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getBigInt64) on MDN.

## Examples
```rescript
let v1 = DataView.getBigInt64(dv, 12)
let v2 = DataView.getBigInt64(dv, 12, ~littleEndian=true)
```
*/
@send external getBigInt64: (t, int, ~littleEndian: bool=?) => bigint = "getBigInt64"

/**
`getBigUint64(dataView, offset, ~littleEndian=?)` reads a 64-bit unsigned integer (BigInt) at the specified byte offset, with optional endianness.

See [`DataView.getBigUint64`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/getBigUint64) on MDN.

## Examples

```rescript
let v1 = DataView.getBigUint64(dv, 14)
let v2 = DataView.getBigUint64(dv, 14, ~littleEndian=true)
```
*/
@send external getBigUint64: (t, int, ~littleEndian: bool=?) => bigint = "getBigUint64"

/**
`setInt8(dataView, offset, value)` writes an 8-bit signed integer at the specified byte offset.

See [`DataView.setInt8`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setInt8) on MDN.

## Examples

```rescript
DataView.setInt8(dv, 0, -12)
```
*/
@send external setInt8: (t, int, int) => unit = "setInt8"

/**
`setUint8(dataView, offset, value)` writes an 8-bit unsigned integer at the specified byte offset.

See [`DataView.setUint8`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setUint8) on MDN.

## Examples

```rescript
DataView.setUint8(dv, 1, 255)
```
*/
@send external setUint8: (t, int, int) => unit = "setUint8"

/**
`setInt16(dataView, offset, value, ~littleEndian=?)` writes a 16-bit signed integer at the specified byte offset, with optional endianness.

See [`DataView.setInt16`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setInt16) on MDN.

## Examples

```rescript
DataView.setInt16(dv, 2, -1234)
DataView.setInt16(dv, 2, -1234, ~littleEndian=true)
```
*/
@send external setInt16: (t, int, int, ~littleEndian: bool=?) => unit = "setInt16"

/**
`setUint16(dataView, offset, value, ~littleEndian=?)` writes a 16-bit unsigned integer at the specified byte offset, with optional endianness.

See [`DataView.setUint16`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setUint16) on MDN.

## Examples

```rescript
DataView.setUint16(dv, 2, 1234)
DataView.setUint16(dv, 2, 1234, ~littleEndian=true)
```
*/
@send external setUint16: (t, int, int, ~littleEndian: bool=?) => unit = "setUint16"

/**
`setInt32(dataView, offset, value, ~littleEndian=?)` writes a 32-bit signed integer at the specified byte offset, with optional endianness.

See [`DataView.setInt32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setInt32) on MDN.

## Examples

```rescript
DataView.setInt32(dv, 4, -123456)
DataView.setInt32(dv, 4, -123456, ~littleEndian=true)
```
*/
@send external setInt32: (t, int, int, ~littleEndian: bool=?) => unit = "setInt32"

/**
`setUint32(dataView, offset, value, ~littleEndian=?)` writes a 32-bit unsigned integer at the specified byte offset, with optional endianness.

See [`DataView.setUint32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setUint32) on MDN.

## Examples

```rescript
DataView.setUint32(dv, 4, 123456)
DataView.setUint32(dv, 4, 123456, ~littleEndian=true)
```
*/
@send external setUint32: (t, int, int, ~littleEndian: bool=?) => unit = "setUint32"

/**
`setFloat16(dataView, offset, value, ~littleEndian=?)` writes a 16-bit floating point number (if supported) at the specified byte offset, with optional endianness.

See [`DataView.setFloat16`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setFloat16) on MDN.

## Examples

```rescript
DataView.setFloat16(dv, 6, 1.5)
DataView.setFloat16(dv, 6, 1.5, ~littleEndian=true)
```
*/
@send external setFloat16: (t, int, float, ~littleEndian: bool=?) => unit = "setFloat16"

/**
`setFloat32(dataView, offset, value, ~littleEndian=?)` writes a 32-bit floating point number at the specified byte offset, with optional endianness.

See [`DataView.setFloat32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setFloat32) on MDN.

## Examples

```rescript
DataView.setFloat32(dv, 8, 3.14)
DataView.setFloat32(dv, 8, 3.14, ~littleEndian=true)
```
*/
@send external setFloat32: (t, int, float, ~littleEndian: bool=?) => unit = "setFloat32"

/**
`setFloat64(dataView, offset, value, ~littleEndian=?)` writes a 64-bit floating point number at the specified byte offset, with optional endianness.

See [`DataView.setFloat64`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setFloat64) on MDN.

## Examples

```rescript
DataView.setFloat64(dv, 10, 6.28)
DataView.setFloat64(dv, 10, 6.28, ~littleEndian=true)
```
*/
@send external setFloat64: (t, int, float, ~littleEndian: bool=?) => unit = "setFloat64"

/**
`setBigInt64(dataView, offset, value, ~littleEndian=?)` writes a 64-bit signed integer (BigInt) at the specified byte offset, with optional endianness.

See [`DataView.setBigInt64`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setBigInt64) on MDN.

## Examples

```rescript
DataView.setBigInt64(dv, 12, -123456789n)
DataView.setBigInt64(dv, 12, -123456789n, ~littleEndian=true)
```
*/
@send external setBigInt64: (t, int, bigint, ~littleEndian: bool=?) => unit = "setBigInt64"

/**
`setBigUint64(dataView, offset, value, ~littleEndian=?)` writes a 64-bit unsigned integer (BigInt) at the specified byte offset, with optional endianness.

See [`DataView.setBigUint64`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView/setBigUint64) on MDN.

## Examples

```rescript
DataView.setBigUint64(dv, 14, 123456789n)
DataView.setBigUint64(dv, 14, 123456789n, ~littleEndian=true)
```
*/
@send external setBigUint64: (t, int, bigint, ~littleEndian: bool=?) => unit = "setBigUint64"

/**
`ignore(dataView)` ignores the provided DataView and returns unit.

This helper is useful when you want to discard a value (for example, the result of an operation with side effects) without having to store or process it further.
*/
external ignore: t => unit = "%ignore"
