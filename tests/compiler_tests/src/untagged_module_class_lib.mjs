// Classes exported by a module, which untagged_module_class_types.res
// declares as @instanceOf types
export class UntaggedShape {}
export class UntaggedCircle extends UntaggedShape {}
export const shapes = { Square: class Square extends UntaggedShape {} };
export default class Polygon extends UntaggedShape {}
