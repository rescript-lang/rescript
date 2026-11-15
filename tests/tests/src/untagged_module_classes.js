// Classes that untagged_module_classes_test.res imports with @module. None
// of them is a global, so a check that reads a global throws.

export class Object3D {}

export class Mesh extends Object3D {}

export class Group extends Object3D {}

// A global of the same name is a different class
export class Shared {}

export const Addons = {
  Line2: class Line2 extends Object3D {},
};

export default class Widget {}
