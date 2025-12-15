/* @flow strict */

class InnerClass {
  static InnerStuff = {
    innerStuffContents: { x: 34 }
  };
}

export class TopLevelClass {
  static MiddleLevelElements = {
    stuff: InnerClass
  };
}

export const ValueStartingWithUpperCaseLetter = "ValueStartingWithUpperCaseLetter";

export default 42;