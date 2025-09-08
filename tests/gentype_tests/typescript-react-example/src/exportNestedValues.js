// biome-ignore lint/complexity/noStaticOnlyClass: intended use
class InnerClass {
  static InnerStuff = {
    innerStuffContents: { x: 34 },
  };
}

// biome-ignore lint/complexity/noStaticOnlyClass: intended use
export class TopLevelClass {
  static MiddleLevelElements = {
    stuff: InnerClass,
  };
}

export const ValueStartingWithUpperCaseLetter =
  "ValueStartingWithUpperCaseLetter";

export default 42;
