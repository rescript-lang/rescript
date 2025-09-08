export const round: (_: number) => number = Math.round;

export const round2 = round;

export const area = (point: { x: number; y?: number }): number =>
  point.x * (point.y === undefined ? 1 : point.y);

export type numberOrString = number | string;

export const returnMixedArray = (): Array<number | string> => [1, 2];

export class AbsoluteValue {
  public prop!: number;
  public getProp(): number {
    return this.prop;
  }
  public getAbs(): number {
    return this.prop < 0 ? -this.prop : this.prop;
  }
}

export type stringFunction = (_: string) => string;

export const useColor = (_x: "tomato" | "gray"): number => 0;

export const higherOrder = (foo: (_1: number, _2: number) => number) =>
  foo(3, 4);

// biome-ignore lint/suspicious/noExplicitAny: intended use
export const convertVariant = (x: any) => x;

export const polymorphic = <T>(x: T): T => x;

export type num = number;

export type polyType<T> = { x: T };

export default 34;
