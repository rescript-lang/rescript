
export abstract class EmptyList {
  protected opaque: unknown;
}

export abstract class Cons<T> {
  protected opaque!: T;
}

export type list<T> = Cons<T> | EmptyList;
