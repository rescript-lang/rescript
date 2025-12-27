import type * as rescript from "@rescript/runtime/types";

export interface field {
  readonly name: string;
  readonly docstrings: string[];
  readonly signature: string;
  readonly optional: boolean;
  readonly deprecated?: rescript.option<string>;
}

export type constructorPayload =
  | { readonly kind: "inlineRecord"; readonly fields: field[] };

export interface constructor {
  readonly name: string;
  readonly docstrings: string[];
  readonly signature: string;
  readonly deprecated?: rescript.option<string>;
  readonly payload?: rescript.option<constructorPayload>;
}

export interface typeInSignature {
  readonly path: string;
  readonly genericTypeParameters: typeInSignature[];
}

export interface signatureDetails {
  readonly parameters: typeInSignature[];
  readonly returnType: typeInSignature;
}

export type detail =
  | { readonly kind: "record"; readonly items: field[] }
  | { readonly kind: "variant"; readonly items: constructor[] }
  | { readonly kind: "signature"; readonly details: signatureDetails };

export interface source {
  readonly filepath: string;
  readonly line: number;
  readonly col: number;
}

export type item =
  | {
    readonly kind: "value";
    readonly id: string;
    readonly docstrings: string[];
    readonly signature: string;
    readonly name: string;
    readonly deprecated?: rescript.option<string>;
    readonly source: source;
    readonly detail?: rescript.option<detail>;
  }
  | {
    readonly kind: "type";
    readonly id: string;
    readonly docstrings: string[];
    readonly signature: string;
    readonly name: string;
    readonly deprecated?: rescript.option<string>;
    readonly source: source;
    readonly detail?: rescript.option<detail>;
  }
  | {
    readonly kind: "module";
    readonly id: string;
    readonly docstrings: string[];
    readonly deprecated?: rescript.option<string>;
    readonly name: string;
    readonly moduletypeid?: rescript.option<string>;
    readonly source: source;
    readonly items: item[];
  }
  | {
    readonly kind: "moduleType";
    readonly id: string;
    readonly docstrings: string[];
    readonly deprecated?: rescript.option<string>;
    readonly name: string;
    readonly source: source;
    readonly items: item[];
  }
  | {
    readonly kind: "moduleAlias";
    readonly id: string;
    readonly docstrings: string[];
    readonly name: string;
    readonly source: source;
    readonly items: item[];
  };

export interface doc {
  readonly name: string;
  readonly deprecated: rescript.option<string>;
  readonly docstrings: string[];
  readonly source: source;
  readonly items: item[];
}
