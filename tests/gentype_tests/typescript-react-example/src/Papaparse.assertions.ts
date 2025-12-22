export {};

type $RescriptTypeSatisfiesTypeScriptType<RescriptType extends TypeScriptType, TypeScriptType> = RescriptType;

type parseError = $RescriptTypeSatisfiesTypeScriptType<{
  /** A generalization of the error */
  readonly type: 
    "Delimiter"
  | "FieldMismatch"
  | "Quotes"; 
  /** Standardized error code */
  readonly code: 
    "TooManyFields"
  | "MissingQuotes"
  | "UndetectableDelimiter"
  | "TooFewFields"
  | "InvalidQuotes"; 
  /** Human-readable details */
  readonly message: string; 
  /** Row index of parsed data where error is */
  readonly row?: number; 
  /** Index within the row where error is */
  readonly index?: number
}, import("papaparse").ParseError>;
type parseMeta = $RescriptTypeSatisfiesTypeScriptType<{
  /** Delimiter used */
  readonly delimiter: string; 
  /** Line break sequence used */
  readonly linebreak: string; 
  /** Whether process was aborted */
  readonly aborted: boolean; 
  /** Array of field names */
  readonly fields?: string[]; 
  /** Whether preview consumed all input */
  readonly truncated: boolean; 
  readonly cursor: number
}, import("papaparse").ParseMeta>;
type parseResult<t> = $RescriptTypeSatisfiesTypeScriptType<{
  /** * an array of rows. If header is false, rows are arrays; otherwise they are objects of data keyed by the field name. */
  readonly data: t[]; 
  /** an array of errors. */
  readonly errors: parseError[]; 
  /** * contains extra information about the parse, such as delimiter used,
     * the newline sequence, whether the process was aborted, etc.
     * Properties in this object are not guaranteed to exist in all situations. */
  readonly meta: parseMeta
}, import("papaparse").ParseResult<t>>;

const parse = undefined as unknown as typeof import("papaparse").parse satisfies <t>(_1:string) => parseResult<t>;
