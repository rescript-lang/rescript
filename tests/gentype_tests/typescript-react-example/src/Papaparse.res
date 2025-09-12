@gentype.satisfies(("papaparse", "ParseError"))
type parseError = {
  /** A generalization of the error */
  @as("type") type_: [#Quotes | #Delimiter | #FieldMismatch],
  /** Standardized error code */
  code: [#MissingQuotes | #UndetectableDelimiter | #TooFewFields | #TooManyFields | #InvalidQuotes],
  /** Human-readable details */
  message: string,
  /** Row index of parsed data where error is */
  row?: int,
  /** Index within the row where error is */
  index?: int,
}

@gentype.satisfies(("papaparse", "ParseMeta"))
type parseMeta = {
  /** Delimiter used */
  delimiter: string,
  /** Line break sequence used */
  linebreak: string,
  /** Whether process was aborted */
  aborted: bool,
  /** Array of field names */
  fields?: array<string>,
  /** Whether preview consumed all input */
  truncated: bool,
  cursor: float,
}

@gentype.satisfies(("papaparse", "ParseResult"))
type parseResult<'t> = {
  /**
     * an array of rows. If header is false, rows are arrays; otherwise they are objects of data keyed by the field name.
     */
  data: array<'t>,
  /** an array of errors. */
  errors: array<parseError>,
  /**
     * contains extra information about the parse, such as delimiter used,
     * the newline sequence, whether the process was aborted, etc.
     * Properties in this object are not guaranteed to exist in all situations.
     */
  meta: parseMeta,
}

@gentype.satisfies(("papaparse", "parse"))
external parseCsvString: string => parseResult<'t> = "parse"
