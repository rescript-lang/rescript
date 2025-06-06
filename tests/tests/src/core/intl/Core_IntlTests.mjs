// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib_JsExn from "rescript/lib/es6/Stdlib_JsExn.js";
import * as Stdlib_Option from "rescript/lib/es6/Stdlib_Option.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as Core_Intl_LocaleTest from "./Core_Intl_LocaleTest.mjs";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";
import * as Core_Intl_CollatorTest from "./Core_Intl_CollatorTest.mjs";
import * as Core_Intl_SegmenterTest from "./Core_Intl_SegmenterTest.mjs";
import * as Core_Intl_ListFormatTest from "./Core_Intl_ListFormatTest.mjs";
import * as Core_Intl_PluralRulesTest from "./Core_Intl_PluralRulesTest.mjs";
import * as Core_Intl_NumberFormatTest from "./Core_Intl_NumberFormatTest.mjs";
import * as Core_Intl_DateTimeFormatTest from "./Core_Intl_DateTimeFormatTest.mjs";
import * as Core_Intl_RelativeTimeFormatTest from "./Core_Intl_RelativeTimeFormatTest.mjs";

console.log("---");

console.log("Intl");

console.log(Intl.getCanonicalLocales("EN-US"));

console.log(Intl.getCanonicalLocales([
  "EN-US",
  "Fr"
]));

try {
  console.log(Intl.getCanonicalLocales("bloop"));
} catch (raw_e) {
  let e = Primitive_exceptions.internalToException(raw_e);
  if (e.RE_EXN_ID === "JsExn") {
    console.error(e._1);
  } else {
    throw e;
  }
}

try {
  console.log(Intl.supportedValuesOf("calendar"));
  console.log(Intl.supportedValuesOf("collation"));
  console.log(Intl.supportedValuesOf("currency"));
  console.log(Intl.supportedValuesOf("numberingSystem"));
  console.log(Intl.supportedValuesOf("timeZone"));
  console.log(Intl.supportedValuesOf("unit"));
} catch (raw_e$1) {
  let e$1 = Primitive_exceptions.internalToException(raw_e$1);
  if (e$1.RE_EXN_ID === "JsExn") {
    console.error(e$1._1);
  } else {
    throw e$1;
  }
}

try {
  Intl.supportedValuesOf("someInvalidKey");
  console.error("Shouldn't have been hit");
} catch (raw_e$2) {
  let e$2 = Primitive_exceptions.internalToException(raw_e$2);
  if (e$2.RE_EXN_ID === "JsExn") {
    let e$3 = e$2._1;
    let message = Stdlib_Option.map(Stdlib_JsExn.message(e$3), prim => prim.toLowerCase());
    let exit = 0;
    if (message === "invalid key : someinvalidkey") {
      console.log("Caught expected error");
    } else {
      exit = 1;
    }
    if (exit === 1) {
      console.warn("Unexpected error message: \"" + message + "\"");
      throw e$3;
    }
    
  } else {
    let e$4 = Stdlib_JsExn.fromException(e$2);
    if (e$4 !== undefined) {
      throw Primitive_option.valFromOption(e$4);
    }
    console.error("Unexpected error");
  }
}

let _collator = Core_Intl_CollatorTest._collator;

let collator = Core_Intl_CollatorTest.collator;

let resolvedOptions = Core_Intl_DateTimeFormatTest.resolvedOptions;

let timeZone = Core_Intl_DateTimeFormatTest.timeZone;

let _locale = Core_Intl_LocaleTest._locale;

let locale = Core_Intl_LocaleTest.locale;

let currencyFormatter = Core_Intl_NumberFormatTest.currencyFormatter;

let roundingFormatter = Core_Intl_NumberFormatTest.roundingFormatter;

let groupingFormatter1 = Core_Intl_NumberFormatTest.groupingFormatter1;

let groupingFormatter2 = Core_Intl_NumberFormatTest.groupingFormatter2;

let sigFormatter = Core_Intl_NumberFormatTest.sigFormatter;

let options = Core_Intl_NumberFormatTest.options;

let _formatter = Core_Intl_SegmenterTest._formatter;

let formatter = Core_Intl_SegmenterTest.formatter;

let segments = Core_Intl_SegmenterTest.segments;

export {
  _collator,
  collator,
  resolvedOptions,
  timeZone,
  _locale,
  locale,
  currencyFormatter,
  roundingFormatter,
  groupingFormatter1,
  groupingFormatter2,
  sigFormatter,
  options,
  _formatter,
  formatter,
  segments,
}
/*  Not a pure module */
