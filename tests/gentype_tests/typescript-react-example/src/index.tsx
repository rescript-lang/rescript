import type * as React from "react";
import * as ReactDOM from "react-dom";
import App from "./App.tsx";
import * as ImportJsValue from "./ImportJsValue.gen.tsx";
import * as Uncurried from "./Uncurried.gen.tsx";
import "./index.css";
import * as DocStrings from "./Docstrings.gen.tsx";
import Hooks from "./Hooks.gen.tsx";
import * as MyMath from "./MyMath.tsx";
import { Universe_Nested2_Nested3_nested3Value } from "./NestedModules.gen.tsx";
import * as Records from "./Records.gen.tsx";
import * as TestPromise from "./TestPromise.gen.tsx";
import * as Variants from "./Variants.gen.tsx";
import {
  printManyPayloads,
  printVariantWithPayload,
  printVariantWithPayloads,
  testManyPayloads,
  testVariantWithPayloads,
  testWithPayload,
} from "./VariantsWithPayload.gen.tsx";
import * as Types from "./nested/Types.gen.tsx";

const consoleLog = console.log;

const intList = Types.map(Types.someIntList, x => x + 1);

const businesses = [
  {
    address: "Poison road",
    name: "AcmeLTD",
    owner: { name: "John", age: 12, address: "garage" },
  },
];

const addresses = Records.findAllAddresses(businesses);

consoleLog("indList", intList);
consoleLog("addresses", addresses);

consoleLog("index.tsx roundedNumber:", ImportJsValue.roundedNumber);
consoleLog("index.tsx areaValue:", ImportJsValue.areaValue);
consoleLog(
  "index.tsx returnedFromHigherOrder:",
  ImportJsValue.returnedFromHigherOrder,
);

consoleLog(
  "index.tsx callback:",
  Uncurried.callback(() => 3),
);
consoleLog(
  "index.tsx callback2:",
  Uncurried.callback2({ login: () => "hello" }),
);
consoleLog(
  "index.tsx callback2U:",
  Uncurried.callback2U({ loginU: () => "hello" }),
);
Uncurried.sumU(3, 4);
Uncurried.sumU2(3)(4);
Uncurried.sumCurried(3)(4);
Uncurried.sumLblCurried("hello", 3)(4);

ReactDOM.render(
  <div>
    <App name={"Hello"} />
    <Hooks vehicle={{ name: "Car" }} />
  </div>,
  document.getElementById("root") as HTMLElement,
);

const x1 = Records.getPayload(Records.payloadValue).v;
const x2 = Records.getPayloadRecord(Records.payloadValue).v;
const x3 = Records.payloadValue.payload.v;
const x4 = Records.getPayloadRecordPlusOne(Records.payloadValue).v;
consoleLog("x1,x2,x3,x4 are", x1, x2, x3, x4);

consoleLog(
  "Universe_Nested2_Nested3_nested3Value: ",
  Universe_Nested2_Nested3_nested3Value,
);

consoleLog("Enums: swap(sunday) =", Variants.swap("sunday"));
consoleLog("Enums: fortytwoOK is", Variants.fortytwoOK);
consoleLog("Enums: fortytwoBAD is", Variants.fortytwoBAD);
consoleLog(
  "Variants: testConvert3to2('module') =",
  Variants.testConvert2to3("module"),
);
consoleLog("Variants: testConvert3to2(42) =", Variants.testConvert2to3(42));

const absoluteValueInstance = new MyMath.AbsoluteValue();
absoluteValueInstance.prop = -3;
consoleLog("absoluteValueInstance", absoluteValueInstance);

const propValue = ImportJsValue.useGetProp(absoluteValueInstance);
const absValue = ImportJsValue.useGetAbs(absoluteValueInstance);
consoleLog("ImportJsValue: getProp() =", propValue);
consoleLog("ImportJsValue: getAbs() =", absValue);

printVariantWithPayload("a");
printVariantWithPayload("b");
printVariantWithPayload("True");
printVariantWithPayload("Twenty");
printVariantWithPayload("Half");
printVariantWithPayload(testWithPayload({ NAME: "c", VAL: { x: 15 } }));

printManyPayloads({ NAME: "one", VAL: 34 });
printManyPayloads({ NAME: "two", VAL: ["hello", "world"] });
printManyPayloads(testManyPayloads({ NAME: "three", VAL: { x: 15 } }));

printVariantWithPayloads(testVariantWithPayloads("A"));
printVariantWithPayloads(testVariantWithPayloads({ TAG: "B", _0: 4 }));
printVariantWithPayloads(testVariantWithPayloads({ TAG: "C", _0: 1, _1: 2 }));
printVariantWithPayloads(testVariantWithPayloads({ TAG: "D", _0: 1, _1: 2 }));
printVariantWithPayloads(
  testVariantWithPayloads({ TAG: "E", _0: 1, _1: "hello", _2: 2 }),
);

TestPromise.convert(Promise.resolve({ x: 3, s: "hello" })).then(x =>
  consoleLog("TestPromise result:", x.result),
);

type Props = {
  readonly method?: "push" | "replace";
};

export const make: React.FC<Props> = (_x: Props) => {
  return <div />;
};

const signedMessage = DocStrings.signMessage("hello", 42);
consoleLog("signedMessage:", signedMessage);

export default make;
