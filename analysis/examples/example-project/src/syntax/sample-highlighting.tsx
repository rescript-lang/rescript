// Bindings
let numberBinding = 123;

const SomeComp = {
  Nested: () => null,
};

let someFunction = (param: number): number => {
  let innerBinding = param + 2;
  return innerBinding;
};

// Types
type someRecord<typeParameter> = {
  someField: number;
  someOtherField: string;
  theParam: typeParameter;
  another: boolean;
  to: string;
};

enum someEnum {
  SomeMember,
  AnotherMember,
}

// Destructuring
let destructuring = () => {
  let someVar = [1, 2, 3];
  let [one, two, three] = someVar;
  let someObj: someRecord<number> = {
    someField: 1,
    someOtherField: "hello",
    theParam: 2,
    another: true,
    to: "123",
  };
  let { someField, someOtherField, theParam } = someObj;

  return someField;
};

namespace SomeModule {
  export enum t {
    Some,
    Value,
    Here,
  }
}

// Decorators and classes
function someDecorator() {
  return function (
    target: any,
    propertyKey: string,
    descriptor: PropertyDescriptor
  ) {
    console.log("first(): called");
  };
}

class SomeClass {
  @someDecorator() doStuff() {
    return 123;
  }
}

// Strings
let interpolated = `${numberBinding} ${"123"}`;

// JSX
interface Props {
  someProp: number;
  otherProp: string;
  thirdProp: SomeModule.t;
}
const SomeComponent = ({ someProp, otherProp, thirdProp }: Props) => {
  return null;
};

let jsx = (
  <div>
    <SomeComponent
      someProp={123}
      otherProp="hello"
      thirdProp={SomeModule.t.Value}
    />
    <SomeComp.Nested />
    {"Hello"}
  </div>
);
function Property() {
  throw new Error("Function not implemented.");
}
