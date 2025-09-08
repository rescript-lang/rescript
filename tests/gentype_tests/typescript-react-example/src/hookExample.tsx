import * as React from "react";

export const foo = (person: { readonly name: string; readonly age: number }) =>
  person.name;

type Props = {
  readonly person: { readonly name: string; readonly age: number };
  readonly children: React.ReactNode;
  readonly renderMe: React.ComponentType<{
    randomString: string;
    readonly poly: string;
  }>;
};

export const make: React.FC<Props> = (x: Props) => {
  const RenderMe = x.renderMe;
  return (
    <div>
      {" "}
      {x.person.name} {x.children}{" "}
      <RenderMe randomString="random-string" poly="" />
    </div>
  );
};

class AsClassComponent extends React.PureComponent<Props> {
  public render() {
    const RenderMe = this.props.renderMe;
    return (
      <div>
        {" "}
        {this.props.person.name} {this.props.children}{" "}
        <RenderMe randomString="random-string" poly="" />
      </div>
    );
  }
}

export const makeRenamed = AsClassComponent;

export default make;
