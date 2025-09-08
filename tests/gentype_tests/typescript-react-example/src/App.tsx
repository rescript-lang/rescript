import * as React from "react";
import "./App.css";

export interface Props {
  name: string;
  count?: number;
}

class App extends React.PureComponent<Props> {
  public render() {
    return <div className="App" />;
  }
}

export default App;
