import type * as React from "react";

type Props = React.InputHTMLAttributes<HTMLInputElement>;

const MyInput: React.FC<Props> = props => <input {...props} />;

export default MyInput;
