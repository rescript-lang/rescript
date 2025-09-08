@module("@wildcards/react-social-icons") @react.component
external make: (
  ~url: string=?,
  ~bgColor: string=?,
  ~fgColor: string=?,
  ~target: string=?,
  ~className: string=?,
  ~rel: string=?,
  ~network: string=?,
) => React.element = "SocialIcon"
