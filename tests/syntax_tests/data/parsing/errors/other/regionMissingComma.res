external make: (
  ~style: ReactDOMRe.Style.t=?.
  ~image: bool=?,
) => React.element = "ModalContent"

type student<'extraInfo> = {
  name: string,
  age: int
  otherInfo: 'extraInfo
}
