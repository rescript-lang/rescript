module Button = {
  type props = {title: string, color: string, onPress: unit => unit}

  @module("react-native")
  external make: React.component<props> = "Button"
}
