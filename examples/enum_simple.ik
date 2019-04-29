type Color enum:
  Red
  Blue
  Green


type Channel struct:
  color Color
  value Int


fn main():
  let a Color = Red{}
  a = Blue{}
  let c Channel = Channel{
    color: a,
    value: 123,
  }
  print(String(a))
  print("\n")
  print(String(c))
  print("\n")
