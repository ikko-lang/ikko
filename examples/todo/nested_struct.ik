type Bar struct:
  coord struct:
    x Int
    y Int
  color String

fn main():
  let b = Bar{
    coord: {x: 1, y: 2},
    color: "Blue",
  }
  print(String(b))
  print("\n")
