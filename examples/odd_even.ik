type Even enum:
  EElem:
    value Int
    next Odd
  End

type Odd struct:
  value Int
  next Even


fn main():
  let o = Odd{
    value: 1,
    next: EElem{
      value: 2,
      next: Odd{
        value: 3,
        next: End{
        },
      },
    },
  }
  print(String(o))
