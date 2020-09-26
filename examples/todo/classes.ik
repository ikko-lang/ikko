class Printable:
  fn print(self)

type Color enum:
  Red
  Green
  Blue

instance Printable Color:
  fn print(self):
    print(self.to_string())

type Point struct:
  x Int
  y Int

instance Printable Point:
  fn print(self):
     print("x: {}, y: {}".format(x, y))

fn takes_printable(p a) where a: Printable:
  p.print()
