trait Logger extends PrettyPrint:
    fn logTypedReturn(Self, to t) () where Writer t
    fn log(Self, to t) where  Writer t

trait Counter:
    fn getCount(String) Int

    // A comment!
    fn alterCount(String, Int)


fn main():
    print("Hello world\n")
