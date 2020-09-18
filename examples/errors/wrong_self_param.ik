type Foo class:
    fn foo(Self, Int) Int

instance Foo Bool:
    // x should be called self
    fn foo(x, self):
        return 0

fn main():
    print("\n")
