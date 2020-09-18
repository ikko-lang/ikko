type Foo class:
    fn foo(Self) Int

instance Foo Bool:
    // x should be called self
    fn foo(x):
        return 0

fn main():
    print("\n")
