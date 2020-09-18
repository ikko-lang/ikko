type Foo class:
    fn foo(Self) Int
    fn bar(Self) Bool

instance Foo Bool:
    // missing bar
    fn foo(self):
        return 0

fn main():
    print("\n")
