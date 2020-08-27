type Foo class:
    fn foo(Self) Int

instance Count Bool:
    fn foo(self):
        return 0
    // bar isn't in the class
    fn bar(self):
        return False

fn main():
    print("\n")
