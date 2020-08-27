type Foo class:
    fn foo(Int) Self

instance Count Bool:
    // self isn't of type Self
    fn foo(self):
        return True

fn main():
    print("\n")
