// TODO: Nothing catches this
type Foo class extends Bar:
    fn foo(Self) ()

type Bar class extends Foo:
    fn bar(Self) Int

fn main():
    print("\n")
