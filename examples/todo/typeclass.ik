type PrettyPrint a class:
    fn prettyPrint(a): String

impl PrettyPrint Int:
    fn prettyPrint(n):
        return String(n)

// Instances can have additional predicates
impl PrettyPrint [a] where PrettyPrint a:
    fn prettyPrint(xs):
        return "[" + join(", ", map(prettyPrint, xs)) + "]"

// Methods on a class can have predicates
type Logger a class:
    fn log(a) where PrettyPrint a

type Counter a class:
    fn getCount(String): Int
    fn alterCount(String, Int)

// Classes can have superclasses
type MyClass a extends Counter a, Logger a:
     fn logCount(String)

fn main():
    print(prettyPrint(123))
