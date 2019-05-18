type PrettyPrint a class:
    fn prettyPrint(a): String

impl PrettyPrint for Int:
    fn prettyPrint(n):
        return String(n)

// Instances can have additional predicates
impl PrettyPrint for [a] where (PrettyPrint a):
    fn prettyPrint(xs):
        return "[" + join(", ", map(prettyPrint, xs)) + "]"

// Methods on a class can have predicates
type Logger a class:
    fn log(a) where PrettyPrint a

type Counter a class:
    fn getCount(String): Int
    fn alterCount(String, Int)

// Classes can have superclasses
type MyClass a class extends Counter a, Logger a:
     fn logCount(String)

// function that uses a typeclass
fn foo(x a) a
where (MyClass a):
  x.logCount("foo")
  return x

fn bar(x [a]) String
where PrettyPrint a => prettyPrint(x)

fn baz(vals [a]) String where Show a, Ord a:
  return ", ".join(map(_.show, sorted(vals)))

fn main():
    print(prettyPrint(123))
