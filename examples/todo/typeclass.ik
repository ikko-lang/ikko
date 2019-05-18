trait PrettyPrint:
    fn prettyPrint(Self): String

impl PrettyPrint Int:
    fn prettyPrint(n):
        return String(n)

// Instances can have additional predicates
impl PrettyPrint [a] where PrettyPrint a:
    fn prettyPrint(xs):
        return "[" + join(", ", map(prettyPrint, xs)) + "]"

// Methods on a trait can have predicates
trait Logger extends PrettyPrint:
    fn log(Self, to t) where Writer t

trait Counter a:
    fn getCount(String): Int
    fn alterCount(String, Int)

// Classes can have superclasses
trait MyClass a where Counter a, Logger a:
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
