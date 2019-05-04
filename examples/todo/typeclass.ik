type PrettyPrint a class:
    fn prettyPrint(a): String

impl PrettyPrint Int:
    fn prettyPrint(n):
        return String(n)

fn main():
    print(prettyPrint(123))
