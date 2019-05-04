fn main():
    let list = prepend(5, prepend(4, prepend(3, End)))
    if contains(list, 3):
        print("List contains 3\n")

    if contains(list, 10):
        print("List contains 10\n")

type List<t> enum:
  End
  Link:
    value t
    next List<t>

fn contains(list, val):
    match list:
        End:
            return False
        Link(n, next):
            return n == val || contains(next, val)

fn prepend(val, list):
    return Link{value: val, next: list,}
