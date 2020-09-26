type LinkedList<t> enum:
  End
  Link:
    value t
    next LinkedList<t>

fn map(f, list):
    match list:
        End:
            return End
        Link(val, next):
            return Link{
                value: f(val),
                next: map(f, next),
            }

fn fold(reducer, init, list):
    match list:
        End:
            return init
        Link(val, next):
            let rest = fold(reducer, init, next)
            return reducer(val, rest)

fn prepend(val, list):
    return Link{
        value: val,
        next: list,
    }

fn singleton(val):
    return prepend(val, End)

fn double(x):
    return x + x

fn add(x, y):
    return x + y

fn println(s):
    print(s)
    print("\n")

fn main():
    let l1 = prepend(1, prepend(2, singleton(3)))
    let l2 = map(double, l1)
    println(String(l2))
    println(String(fold(add, 0, l2)))
