type LinkedList<T> enum {
  End
  Link {
    value T
    next LinkedList<T>
  }
}

fn map<A, B>(f func(A) B, list LinkedList<A>) LinkedList<B> {
    match list {
        End() {
            return End{}
        }
        Link(val, next) {
            return Link{
                value: f(val),
                next: map(f, next),
            }
        }
    }
}

fn fold<A, B>(reducer func(A, B) B, init B, list LinkedList<A>) B {
    match list {
        End() {
            return init
        }
        Link(val, next) {
            let rest = fold(reducer, init, next)
            return reducer(val, rest)
        }
    }
}

fn prepend<A>(val A, list LinkedList<A>) LinkedList<A> {
    return Link{
        value: val,
        next: list,
    }
}

fn singleton<A>(val A) LinkedList<A> {
    return prepend(val, End{})
}

fn double(x Int) Int {
    return x + x
}

fn add(x Int, y Int) Int {
    return x + y
}

fn println(s String) () {
    print(s)
    print("\n")
}

fn main() {
    let l1 = prepend(1, prepend(2, singleton(3)))
    let l2 = map(double, l1)
    println(String(l2))
    println(String(fold(add, 0, l2)))
}
