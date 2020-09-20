fn more_general<A, B>(x A, y B) A:
    return x

fn less_general<A>(x A, y A) A:
    return x

fn main():
    let f = more_general
    f = less_general
