fn more_general<a, b>(x a, y b) a:
    return x

fn less_general<a>(x a, y a) a:
    return x

fn main():
    let f = more_general
    f = less_general
