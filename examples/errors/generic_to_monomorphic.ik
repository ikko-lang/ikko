fn identity(x t) t:
    return x

fn same_int(x Int) Int:
    return x

fn main():
    let id = identity
    id = same_int
    print(id("Hello world\n"))
