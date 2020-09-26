fn identity(x t) t:
    return x

fn main():
    let id = identity
    id = id
    print(id("Hello world\n"))
