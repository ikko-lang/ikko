fn identity<T>(x T) T:
    return x

fn main():
    let id = identity
    id = id
    print(id("Hello world\n"))
