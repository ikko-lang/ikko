type Container<A> struct:
    function fn(A) A

fn broken_identity<T>(x T) T:
    return broken_identity(x)

fn identity<T>(x T) T:
    return x

fn main():
    let c = Container{
        function: broken_identity,
    }

    c.function = identity

    let f1 = c.function
    f1(True)

    let f2 = c.function
    print(f2("Hello structs\n"))
