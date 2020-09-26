type Container<a> struct:
    function fn(a) a

fn broken_identity(x t) t:
    print("called broken_identity\n")
    return broken_identity(x)

fn identity(x t) t:
    print("called identity\n")
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
