fn use_int(a Int) Int:
    return a

fn use_two_types(a fn(Bool) Bool, b fn(String) String):
    print(String(a(True)))
    print("\n")
    print(String(b("foo")))
    print("\n")

fn more_general(x a) a:
    return x

fn make_less_general(y a) fn(a) a:
    return more_general

fn problem(x):
    // Set f to something that is polymorphic
    let f = more_general

    // Reassign f to something monomorphic, where the type is not yet known
    f = make_less_general(x)

    // use f at two different types, requiring f to be polymorphic
    use_two_types(f, f)

    // force x to be an int (discovered after inferring the type of the assignment)
    use_int(x)

fn main():
    problem(1234)
    print("hello\n")
