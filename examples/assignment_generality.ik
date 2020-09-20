fn more_general<A>(x A) A:
    return x

fn just_as_general<A>(x A) A:
    return x

fn less_general(x Int) Int:
    return x

fn reasign_same_generality():
    let f = more_general
    f = just_as_general

fn reasign_more_general():
    let f = less_general
    f = more_general

fn main():
    print("hello\n")
