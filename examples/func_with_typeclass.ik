fn are_same(a t, b t) Bool where t: Eq:
    return a == b

fn max(a t, b t) t where t: Ord:
    if b > a:
        return b
    return a


fn main():
    print(String(are_same("x", "y")))
    print("\n")

    print(String(are_same(False, False)))
    print("\n")

    print(String(max(42, 6*7)))
    print("\n")

    print(String(max(2.78, 3.14)))
    print("\n")
