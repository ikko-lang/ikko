fn are_same<T>(a T, b T) Bool where T: Eq:
    return a == b

fn main():
    print(are_same("x", "y"))
    print(are_same(False, False))
