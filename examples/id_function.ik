fn id(a):
    return a

fn f(n):
    return id(n > 3)

fn main():
    print(String(f(4)))
    print("\n")
