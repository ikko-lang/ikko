fn fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

fn main():
    let i = 1
    while i < 12:
        print(String(fib(i)))
        print("\n")
        i = i + 1
