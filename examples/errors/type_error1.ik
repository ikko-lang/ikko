fn main():
    let a = 1
    let b = 0
    while a < 200:
        print(String(a))
        print("\n")
        let temp = a
        a = a + b
        b = String(temp)
