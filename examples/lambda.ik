fn multiples_of(i Int) fn() Int
    let x = 0
    let f = fn():
        let y = x
        x = x + i
        return y
    return f

fn println_(s):
    print(s)
    print("\n")

fn putln(i):
    println_(String(i))

fn main():
    let m = multiples_of(3)
    putln(m())
    putln(m())
    putln(m())
    putln(m())
