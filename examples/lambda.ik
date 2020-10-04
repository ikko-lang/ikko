fn multiples_of(i Int) fn() Int:
    // TODO: This should still work even if x isn't annotated
    let x Int = 0
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
