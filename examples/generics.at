type Pair<A, B> struct {
    first A
    second B
}

type Maybe<Val> enum {
    Nothing
    Just {
        value Val
    }
}


fn isJust<V>(m Maybe<V>) Bool {
    match m {
        Nothing() {
            return False
        }
        Just(_) {
            return True
        }
    }
}


fn flip<A, B>(p Pair<A, B>) Pair<B, A> {
    return Pair{
        first: p.second,
        second: p.first,
    }
}

fn foo<A, B>(p Pair<A, B>) Pair<A, B> {
    return p
}

fn main() {
    let p1 = Pair{
        first: 123,
        second: "foo",
    }
    print(String(p1))
    print("\n")
    print(String(flip(p1)))
    print("\n")
    let m2 Maybe<Pair<Int, Int>> = Nothing{}
    print(String(isJust(m2)))
    print("\n")
}
