type Count class:
    fn count(Self) Int

type Maybe<val> enum:
    Nothing
    Just:
        value val

instance Count Maybe<a> where a: Eq:
    fn count(self):
        match self:
            Nothing():
                return 0
            Just(value):
                if value == value:
                    return 1
                else:
                    return 2

fn use_instance() ():
    let m = Just{
        value: "foo",
    }
    print(String(count(m)))

fn main():
    print("\n")
