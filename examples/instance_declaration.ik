type Maybe<Val> enum:
    Nothing
    Just:
        value Val

type Count class:
    fn count(Self) Int

instance Count Bool:
    fn count(self):
        if self:
            return 1
        return 0

instance Count Maybe<A> where A: Count:
    fn count(self):
        match self:
            Nothing():
                return 0
            Just(value):
                return count(value)

fn main():
    print("\n")
