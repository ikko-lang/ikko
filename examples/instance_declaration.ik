type Maybe<val> enum:
    Nothing
    Just:
        value val

type Count class:
    fn count(Self) Int

instance Count Bool:
    fn count(self):
        if self:
            return 1
        return 0

instance Count Maybe<a> where a: Count:
    fn count(self):
        match self:
            Nothing():
                return 0
            Just(value):
                return count(value)

fn main():
    print("\n")
