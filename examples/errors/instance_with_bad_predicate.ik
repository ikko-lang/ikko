type Count class:
    fn count(Self) Int

// `a` isn't part of the implemented type
instance Count Bool where a: Eq:
    fn count(self):
        if self:
            return 1
        return 0

fn main():
    print("x")
