type Count class:
    fn count(Self) Int

instance Count Bool:
    fn count(self):
        // bad usage of `self`
        return self + 1

fn main():
    print("")
