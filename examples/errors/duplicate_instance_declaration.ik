type Count class:
    fn count(Self) Int

instance Count Bool:
    fn count(self):
        if self:
            return 1
        return 0

instance Count Bool:
    fn count(self):
        if self:
            return 0
        return 1

fn main():
    print("\n")
