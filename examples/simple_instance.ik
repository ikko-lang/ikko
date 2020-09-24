type Count class:
    fn count(Self) Int

instance Count Bool:
    fn count(self):
        if self:
            return 1
        return 0

fn can_typecheck():
    // But can't be evaluated yet
    count(True) + count(False)

fn main():
    print("simple instance\n")
