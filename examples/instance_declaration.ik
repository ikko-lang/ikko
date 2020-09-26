type Count class:
    fn count(Self) Int

instance Count Bool:
    fn count(self):
        if self:
            return 1
        return 0


type Result<ok, err> enum:
    Ok:
        value ok
    Err:
        error err

instance Count Result<a, b>:
    fn count(self):
        match self:
            Err(_):
                return 0
            Ok(_):
                return 1

fn main():
    print("\n")
