type Sized class:
    fn size(Self) Int

type Use class:
    fn use(Self, t) ()

type Storage class:
    fn store(Self, t) () where t: Sized

type MyOrd class extends Use, Sized:
    fn cmp(Self, Self) Bool


fn main():
    print("Hello\n")
