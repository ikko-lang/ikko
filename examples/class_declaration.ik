type Sized class:
    fn size(Self) Int

type Use class:
    fn use<T>(Self, T) ()

type Storage class:
    fn store<T>(Self, T) () where T: Sized

type MyOrd class extends Use, Sized:
    fn cmp(Self, Self) Bool


fn main():
    print("Hello\n")
