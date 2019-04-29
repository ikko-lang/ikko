type Bar struct:
    x Int
    y Float


type Foo struct:
    asdf Int
    xyz Bar


fn main():
    let foo = Foo{
        asdf: 123 + 345,
        xyz: Bar{
            x: 1,
            y: 2.333,
        },
    }
    let b = foo.xyz
    // print(String(foo))
    print("Hello, world\n")
    foo.xyz.x = 10000
    print("an Int: ")
    print(String(foo.xyz.x + 9))
    //print("\na Float: ")
    //print(String(foo.xyz.y * 0.6))
    print("\n")
