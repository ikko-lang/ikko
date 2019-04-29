type Bar struct:
    x Int
    y Float

fn get_x(b Bar) Int:
    return b.x

fn main():
    let b1 = Bar{
        x: 123,
        y: 0.0,
    }
    print(String(get_x(b1)))
    print("\n")
