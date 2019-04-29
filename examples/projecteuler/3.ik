fn main():
    /* Block comments work */
    let n = 600851475143
    let largest = 0
    let factor = 2
    while n >= factor:
        if n % factor == 0:
            largest = factor
            n = n / factor
        else:
            factor =
                factor + // can do funny line wraps
                1

    print(String(largest))
    print("\n")
