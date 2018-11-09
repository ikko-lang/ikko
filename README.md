# Ikko

Ikko is a programing language. See [ikkolang.com](ikkolang.com) for a bit more
information, or the examples directory for some example code.


## Building

This requires `stack` to be installed.

Run:

- `make` to build
- `make test` to run tests
- `make run` to run an example
- `stack exec athena -- examples/fib.at` to run a specific file

## Example

See the `examples/` directory for more examples.

```rust
fn identity<A>(a A) A {
    return a
}

fn main() {
    print(identity("Hello generics\n"))
}
```

## Features

- Static typing with type inference.
- No null value.
- The type system has generics and tagged unions.
- Garbage colection.
- Pattern matching.

Not yet implemented:

- Typeclasses (called `traits` in Rust).
- Any kind of standard library.
