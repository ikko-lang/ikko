# Ikko

Ikko is a programing language. See ikkolang.com for a bit more information, or
the examples directory for some example code.


## Building

This requires `stack` to be installed.

Run:

- `make` to build
- `make test` to run tests
- `make run` to run an example
- `stack exec athena -- examples/fib.at` to run a specific file

## Example

See the `examples/` directory for more examples.

```
fn identity<A>(a A) A {
    return a
}

fn main() {
    print(identity("Hello generics\n"))
}
```
