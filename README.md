# `referential!` - safe self-referential structures

Yet another way to create self-referential structs.

This solution is quite similar to [`self_cell`](https://crates.io/crates/self_cell), but it allows for any owning type as long as it can be `Pin`ned.

## Example
```rust
use referential::referential;

struct Indices<'a> {
    words: Vec<&'a str>,
}

referential!{
    struct TextParser + 'a (Indices<'a>);
}

let parser = TextParser::new_with(
    "Hello beautiful world!",
    |s| Indices { words: s.split_whitespace().collect() }
);

assert_eq!(parser.referencing().words, ["Hello", "beautiful", "world!"]);
assert!(std::ptr::eq(parser.owning().as_ptr(), parser.referencing().words[0].as_ptr()));
```

## Safety
This crate has not seen much real-world usage and has not been reviewed, so usage could lead to undefined behavior.

## Minimum Stable Rust Version
`referential` uses `Pin::into_inner`, which requires at least **Rust 1.39.0** to compile.

## Related projects
* [`self_cell`](https://crates.io/crates/self_cell)
* [`ouroboros`](https://crates.io/crates/ouroboros)
* [`rental`](https://crates.io/crates/rental)
* [`owning_ref`](https://crates.io/crates/owning_ref)
* [`ghost-cell`](https://crates.io/crates/ghost-cell)

## License

Licensed under either of
* Apache License, Version 2.0
* MIT License
