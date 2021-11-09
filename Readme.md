# `referential!` - safe self-referential structures

Yet another way to create self-referential structs.

This solution is quite similar to [`self_cell`](https://crates.io/crates/self_cell), but it allows for any owning type as long as it can be `Pin`ned.
