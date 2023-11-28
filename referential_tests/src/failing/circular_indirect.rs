use core::cell::Cell;

use referential::referential;

struct CircularIndirect<'a>(Cell<Option<&'a CircularIndirect<'a>>>);

#[referential('a)]
struct Circular(CircularIndirect<'a>);

fn main() {
}
