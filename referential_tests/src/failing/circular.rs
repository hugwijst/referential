use core::cell::Cell;

use referential::referential;

#[referential('a)]
struct Circular(Cell<Option<&'a Circular<()>>>);

fn main() {
}
