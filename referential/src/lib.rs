// #![cfg_attr(not(test), no_std)]

pub use stable_deref_trait::StableDeref;

// #[doc(hidden)]
pub use referential_macros::referential;

// mod test;

pub trait FromData<'a, P: ?Sized> {
    fn from_data(data: &'a P) -> Self;
}
