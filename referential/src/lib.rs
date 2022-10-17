pub use stable_deref_trait::StableDeref;

pub use referential_macros::referential;

/// Trait to create a referencing struct from some owned data.
pub trait FromOwned<'a, P: ?Sized> {
    fn from_owned(data: &'a P) -> Self;
}
