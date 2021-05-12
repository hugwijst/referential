#![cfg_attr(not(test), no_std)]

trait Referencer<'a, P: ?Sized> {
    fn from_data(data: &'a P) -> Self;
}

#[macro_export]
macro_rules! referential {
    (
        @struct (<> $($tail:tt)*) -> ($(#[$attr:meta])*, $vis:vis, $name:ident)
    ) => {
        referential!{ @own_lifetime ($($tail)*) -> ($(#[$attr])*, $vis, $name, ()) }
    };
    (
        @struct (<$($own_gen_lifetime:lifetime $(: $own_first_lifetime_bound:lifetime $(+ $own_other_lifetime_bound:lifetime)*)?),* $(,)?> $($tail:tt)*) -> ($(#[$attr:meta])*, $vis:vis, $name:ident)
    ) => {
        referential!{ @own_lifetime ($($tail)*) -> ($(#[$attr])*, $vis, $name, ($($own_gen_lifetime $(: $own_first_lifetime_bound $(+ $own_other_lifetime_bound)*)?),*,)) }
    };
    (
        @struct ($($tail:tt)*) -> ($(#[$attr:meta])*, $vis:vis, $name:ident)
    ) => {
        referential!{ @own_lifetime ($($tail)*) -> ($(#[$attr])*, $vis, $name, ()) }
    };
    (
        @own_lifetime (+ $own_lifetime:lifetime ($ref:ident);) -> ($(#[$attr:meta])*, $vis:vis, $name:ident, ($($new_lifetime_bounds:tt)*))
    ) => {
        referential!{ @own_lifetime (+ $own_lifetime ($ref<>);) -> ($(#[$attr])*, $vis, $name, ($($new_lifetime_bounds)*)) }
    };
    (
        @own_lifetime (+ $own_lifetime:lifetime ($ref:ident<$($ref_lifetimes:lifetime),*>);) -> ($(#[$attr:meta])*, $vis:vis, $name:ident, ($($new_lifetime_bounds:tt)*))
    ) => {
        referential!{ @generate ($) $(#[$attr])*, $vis, $name, ($($new_lifetime_bounds)*), $own_lifetime, $ref, ($($ref_lifetimes),*) }
    };
    // The argument `$d` is used required to be `$`, to make it so repitions don't get matched when
    // evaluating the `@generate` rule. See https://github.com/rust-lang/rust/issues/35853.
    (
        @generate ($d:tt) $(#[$attr:meta])*, $vis:vis, $name:ident, ($($new_lifetime_bounds:tt)*), $own_lifetime:lifetime, $ref:ident, ($($ref_lifetimes:lifetime),*)
    ) => {
        // We create a new macro which can match on the tokens of `$own_lifetime`. This
        // way we can replace `$own_lifetime` by `'static`, which is captured in `$d lt_static`.
        // Note that we cannot directly use `$` here, so we pass in the `$` token as `$d`.
        macro_rules! __lifetime_replaced {
            (
                [] -> ($d($d lt_static:lifetime),* $d(,)?)
            ) => {
                // The order of elements here is critical to ensure safety: the
                // drop order is the same as declared here. First dropping the referenced
                // data is obviously unsafe, dropping the references first should be safe.
                $vis struct $name<$($new_lifetime_bounds)* P>($ref<$d ($d lt_static),*>, ::std::pin::Pin<P>)
                where
                    P: ::core::ops::Deref,
                    <P as ::core::ops::Deref>::Target: Unpin;

                impl<$($new_lifetime_bounds)* P> $name<$($new_lifetime_bounds)* P>
                where
                    P: ::core::ops::Deref,
                    <P as ::core::ops::Deref>::Target: Unpin,
                {
                    #![allow(dead_code)]

                    pub fn new_with<F>(pinnable: P, f: F) -> Self
                    where
                        F: for<$own_lifetime> FnOnce(&$own_lifetime <P as ::core::ops::Deref>::Target) -> $ref<$($ref_lifetimes),*>
                    {
                        let pinned_data = ::core::pin::Pin::new(pinnable);
                        let references_local = (f)(pinned_data.as_ref().get_ref());
                        let references_static = unsafe {
                            ::core::mem::transmute(references_local)
                        };

                        Self(references_static, pinned_data)
                    }

                    pub fn pinned<$own_lifetime>(&$own_lifetime self) -> &$own_lifetime <P as ::core::ops::Deref>::Target {
                        self.1.as_ref().get_ref()
                    }

                    pub fn referenced<$own_lifetime>(&$own_lifetime self) -> &$own_lifetime $ref<$($ref_lifetimes),*> {
                        &self.0
                    }
                }

                impl<$($new_lifetime_bounds)* P> $name<$($new_lifetime_bounds)* P>
                where
                    P: ::core::ops::Deref,
                    <P as ::core::ops::Deref>::Target: Unpin,
                    for<$own_lifetime> $ref<$($ref_lifetimes),*>: $crate::Referencer<$own_lifetime, <P as ::core::ops::Deref>::Target>,
                {
                    #![allow(dead_code)]

                    pub fn new(pinnable: P) -> Self {
                        Self::new_with(pinnable, |p_ref| $ref::from_data(p_ref))
                    }
                }
            };
            (
                [$own_lifetime $d(, $d lt:lifetime)* $d(,)?] -> ($d($d lt_static_accum:tt)*)
            ) => {
                __lifetime_replaced!{ [$d($d lt),*] -> ($d($d lt_static_accum)* 'static,) }
            };
            (
                [$d i:lifetime $d(, $d lt:lifetime)* $d(,)?] -> ($d($d lt_static_accum:tt)*)
            ) => {
                __lifetime_replaced!{ [$d($d lt),*] -> ($d($d lt_static_accum)* $d i,) }
            };
        }

        __lifetime_replaced!{ [$($ref_lifetimes),*] -> () }
    };
    (
        $(#[$attr:meta])* $vis:vis struct $name:ident $($tail:tt)*
    ) => {
        referential!{ @struct ($($tail)*) -> ($(#[$attr])*, $vis, $name) }
    };
}

#[cfg(test)]
mod tests {
    use super::Referencer;

    struct OwnedVec {
        vec: Vec<u8>,
    }

    impl OwnedVec {
        fn new(n: usize) -> OwnedVec {
            OwnedVec {
                vec: (0..n as u8).collect(),
            }
        }
    }

    struct OwnedConstGeneric<const N: usize> {
        array: [u8; N],
    }

    impl<const N: usize> Default for OwnedConstGeneric<N> {
        fn default() -> OwnedConstGeneric<N> {
            use core::convert::TryFrom;
            OwnedConstGeneric {
                array: <[u8; N]>::try_from((0..N as u8).collect::<Vec<_>>().as_ref()).unwrap(),
            }
        }
    }

    pub struct Refs<'a> {
        last_element: &'a u8,
    }

    impl<'a> Referencer<'a, OwnedVec> for Refs<'a> {
        fn from_data(data: &'a OwnedVec) -> Self {
            Self {
                last_element: &data.vec[data.vec.len() - 1],
            }
        }
    }

    impl<'a, const N: usize> Referencer<'a, OwnedConstGeneric<N>> for Refs<'a> {
        fn from_data(data: &'a OwnedConstGeneric<N>) -> Self {
            Self {
                last_element: &data.array[N - 1],
            }
        }
    }

    #[test]
    fn no_generics() {
        referential! {
            struct NoGenerics + 'a (Refs<'a>);
        }

        let referential = NoGenerics::new(Box::new(OwnedVec::new(5)));
        assert_eq!(*referential.referenced().last_element, 4);
        assert!(core::ptr::eq(
            referential.referenced().last_element,
            &referential.pinned().vec[referential.pinned().vec.len() - 1]
        ));
    }

    #[test]
    fn empty_generics() {
        referential! {
            struct EmptyGenerics< > + 'a (Refs<'a>);
        }

        let referential = EmptyGenerics::new(Box::new(OwnedVec::new(5)));
        assert_eq!(*referential.referenced().last_element, 4);
        assert!(core::ptr::eq(
            referential.referenced().last_element,
            &referential.pinned().vec[referential.pinned().vec.len() - 1]
        ));
    }

    #[test]
    fn new_with() {
        referential! {
            struct NoGenerics + 'a (Refs<'a>);
        }

        let data = Box::new(OwnedVec::new(5));
        let referential = NoGenerics::new_with(data, |d| Refs {
            last_element: &d.vec[1],
        });
        assert_eq!(*referential.referenced().last_element, 1);
        assert!(core::ptr::eq(
            referential.referenced().last_element,
            &referential.pinned().vec[1]
        ));
    }

    #[test]
    fn ref_has_outside_lifetimes() {
        struct DoubleRefs<'a, 'b> {
            last_elem_a: &'a u8,
            last_elem_b: &'b u8,
        }

        referential! {
            struct OutsideRef<'b> + 'o (DoubleRefs<'o, 'b>);
        }

        let stack_vec = vec![0, 1, 2];
        let data = Box::new(OwnedVec::new(5));
        let outside_ref = OutsideRef::new_with(data, |d| DoubleRefs {
            last_elem_a: &d.vec[0],
            last_elem_b: &stack_vec[0],
        });
        assert_eq!(*outside_ref.referenced().last_elem_a, 0);
        assert!(core::ptr::eq(
            outside_ref.referenced().last_elem_a,
            &outside_ref.pinned().vec[0]
        ));
        assert!(core::ptr::eq(
            outside_ref.referenced().last_elem_b,
            &stack_vec[0]
        ));
    }

    #[test]
    fn ref_without_lifetimes() {
        use std::rc::Rc;

        struct NoLifetimeRef {
            max: u8,
        }

        impl Referencer<'_, OwnedVec> for NoLifetimeRef {
            fn from_data(data: &OwnedVec) -> Self {
                NoLifetimeRef {
                    max: data.vec.iter().copied().max().unwrap_or(0),
                }
            }
        }

        referential! {
            struct ReferentialWithoutLifetime + 'o (NoLifetimeRef);
        }

        let data = Rc::new(OwnedVec::new(4));
        let referential = ReferentialWithoutLifetime::new(data.clone());
        assert_eq!(referential.referenced().max, 3);

        referential! {
            struct ReferentialEmptyLifetime + 'p (NoLifetimeRef<>);
        }
        let referential =
            ReferentialEmptyLifetime::new_with(data.clone(), |_| NoLifetimeRef { max: 4 });
        assert_eq!(referential.referenced().max, 4);
    }

    #[test]
    fn ref_to_ref() {
        struct U8Referencer<'a>(&'a u8);
        referential! {
            struct U8Ref + 'b (U8Referencer<'b>);
        }

        let val: u8 = 5;
        let referential = U8Ref::new_with(&val, |v| U8Referencer(v));
        assert!(std::ptr::eq(&val, referential.referenced().0));
        assert!(std::ptr::eq(
            referential.pinned(),
            referential.referenced().0
        ));
    }

    #[test]
    fn derive_clone() {
        referential! {
            #[derive(Clone)]
            struct ClonableReferential + 'b (Refs<'b>);
        }

        let referential = ClonableReferential::new(Box::new(OwnedVec::new(5)));
        assert_eq!(*referential.referenced().last_element, 4);
        assert!(core::ptr::eq(
            referential.referenced().last_element,
            &referential.pinned().vec[4]
        ));
    }

    #[test]
    fn rc_pinned() {
        use std::rc::Rc;
        referential! {
            struct Simple<> + 'a (Refs<'a>);
        }

        let data = OwnedVec::new(2);
        let simple = Simple::new(Rc::new(data));

        assert_eq!(*simple.referenced().last_element, 1);
    }
}
