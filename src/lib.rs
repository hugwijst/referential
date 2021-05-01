#![cfg_attr(not(test), no_std)]

// #![feature(trace_macros)]

trait Referencer<'a, P: ?Sized> {
    fn from_data(data: &'a P) -> Self;
}

#[macro_export]
macro_rules! referential {
    (
        struct $name:ident + $own_lifetime:lifetime ($ref:ident);
    ) => {
        referential!{ struct $name< > + $own_lifetime ($ref<>); }
    };
    (

        $vis:vis struct $name:ident<$($own_gen_lifetime:lifetime $(: $own_first_lifetime_bound:lifetime $(+ $own_other_lifetime_bound:lifetime)*)?),*> + $own_lifetime:lifetime ($ref:ident<$($ref_lifetimes:lifetime),*>);
    ) => {
        // The order of elements here is critical to ensure safety: the
        // drop order is the same as declared here. First dropping the referenced
        // data is obviously unsafe, dropping the references first should be safe.
        $vis struct $name<$($own_gen_lifetime $(: $own_first_lifetime_bound $(+ $own_other_lifetime_bound)*)?),* P>($ref<'static>, ::std::pin::Pin<P>)
        where
            P: ::core::ops::Deref,
            <P as ::core::ops::Deref>::Target: Unpin;

        impl<P> $name<P>
        where
            P: ::core::ops::Deref,
            <P as ::core::ops::Deref>::Target: Unpin,
            for<'a> $ref<'a>: $crate::Referencer<'a, <P as ::core::ops::Deref>::Target>,
        {
            #![allow(dead_code)]

            pub fn new(pinnable: P) -> Self {
                let pinned_data = ::core::pin::Pin::new(pinnable);
                let references_local = $ref::from_data(pinned_data.as_ref().get_ref());
                let references_static = unsafe {
                    ::core::mem::transmute::<$ref<'_>, $ref<'static>>(references_local)
                };

                Self(references_static, pinned_data)
            }

            pub fn pinned<'a>(&'a self) -> &'a <P as ::core::ops::Deref>::Target {
                self.1.as_ref().get_ref()
            }

            pub fn referenced<'a>(&'a self) -> &'a $ref<'a> {
                &self.0
            }
        }
    };
}

// trace_macros!(true);

// referential! {
//     struct WithUnusedOwnLifetime<'a> + 'b (LargeDataRefs<'b>);
// }

#[cfg(test)]
mod tests {
    use super::*;

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
            struct NoGenerics + 'a (Refs);
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
