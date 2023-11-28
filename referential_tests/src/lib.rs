#[cfg(test)]
mod tests {
    use referential::{referential, FromOwned};

    #[derive(Debug, Clone, PartialEq, Eq)]
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

    #[derive(Clone)]
    pub struct Refs<'a> {
        last_element: &'a u8,
    }

    impl<'a> FromOwned<'a, OwnedVec> for Refs<'a> {
        fn from_owned(data: &'a OwnedVec) -> Self {
            Self {
                last_element: &data.vec[data.vec.len() - 1],
            }
        }
    }

    #[test]
    fn no_generics() {
        #[referential('a)]
        struct NoGenerics(Refs<'a>);

        let referential = NoGenerics::new(Box::new(OwnedVec::new(5)));
        assert_eq!(*referential.referencing().last_element, 4);
        assert!(core::ptr::eq(
            referential.referencing().last_element,
            &referential.owning().vec[referential.owning().vec.len() - 1]
        ));
    }

    #[test]
    fn empty_generics() {
        #[referential('a)]
        struct EmptyGenerics(Refs<'a>);

        let referential = EmptyGenerics::new(Box::new(OwnedVec::new(5)));
        assert_eq!(*referential.referencing().last_element, 4);
        assert!(core::ptr::eq(
            referential.referencing().last_element,
            &referential.owning().vec[referential.owning().vec.len() - 1]
        ));
    }

    #[test]
    fn new_with() {
        #[referential('a)]
        struct NoGenerics(Refs<'a>);

        let data = Box::new(OwnedVec::new(5));
        let referential = NoGenerics::new_with(data, |d| Refs {
            last_element: &d.vec[1],
        });
        assert_eq!(*referential.referencing().last_element, 1);
        assert!(core::ptr::eq(
            referential.referencing().last_element,
            &referential.owning().vec[1]
        ));
    }

    #[test]
    fn ref_has_outside_lifetimes() {
        struct DoubleRefs<'a, 'b> {
            last_elem_a: &'a u8,
            last_elem_b: &'b u8,
        }

        #[referential('o)]
        struct OutsideRef<'b>(DoubleRefs<'o, 'b>);

        let stack_vec = vec![0, 1, 2];
        let data = Box::new(OwnedVec::new(5));
        let outside_ref = OutsideRef::new_with(data, |d| DoubleRefs {
            last_elem_a: &d.vec[0],
            last_elem_b: &stack_vec[0],
        });
        assert_eq!(*outside_ref.referencing().last_elem_a, 0);
        assert!(core::ptr::eq(
            outside_ref.referencing().last_elem_a,
            &outside_ref.owning().vec[0]
        ));
        assert!(core::ptr::eq(
            outside_ref.referencing().last_elem_b,
            &stack_vec[0]
        ));
    }

    #[test]
    fn ref_without_lifetimes() {
        use std::rc::Rc;

        struct NoLifetimeRef {
            max: u8,
        }

        impl FromOwned<'_, OwnedVec> for NoLifetimeRef {
            fn from_owned(data: &OwnedVec) -> Self {
                NoLifetimeRef {
                    max: data.vec.iter().copied().max().unwrap_or(0),
                }
            }
        }

        #[referential('o)]
        struct ReferentialWithoutLifetime(NoLifetimeRef);

        let data = Rc::new(OwnedVec::new(4));
        let referential = ReferentialWithoutLifetime::new(data.clone());
        assert_eq!(referential.referencing().max, 3);

        #[referential('p)]
        struct ReferentialEmptyLifetime(NoLifetimeRef);
        let referential =
            ReferentialEmptyLifetime::new_with(data.clone(), |_| NoLifetimeRef { max: 4 });
        assert_eq!(referential.referencing().max, 4);
    }

    #[test]
    fn ref_to_ref() {
        struct U8Referencer<'a>(&'a u8);
        #[referential('b)]
        struct U8Ref(U8Referencer<'b>);

        let val: u8 = 5;
        let referential = U8Ref::new_with(&val, |v| U8Referencer(v));
        assert!(std::ptr::eq(&val, referential.referencing().0));
        assert!(std::ptr::eq(
            referential.owning(),
            referential.referencing().0
        ));
    }

    #[test]
    fn derive_clone() {
        #[referential('b)]
        #[derive(Clone)]
        struct ClonableReferential(Refs<'b>);

        let referential = ClonableReferential::new(Box::new(OwnedVec::new(5)));
        assert_eq!(*referential.referencing().last_element, 4);
        assert!(core::ptr::eq(
            referential.referencing().last_element,
            &referential.owning().vec[4]
        ));

        assert!(core::ptr::eq(
            referential.clone().referencing().last_element,
            &referential.owning().vec[4]
        ));
    }

    #[test]
    fn rc_pinned() {
        use std::rc::Rc;
        #[referential('a)]
        struct Simple(Refs<'a>);

        let data = OwnedVec::new(2);
        let simple = Simple::new(Rc::new(data));

        assert_eq!(*simple.referencing().last_element, 1);
    }

    #[test]
    fn into_owned() {
        #[referential('a)]
        struct Simple(Refs<'a>);

        let data = OwnedVec::new(2);
        let simple = Simple::new(Box::new(data.clone()));

        assert_eq!(*simple.referencing().last_element, 1);

        // Accessing `element` after `simple.into_owned` is not possible as
        // `Simple::into_owned` tries to move out of `simple`.
        let _element = simple.referencing().last_element;

        let pinned_data = simple.into_owning();
        assert_eq!(*pinned_data, data);
    }

    #[test]
    fn generics() {
        pub struct GenericRefs<'a, T> {
            last_element: &'a T,
        }

        #[referential('a)]
        struct Generic<T: 'a>(GenericRefs<'a, T>);

        let data = Box::new(vec!['a', 'b', 'e']);
        let generics = Generic::new_with(data, |d| GenericRefs {
            last_element: &d[d.len() - 1],
        });

        assert_eq!(*generics.referencing().last_element, 'e');
        assert_eq!(generics.referencing().last_element, &generics.owning()[2]);
    }

    #[test]
    fn duplicate_internal_types() {
        #[referential('a)]
        struct FirstAndLast((&'a char, &'a char));

        let data = Box::new(vec!['a', 'b', 'e']);
        let generics = FirstAndLast::new_with(data, |d| (&d[0], &d[d.len() - 1]));

        assert_eq!(*generics.referencing().1, 'e');
        assert_eq!(generics.referencing().1, &generics.owning()[2]);
    }

    /// Test the aliasing issue that hit `OwningRef`: https://github.com/Kimundi/owning-ref-rs/issues/49.
    #[test]
    fn aliasing_test() {
        use std::cell::Cell;
        struct CellRef<'a>(&'a Cell<u8>);
        #[referential('a)]
        struct Aliasing(CellRef<'a>);

        let referential = Aliasing::new_with(Box::new(Cell::new(42)), |b| CellRef(b));
        assert_eq!(referential.referencing().0.get(), 42);
        referential.owning().set(10);
        referential.referencing().0.set(20);
        assert_eq!(referential.owning().get(), 20);
    }

    #[test]
    fn failing() {
        let t = trybuild::TestCases::new();
        t.compile_fail("src/failing/*.rs");
    }
}

#[cfg(doctest)]
mod test_readme {
    macro_rules! external_doc_test {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }

    external_doc_test!(include_str!("../../README.md"));
}
