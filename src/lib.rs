use std::ops::Deref;
use std::pin::Pin;

trait Referencer<'a, P: ?Sized> {
    fn from_data(data: &'a P) -> Self;
}

macro_rules! referential(
    {
        struct $name:ident + $own_lifetime:lifetime ($ref:ident);
    } => {
        referential!{ struct $name + $own_lifetime ($ref<>); }
    };
    {
        struct $name:ident + $own_lifetime:lifetime ($ref:ident<$($ref_lifetimes:lifetime),*>);
    } => {
        struct $name<P>(Pin<P>, $ref<'static>)
        where
            P: Deref,
            <P as Deref>::Target: Unpin;

        impl<P> $name<P>
        where
            P: Deref,
            <P as Deref>::Target: Unpin,
            for<'a> $ref<'a>: Referencer<'a, <P as Deref>::Target>,
        {
            #![allow(dead_code)]

            pub fn new(pinnable: P) -> Self {
                let pinned_data = Pin::new(pinnable);
                let references_local = $ref::from_data(pinned_data.as_ref().get_ref());
                let references_static = unsafe {
                    std::mem::transmute::<$ref<'_>, $ref<'static>>(references_local)
                };

                Self(pinned_data, references_static)
            }

            pub fn pinned<'a>(&'a self) -> &'a <P as Deref>::Target {
                self.0.as_ref().get_ref()
            }

            pub fn referenced<'a>(&'a self) -> &'a $ref<'a> {
                &self.1
            }
        }
    }
);

struct LargeDataOwned<const N: usize> {
    large_array: [u8; N],
    large_vec: Vec<u8>,
}

struct LargeDataRefs<'a> {
    last_array_element: &'a u8,
    last_vec_element: &'a u8,
}

impl<'a, const N: usize> Referencer<'a, LargeDataOwned<N>> for LargeDataRefs<'a> {
    fn from_data(data: &'a LargeDataOwned<N>) -> Self {
        Self {
            last_array_element: &data.large_array[data.large_array.len() - 1],
            last_vec_element: &data.large_vec[data.large_vec.len() - 1],
        }
    }
}

//// vvvv TO BE GENERATED vvvv ////
referential! {
    struct NoReference + 'a (LargeDataRefs);
}


referential! {
    struct LargeData2 + 'a (LargeDataRefs<'a>);
}

struct LargeData<P>(Pin<P>, LargeDataRefs<'static>)
where
    P: Deref,
    <P as Deref>::Target: Unpin;

impl<P> LargeData<P>
where
    P: Deref,
    <P as Deref>::Target: Unpin,
    for<'a> LargeDataRefs<'a>: Referencer<'a, <P as Deref>::Target>,
{
    fn new(pinnable: P) -> Self {
        let pinned_data = Pin::new(pinnable);
        let references_local = LargeDataRefs::from_data(pinned_data.as_ref().get_ref());
        let references_static = unsafe {
            std::mem::transmute::<LargeDataRefs<'_>, LargeDataRefs<'static>>(references_local)
        };

        Self(pinned_data, references_static)
    }

    fn pinned<'a>(&'a self) -> &'a <P as Deref>::Target {
        self.0.as_ref().get_ref()
    }

    fn referenced<'a>(&'a self) -> &'a LargeDataRefs<'a> {
        &self.1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::rc::Rc;

    #[test]
    fn it_works() {
        let data = LargeDataOwned {
            large_array: [1, 2, 3, 4, 5, 6, 7, 8],
            large_vec: (5..=10).collect(),
        };
        let large_data = LargeData::new(Box::new(data));

        assert_eq!(*large_data.referenced().last_array_element, 8);
        assert_eq!(*large_data.referenced().last_vec_element, 10);

        let data2 = LargeDataOwned {
            large_array: [1, 2],
            large_vec: vec![1, 2],
        };
        let large_data2 = LargeData2::new(Rc::new(data2));

        assert_eq!(*large_data2.referenced().last_array_element, 2);
        assert_eq!(*large_data2.referenced().last_vec_element, 2);
    }
}
