use std::pin::Pin;
use std::ops::Deref;

struct LargeData {
    large_array: [u8; 8],
    large_vec: Vec<u8>,
}

struct References<'a> {
    last_array_element: &'a u8,
    last_vec_element: &'a u8,
}


struct OwningHandle<P, R>(Pin<P>, R)
where
    P: Deref,
    <P as Deref>::Target: Unpin,
    for<'r> R: 'r;

impl OwningHandle2<'_, Box<LargeData>, References<'static>> for MyData
{
    fn pinned<'a>(&'a self) -> &'a LargeData {
        self.0.as_ref().get_ref()
    }
    
    fn references<'a>(&'a self) -> &'a References<'a> {
        &self.1
    }
}

trait OwningHandle2<'r, P, R: 'r>
where
    P: Deref,
    <P as Deref>::Target: Unpin,
{
    fn pinned<'a>(&'a self) -> &'a <P as Deref>::Target;
    
    fn references<'a>(&'a self) -> &'a R
    where R: 'a;
}


struct MyData(Pin<Box<LargeData>>, References<'static>);

fn create_ref<'a>(large_data: &'a LargeData) -> References<'a> {
    References {
        last_array_element: &large_data.large_array[large_data.large_array.len() - 1],
        last_vec_element: &large_data.large_vec[large_data.large_vec.len() - 1],
    }
}

impl MyData {
    fn new(large_data: Box<LargeData>) -> Self {
        let pinned_data = Pin::new(large_data);
        let references_local = create_ref(pinned_data.as_ref().get_ref());
        let references_static = unsafe { std::mem::transmute::<References<'_>, References<'static>>(references_local) };
        
        MyData(pinned_data, references_static as References<'static>)
    }

    fn pinned<'a>(&'a self) -> &'a LargeData {
        self.0.as_ref().get_ref()
    }
    
    fn references<'a>(&'a self) -> &'a References<'a> {
        &self.1
    }
}



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
