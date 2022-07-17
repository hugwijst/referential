
pub struct Refs<'a> {
    last_element: &'a u8,
}

impl<'a> TypeTag<'a> for Refs<'a> {
    type Type = Refs<'a>;
}

// pub trait Provider {
//     fn provide<'a>(&'a self, req: &mut Requisition<'a>);
// }

// pub fn request_by_type_tag<'a, I: TypeTag<'a>>(provider: &'a dyn Provider) -> Option<I::Type> { 
//     todo!();
// }

// pub type Requisition<'a> = ...;

// impl<'a> Requisition<'a> {
//     pub fn provide_ref<T: ?Sized + 'static>(&mut self, value: &'a T) -> &mut Self { todo!() }
// }

pub trait TypeTag<'a>: Sized {
    type Type: 'a;
}
