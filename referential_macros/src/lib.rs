extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::ToTokens;


fn referential_impl(attr: TokenStream, item: syn::Item) -> syn::Item {
    item
}


#[proc_macro_attribute]
pub fn referential(attr: proc_macro::TokenStream, item_stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let attr_input = TokenStream::from(attr);
    let item = syn::parse_macro_input!(item_stream as syn::Item);

    let output = referential_impl(attr_input, item);

    output.into_token_stream().into()
}