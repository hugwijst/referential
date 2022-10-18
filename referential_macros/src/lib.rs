extern crate proc_macro;

use std::cell::Cell;
use std::collections::HashSet;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse::Parse, punctuated::Punctuated, token::Comma, Error, GenericArgument};

struct Attributes {
    owned_lifetime: syn::Lifetime,
}

impl Parse for Attributes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Attributes {
            owned_lifetime: input.parse()?,
        })
    }
}

fn get_ty_generics(ty: &syn::Type) -> Vec<GenericArgument> {
    match ty {
        syn::Type::Array(ty) => get_ty_generics(&ty.elem),
        syn::Type::BareFn(_) => vec![],
        syn::Type::Group(ty) => get_ty_generics(&ty.elem),
        syn::Type::ImplTrait(_) => vec![],
        syn::Type::Infer(_) => vec![],
        syn::Type::Macro(_) => vec![],
        syn::Type::Never(_) => vec![],
        syn::Type::Paren(ty) => get_ty_generics(&ty.elem),
        syn::Type::Path(ty) => ty
            .path
            .segments
            .iter()
            .flat_map(|segment| match &segment.arguments {
                syn::PathArguments::None => vec![],
                syn::PathArguments::AngleBracketed(args) => args.args.iter().cloned().collect(),
                syn::PathArguments::Parenthesized(_) => todo!(),
            })
            .collect(),
        syn::Type::Ptr(ty) => get_ty_generics(&ty.elem),
        syn::Type::Reference(ty) => ty
            .lifetime
            .iter()
            .map(|lt| GenericArgument::Lifetime(lt.clone()))
            .chain(get_ty_generics(&ty.elem))
            .collect(),
        syn::Type::Slice(ty) => get_ty_generics(&ty.elem),
        syn::Type::TraitObject(_) => todo!(),
        syn::Type::Tuple(ty) => ty
            .elems
            .iter()
            .flat_map(|ty| get_ty_generics(ty).into_iter())
            .collect(),
        syn::Type::Verbatim(_) => vec![],
        _ => vec![],
    }
}

/// Check if any of the `params` define a type with identifier `ident`.
fn params_contain_ident<'a>(
    mut params: impl Iterator<Item = &'a syn::GenericParam>,
    ident: &str,
) -> bool {
    params.any(|p| match p {
        syn::GenericParam::Type(ty) => ty.ident == ident,
        syn::GenericParam::Lifetime(_) | syn::GenericParam::Const(_) => false,
    })
}

fn deduplicate_generic_arguments(args: &[syn::GenericArgument]) -> Vec<&GenericArgument> {
    let mut dedup_set = HashSet::new();
    args.iter().filter(|a| dedup_set.insert(*a)).collect()
}

fn create_ident_not_in_params<'a>(
    ident_base: &str,
    params: impl Iterator<Item = &'a syn::GenericParam> + Clone,
) -> syn::Ident {
    std::iter::once(None)
        .chain((1u32..).map(Some))
        .find_map(|ident_num| {
            let param_ident_name = format!(
                "{}{}",
                ident_base,
                ident_num.map_or(String::new(), |i| i.to_string())
            );

            if params_contain_ident(params.clone(), &param_ident_name) {
                None
            } else {
                Some(syn::Ident::new(&param_ident_name, Span::call_site()))
            }
        })
        .unwrap()
}

fn replace_lifetime_lt(lt: &mut syn::Lifetime, orig: &syn::Lifetime, new: &syn::Lifetime) {
    if lt == orig {
        *lt = new.clone();
    }
}

fn replace_lifetime_path(path: &mut syn::Path, orig: &syn::Lifetime, new: &syn::Lifetime) {
    for segment in path.segments.iter_mut() {
        match &mut segment.arguments {
            syn::PathArguments::None => (),
            syn::PathArguments::AngleBracketed(args) => {
                for arg in args.args.iter_mut() {
                    match arg {
                        syn::GenericArgument::Lifetime(lt) => replace_lifetime_lt(lt, orig, new),
                        syn::GenericArgument::Type(ty) => replace_lifetime_ty(ty, orig, new),
                        syn::GenericArgument::Binding(_) => todo!(),
                        syn::GenericArgument::Constraint(_) => todo!(),
                        syn::GenericArgument::Const(_) => todo!(),
                    }
                }
            }
            syn::PathArguments::Parenthesized(_) => todo!(),
        }
    }
}

fn replace_lifetime_ty(ty: &mut syn::Type, orig: &syn::Lifetime, new: &syn::Lifetime) {
    match ty {
        syn::Type::Array(ty) => replace_lifetime_ty(&mut ty.elem, orig, new),
        syn::Type::BareFn(_) => (),
        syn::Type::Group(ty) => replace_lifetime_ty(&mut ty.elem, orig, new),
        syn::Type::ImplTrait(_) => (),
        syn::Type::Infer(_) => (),
        syn::Type::Macro(_) => (),
        syn::Type::Never(_) => (),
        syn::Type::Paren(ty) => replace_lifetime_ty(&mut ty.elem, orig, new),
        syn::Type::Path(ty) => replace_lifetime_path(&mut ty.path, orig, new),
        syn::Type::Ptr(ty) => replace_lifetime_ty(&mut ty.elem, orig, new),
        syn::Type::Reference(ty) => {
            if let Some(lt) = ty.lifetime.as_mut().filter(|lt| lt == &orig) {
                *lt = new.clone()
            };
            replace_lifetime_ty(&mut ty.elem, orig, new)
        }
        syn::Type::Slice(ty) => replace_lifetime_ty(&mut ty.elem, orig, new),
        syn::Type::TraitObject(_) => todo!(),
        syn::Type::Tuple(ty) => ty
            .elems
            .iter_mut()
            .for_each(|ty| replace_lifetime_ty(ty, orig, new)),
        syn::Type::Verbatim(_) => (),
        _ => (),
    }
}

fn replace_lifetime_generic_param(
    param: &mut syn::GenericParam,
    orig: &syn::Lifetime,
    new: &syn::Lifetime,
) {
    match param {
        syn::GenericParam::Type(ty) => {
            for bound in ty.bounds.iter_mut() {
                match bound {
                    syn::TypeParamBound::Trait(bound) => {
                        replace_lifetime_path(&mut bound.path, orig, new);
                    }
                    syn::TypeParamBound::Lifetime(lt) => replace_lifetime_lt(lt, orig, new),
                }
            }
            if let Some(def_ty) = ty.default.as_mut() {
                replace_lifetime_ty(def_ty, orig, new);
            }
        }
        syn::GenericParam::Lifetime(lt) => {
            replace_lifetime_lt(&mut lt.lifetime, orig, new);
            for bound_lt in lt.bounds.iter_mut() {
                replace_lifetime_lt(bound_lt, orig, new);
            }
        }
        syn::GenericParam::Const(cons) => replace_lifetime_ty(&mut cons.ty, orig, new),
    }
}

/// Transform the input struct into a "referential" struct.
///
/// Given an input of the form:
/// ```ignore
/// #[referential('a)]
/// struct Referential<'b, T>(Referencing<'a, 'b, T>);
/// ```
/// this function will generate
/// ```ignore
/// impl<'a, 'b, T> __referential_inner_mod_0::__NameType<'a, 'b, T> for () {
///     type Type = Referencing<'a, 'b, T>;
/// }
/// mod __referential_inner_mod_0 {
///     pub(super) trait __NameType<'a, 'b, T> {
///         type Type;
///     }
///     pub(super) struct Inner<'b, T, O> {
///         referencing: <() as __NameType<'static, 'b, T>>::Type,
///         owned: ::core::pin::Pin<O>,
///     }
///     impl<'b, T, O> Inner<'b, T, O>
///     where
///         O: ::core::ops::Deref,
///         <O as ::core::ops::Deref>::Target: ::core::marker::Unpin,
///     {
///         #![allow(dead_code)]
///         pub fn new_with<F>(owned: O, f: F) -> Self
///         where
///             F: for<'a> FnOnce(
///                 &'a <O as ::core::ops::Deref>::Target,
///             ) -> <() as __NameType<'a, 'b>>::Type,
///         {
///             use ::core::ops::Deref;
///             let owned = ::core::pin::Pin::new(owned);
///             let referencing_local = (f)(owned.deref());
///             let referencing_static = unsafe { ::core::mem::transmute(referencing_local) };
///             Self {
///                 referencing: referencing_static,
///                 owned,
///             }
///         }
///         pub fn owning<'a>(&'a self) -> &'a <O as ::core::ops::Deref>::Target {
///             use ::core::ops::Deref;
///             self.owned.deref()
///         }
///         pub fn into_owning(self) -> O {
///             ::core::pin::Pin::into_inner(self.owned)
///         }
///         pub fn referencing<'a>(&'a self) -> &'a <() as __NameType<'a, 'b, T>>::Type {
///             &self.referencing
///         }
///     }
/// }
/// pub struct Referential<'b, T, O> {
///     inner: __referential_inner_mod_0::Inner<'b, T, O>,
/// }
/// impl<'b, T, O> Referential<'b, T, O>
/// where
///     O: ::core::ops::Deref,
///     <O as ::core::ops::Deref>::Target: ::core::marker::Unpin,
/// {
///     #![allow(dead_code)]
///     pub fn new_with<F>(owned: O, f: F) -> Self
///     where
///         F: for<'a> FnOnce(&'a <O as ::core::ops::Deref>::Target) -> Referencing<'a, 'b, T>,
///     {
///         Self { inner: __referential_inner_mod_0::Inner::new_with(owned, f) }
///     }
///     pub fn owning<'a>(&'a self) -> &'a <O as ::core::ops::Deref>::Target {
///         self.inner.owning()
///     }
///     pub fn into_owning(self) -> O {
///         self.inner.into_owning()
///     }
///     pub fn referencing<'a>(&'a self) -> &'a <() as __NameType<'a, 'b, T>>::Type {
///         &self.inner.referencing()
///     }
/// impl<'b, T, P> Referential<'b, T, O>
/// where
///     O: ::core::ops::Deref,
///     <O as ::core::ops::Deref>::Target: ::core::marker::Unpin,
///     for<'a> Referencing<'a, 'b>: ::referential::FromOwned<'a, O::Target>,
/// {
///     #![allow(dead_code)]
///     pub fn new(owning: O) -> Self {
///         use ::referential::FromOwned;
///         Self::new_with(owning, |o_ref| <DoubleRefs<'_, 'b, T>>::from_owned(o_ref))
///     }
/// }
/// ```
fn referential_impl(attr: Attributes, item_struct: syn::ItemStruct) -> syn::Result<TokenStream> {
    let static_lifetime = syn::Lifetime::new("'static", Span::call_site());
    let elided_lifetime = syn::Lifetime::new("'_", Span::call_site());

    // The lifetime of the owned data.
    //
    // Example: `'a`
    let attr_owned_lifetime = &attr.owned_lifetime;

    // General properties of the struct this attributes is applied to.
    let ref_struct_attributes = &item_struct.attrs;
    let ref_struct_ident = &item_struct.ident;
    let ref_struct_vis = &item_struct.vis;
    let ref_struct_generics = &item_struct.generics;
    let ref_struct_where_clause = &ref_struct_generics.where_clause;
    let ref_struct_params = &ref_struct_generics.params;

    // Referential struct generic parameters, with the owned lifetime substituted by the `'static`
    // and with a traiting comma.
    //
    // For example, with input "#[referential('a)] Referential<T: 'a>(...)" this would be
    // `T: 'static,`.
    let mut ref_struct_params_lt_static = ref_struct_params.clone();
    for param in ref_struct_params_lt_static.iter_mut() {
        replace_lifetime_generic_param(param, attr_owned_lifetime, &static_lifetime);
    }
    if !ref_struct_params_lt_static.empty_or_trailing() {
        ref_struct_params_lt_static.push_punct(Default::default());
    }

    // Generic arguments derived from the referential structs parameters, with a trailing comma.
    //
    // Example: `'b, T,`
    let ref_struct_args = ref_struct_params
        .iter()
        .map(|param| match param {
            syn::GenericParam::Type(ty) => syn::GenericArgument::Type(
                syn::TypePath {
                    qself: None,
                    path: ty.ident.clone().into(),
                }
                .into(),
            ),
            syn::GenericParam::Lifetime(lt) => syn::GenericArgument::Lifetime(lt.lifetime.clone()),
            syn::GenericParam::Const(ct) => syn::GenericArgument::Const(
                syn::ExprPath {
                    attrs: Vec::new(),
                    qself: None,
                    path: ct.ident.clone().into(),
                }
                .into(),
            ),
        })
        .map(|param| syn::punctuated::Pair::Punctuated(param, Comma::default()))
        .collect::<Punctuated<syn::GenericArgument, _>>();

    // Type identifier used to represent the owning data type.
    //
    // Example: `O`
    let owned_type_param = create_ident_not_in_params("O", ref_struct_params.iter());
    // Type identifier used to represent the constructor function in `new_with`.
    //
    // Example: `F`
    let function_param = create_ident_not_in_params("F", ref_struct_params.iter());

    // Extract single field of item struct.
    //
    // Example: `Referencing<'a, 'b, T>`
    let field = match &item_struct.fields {
        syn::Fields::Named(_) => Err(Error::new_spanned(
            item_struct.fields,
            "expected single unnamed field, found named fields",
        )),
        syn::Fields::Unnamed(fs) => {
            if fs.unnamed.len() != 1 {
                Err(Error::new_spanned(
                    fs,
                    "expected single unnamed field, found multiple",
                ))
            } else {
                Ok(fs.unnamed.first().unwrap())
            }
        }
        syn::Fields::Unit => Err(Error::new_spanned(
            item_struct.fields,
            "expected single unnamed field, found unit struct",
        )),
    }?;
    let field_ty = &field.ty;

    // All type generics in the field, as generic arguments.
    //
    // Example: `'a, 'b, T`.
    let ty_generic_args = get_ty_generics(field_ty);
    let ty_generic_args = deduplicate_generic_arguments(&ty_generic_args);

    let static_lifetime_argument = GenericArgument::Lifetime(static_lifetime);
    let owned_lifetime_argument = GenericArgument::Lifetime(attr.owned_lifetime.clone());
    // Type generic arguments, with owned lifetime replaced by `'static`.
    //
    // Example: `'static, 'b, T`.
    let ty_generic_args_static = ty_generic_args.iter().map(|&argument| {
        if argument == &owned_lifetime_argument {
            &static_lifetime_argument
        } else {
            argument
        }
    });

    // Field type, with owned lifetime elided.
    //
    // Example: `Referencing<'_, 'b, T>`.
    let mut field_ty_lt_elided = field_ty.clone();
    replace_lifetime_ty(
        &mut field_ty_lt_elided,
        attr_owned_lifetime,
        &elided_lifetime,
    );

    thread_local!(static REFERENTIAL_COUNT: Cell<usize> = Cell::new(0));
    // Identifier of the "private" module holding the referential structs internals.
    //
    // The inner structure is hidden as the lifetimes of the stored referencing struct are
    // incorrect. Only through the methods do we get correct lifetimes.
    //
    // By storing the internal `Inner` struct in the module, we still allow users to implement
    // traits on the struct they define.
    let mod_ident = REFERENTIAL_COUNT.with(|count_cell| {
        let count = count_cell.get();
        count_cell.set(count + 1);
        syn::Ident::new(
            &format!("__referential_inner_mod_{}", count),
            Span::call_site(),
        )
    });

    let out = quote! {
        impl <#attr_owned_lifetime, #ref_struct_params> #mod_ident::__NameType <#(#ty_generic_args, )*> for ()
            #ref_struct_where_clause
        {
            type Type = #field_ty;
        }
        mod #mod_ident {
            pub(super) trait __NameType <#(#ty_generic_args, )*> {
                type Type;
            }

            #(#ref_struct_attributes)*
            pub(super) struct Inner<#ref_struct_params_lt_static #owned_type_param>
                #ref_struct_where_clause
            {
                // The order of elements here is critical to ensure safety: the
                // drop order is the same as declared here. First dropping the referenced
                // data is obviously unsafe, dropping the references first should be safe.
                referencing: <() as __NameType<#(#ty_generic_args_static, )*>>::Type,
                owned: ::core::pin::Pin<#owned_type_param>,
            }

            impl<#ref_struct_params_lt_static #owned_type_param> Inner<#ref_struct_args #owned_type_param>
            where
                #owned_type_param: ::core::ops::Deref,
                <#owned_type_param as ::core::ops::Deref>::Target: ::core::marker::Unpin,
            {
                #![allow(dead_code)]

                pub(super) fn new_with<#function_param>(owned: #owned_type_param, f: #function_param) -> Self
                where
                    #function_param: for<#attr_owned_lifetime> FnOnce(
                        &#attr_owned_lifetime <#owned_type_param as ::core::ops::Deref>::Target
                    ) -> <() as __NameType<#(#ty_generic_args, )*>>::Type,
                {
                    use ::core::ops::Deref;
                    let owned = ::core::pin::Pin::new(owned);
                    let referencing_local = (f)(owned.deref());
                    let referencing_static = unsafe {
                        ::core::mem::transmute(referencing_local)
                    };

                    Self { referencing: referencing_static, owned }
                }

                pub(super) fn owning<#attr_owned_lifetime>(&#attr_owned_lifetime self) -> &#attr_owned_lifetime <#owned_type_param as ::core::ops::Deref>::Target {
                    use ::core::ops::Deref;
                    self.owned.deref()
                }

                pub(super) fn into_owning(self) -> #owned_type_param {
                    ::core::pin::Pin::into_inner(self.owned)
                }

                pub(super) fn referencing<#attr_owned_lifetime>(
                    &#attr_owned_lifetime self
                ) -> &#attr_owned_lifetime <() as __NameType<#(#ty_generic_args, )*>>::Type {
                    &self.referencing
                }
            }
        }
        #(#ref_struct_attributes)*
        #ref_struct_vis struct #ref_struct_ident<#ref_struct_params_lt_static #owned_type_param>
            #ref_struct_where_clause
        {
            inner: #mod_ident::Inner<#ref_struct_args #owned_type_param>,
        }

        impl<#ref_struct_params_lt_static #owned_type_param> #ref_struct_ident<#ref_struct_args #owned_type_param>
        where
            #owned_type_param: ::core::ops::Deref,
            <#owned_type_param as ::core::ops::Deref>::Target: ::core::marker::Unpin,
        {
            #![allow(dead_code)]

            pub fn new_with<#function_param>(owned: #owned_type_param, f: #function_param) -> Self
            where
                #function_param: for<#attr_owned_lifetime> FnOnce(&#attr_owned_lifetime <#owned_type_param as ::core::ops::Deref>::Target) -> #field_ty
            {
                Self { inner: #mod_ident::Inner::new_with(owned, f) }
            }

            pub fn owning<#attr_owned_lifetime>(&#attr_owned_lifetime self) -> &#attr_owned_lifetime <#owned_type_param as ::core::ops::Deref>::Target {
                self.inner.owning()
            }

            pub fn into_owning(self) -> #owned_type_param {
                self.inner.into_owning()
            }

            pub fn referencing<#attr_owned_lifetime>(&#attr_owned_lifetime self) -> &#attr_owned_lifetime #field_ty {
                self.inner.referencing()
            }
        }

        impl<#ref_struct_params_lt_static #owned_type_param> #ref_struct_ident<#ref_struct_args #owned_type_param>
        where
            #owned_type_param: ::core::ops::Deref,
            <#owned_type_param as ::core::ops::Deref>::Target: ::core::marker::Unpin,
            for<#attr_owned_lifetime> #field_ty: ::referential::FromOwned<#attr_owned_lifetime, #owned_type_param::Target>,
        {
            #![allow(dead_code)]

            pub fn new(owning: #owned_type_param) -> Self {
                use ::referential::FromOwned;
                Self::new_with(owning, |o_ref| <#field_ty_lt_elided>::from_owned(o_ref))
            }
        }

    };

    Ok(out)
}

#[proc_macro_attribute]
pub fn referential(
    attr: proc_macro::TokenStream,
    item_stream: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr = syn::parse_macro_input!(attr as Attributes);
    let item = syn::parse_macro_input!(item_stream as syn::ItemStruct);

    referential_impl(attr, item).unwrap().into()
}
