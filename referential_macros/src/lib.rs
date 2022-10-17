extern crate proc_macro;

use std::cell::Cell;

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
                syn::PathArguments::Parenthesized(_) => panic!("todo"),
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
        syn::Type::TraitObject(_) => panic!("todo"),
        syn::Type::Tuple(ty) => ty
            .elems
            .iter()
            .flat_map(|ty| get_ty_generics(ty).into_iter())
            .collect(),
        syn::Type::Verbatim(_) => vec![],
        _ => vec![],
    }
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
                        syn::GenericArgument::Binding(_) => panic!("todo"),
                        syn::GenericArgument::Constraint(_) => panic!("todo"),
                        syn::GenericArgument::Const(_) => panic!("todo"),
                    }
                }
            }
            syn::PathArguments::Parenthesized(_) => panic!("todo"),
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
        syn::Type::TraitObject(_) => panic!("todo"),
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
///     pub(super) struct Inner<'b, T, P> {
///         referencing: <() as __NameType<'static, 'b, T>>::Type,
///         owned: P,
///     }
///     impl<'b, T, P> Inner<'b, T, P>
///     where
///         P: ::referential::StableDeref,
///     {
///         #![allow(dead_code)]
///         pub fn new_with<F>(owned: P, f: F) -> Self
///         where
///             F: for<'a> FnOnce(
///                 &'a <P as ::core::ops::Deref>::Target,
///             ) -> <() as __NameType<'a, 'b>>::Type,
///         {
///             let referencing_local = (f)(owned.deref());
///             let referencing_static = unsafe { ::core::mem::transmute(referencing_local) };
///             Self {
///                 referencing: referencing_static,
///                 owned,
///             }
///         }
///         pub fn owning<'a>(&'a self) -> &'a <P as ::core::ops::Deref>::Target {
///             self.owned.deref()
///         }
///         pub fn into_owning(self) -> P {
///             self.owned
///         }
///         pub fn referencing<'a>(&'a self) -> &'a <() as __NameType<'a, 'b, T>>::Type {
///             &self.referencing
///         }
///     }
/// }
/// pub struct Referential<'b, T, P> {
///     inner: __referential_inner_mod_0::Inner<'b, T, P>,
/// }
/// impl<'b, T, P> Referential<'b, T, P>
/// where
///     P: ::referential::StableDeref
/// {
///     #![allow(dead_code)]
///     pub fn new_with<F>(owned: P, f: F) -> Self
///     where
///         F: for<'a> FnOnce(&'a <P as ::core::ops::Deref>::Target) -> Referencing<'a, 'b, T>,
///     {
///         Self { inner: __referential_inner_mod_0::Inner::new_with(owned, f) }
///     }
///     pub fn owning<'a>(&'a self) -> &'a <P as ::core::ops::Deref>::Target {
///         self.inner.owning()
///     }
///     pub fn into_owning(self) -> P {
///         self.inner.into_owning()
///     }
///     pub fn referencing<'a>(&'a self) -> &'a <() as __NameType<'a, 'b, T>>::Type {
///         &self.inner.referencing()
///     }
/// impl<'b, T, P> Referential<'b, T, P>
/// where
///     P: ::referential::StableDeref,
///     for<'a> Referencing<'a, 'b>: ::referential::FromOwned<'a, P::Target>,
/// {
///     #![allow(dead_code)]
///     pub fn new(owning: P) -> Self {
///         use ::referential::FromOwned;
///         Self::new_with(owning, |p_ref| <DoubleRefs<'_, 'b, T>>::from_owned(p_ref))
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

    let static_lifetime_argument = GenericArgument::Lifetime(static_lifetime);
    let owned_lifetime_argument = GenericArgument::Lifetime(attr.owned_lifetime.clone());
    // Type generic arguments, with owned lifetime replaced by `'static`.
    //
    // Example: `'static, 'b, T`.
    let ty_generic_args_static = ty_generic_args.iter().map(|argument| {
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
            pub(super) struct Inner<#ref_struct_params_lt_static P>
                #ref_struct_where_clause
            {
                // The order of elements here is critical to ensure safety: the
                // drop order is the same as declared here. First dropping the referenced
                // data is obviously unsafe, dropping the references first should be safe.
                referencing: <() as __NameType<#(#ty_generic_args_static, )*>>::Type,
                owned: P,
            }

            impl<#ref_struct_params_lt_static P> Inner<#ref_struct_args P>
            where
                P: ::referential::StableDeref,
            {
                #![allow(dead_code)]

                pub(super) fn new_with<F>(owned: P, f: F) -> Self
                where
                    F: for<#attr_owned_lifetime> FnOnce(
                        &#attr_owned_lifetime <P as ::core::ops::Deref>::Target
                    ) -> <() as __NameType<#(#ty_generic_args, )*>>::Type,
                {
                    let referencing_local = (f)(owned.deref());
                    let referencing_static = unsafe {
                        ::core::mem::transmute(referencing_local)
                    };

                    Self { referencing: referencing_static, owned }
                }

                pub fn owning<#attr_owned_lifetime>(&#attr_owned_lifetime self) -> &#attr_owned_lifetime <P as ::core::ops::Deref>::Target {
                    self.owned.deref()
                }

                pub fn into_owning(self) -> P {
                    self.owned
                }

                pub(super) fn referencing<#attr_owned_lifetime>(
                    &#attr_owned_lifetime self
                ) -> &#attr_owned_lifetime <() as __NameType<#(#ty_generic_args, )*>>::Type {
                    &self.referencing
                }
            }
        }
        #(#ref_struct_attributes)*
        #ref_struct_vis struct #ref_struct_ident<#ref_struct_params_lt_static P>
            #ref_struct_where_clause
        {
            inner: #mod_ident::Inner<#ref_struct_args P>,
        }

        impl<#ref_struct_params_lt_static P> #ref_struct_ident<#ref_struct_args P>
        where
            P: ::referential::StableDeref,
        {
            #![allow(dead_code)]

            pub fn new_with<F>(owned: P, f: F) -> Self
            where
                F: for<#attr_owned_lifetime> FnOnce(&#attr_owned_lifetime <P as ::core::ops::Deref>::Target) -> #field_ty
            {
                Self { inner: #mod_ident::Inner::new_with(owned, f) }
            }

            pub fn owning<#attr_owned_lifetime>(&#attr_owned_lifetime self) -> &#attr_owned_lifetime <P as ::core::ops::Deref>::Target {
                self.inner.owning()
            }

            pub fn into_owning(self) -> P {
                self.inner.into_owning()
            }

            pub fn referencing<#attr_owned_lifetime>(&#attr_owned_lifetime self) -> &#attr_owned_lifetime #field_ty {
                self.inner.referencing()
            }
        }

        impl<#ref_struct_params_lt_static P> #ref_struct_ident<#ref_struct_args P>
        where
            P: ::referential::StableDeref,
            for<#attr_owned_lifetime> #field_ty: ::referential::FromOwned<#attr_owned_lifetime, P::Target>,
        {
            #![allow(dead_code)]

            pub fn new(owning: P) -> Self {
                use ::referential::FromOwned;
                Self::new_with(owning, |p_ref| <#field_ty_lt_elided>::from_owned(p_ref))
            }
        }

    };
    // eprintln!("{}", out);
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
