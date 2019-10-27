//! Modifies the lifetimes in the setter methods to minimize the chance of lifetime weirdness.

use proc_macro2::{TokenStream as SynTokenStream};
use std::collections::HashSet;
use std::result::Result;
use syn::*;
use syn::spanned::Spanned;
use quote::*;

use super::error;

#[derive(Copy, Clone, Debug)]
enum RelaxType {
    Input,
    Output,
}
fn relaxed_lifetime_ident(t: RelaxType, i: &Ident) -> Ident {
    let t = match t {
        RelaxType::Input => "in",
        RelaxType::Output => "out",
    };
    Ident::new(&format!("{}_{}", t, i), i.span())
}
fn relax_lifetime_instance(
    t: RelaxType, lt: &mut Lifetime, generics: &mut HashSet<Ident>, bound: &HashSet<Ident>,
) {
    let unbound = !bound.contains(&lt.ident);
    let do_bound = match t {
        RelaxType::Input => true,
        RelaxType::Output => generics.contains(&lt.ident),
    };
    if unbound && do_bound {
        if let RelaxType::Input = t {
            generics.insert(lt.ident.clone());
        }
        let ident = relaxed_lifetime_ident(t, &lt.ident);
        lt.ident = ident;
    }
}
fn bind_lifetimes(
    t: RelaxType, lts: &mut Option<BoundLifetimes>,
    generics: &mut HashSet<Ident>, bound: &HashSet<Ident>,
) -> HashSet<Ident> {
    let mut bound = bound.clone();
    if let Some(lts) = lts {
        bound.extend(lts.lifetimes.iter().map(|x| x.lifetime.ident.clone()));
        for bound_lifetime in &mut lts.lifetimes {
            for lt in &mut bound_lifetime.bounds {
                relax_lifetime_instance(t, lt, generics, &bound);
            }
        }
    }
    bound
}
fn relax_lifetimes_path(
    t: RelaxType, p: &mut Path, generics: &mut HashSet<Ident>, bound: &HashSet<Ident>,
) -> Result<(), SynTokenStream> {
    for segment in &mut p.segments {
        match &mut segment.arguments {
            PathArguments::None => { }
            PathArguments::AngleBracketed(arg) => for arg in &mut arg.args {
                match arg {
                    GenericArgument::Constraint(c) =>
                        return Err(error(c.span(), "Constraints are not allowed in impl traits.")),
                    GenericArgument::Const(_) => { }
                    GenericArgument::Lifetime(lt) =>
                        relax_lifetime_instance(t, lt, generics, bound),
                    GenericArgument::Type(ty) =>
                        relax_lifetimes_iter(t, ty, generics, bound)?,
                    GenericArgument::Binding(bind) =>
                        relax_lifetimes_iter(t, &mut bind.ty, generics, bound)?,
                }
            }
            PathArguments::Parenthesized(parens) => {
                for arg in &mut parens.inputs {
                    relax_lifetimes_iter(t, arg, generics, bound)?;
                }
                if let ReturnType::Type(_, ty) = &mut parens.output {
                    relax_lifetimes_iter(t, ty, generics, &bound)?;
                }
            }
        }
    }
    Ok(())
}
fn relax_lifetimes_bound(
    t: RelaxType, ty: &mut TypeParamBound, generics: &mut HashSet<Ident>, bound: &HashSet<Ident>,
) -> Result<(), SynTokenStream> {
    match ty {
        TypeParamBound::Lifetime(lt) =>
            relax_lifetime_instance(t, lt, generics, bound),
        TypeParamBound::Trait(trt) => {
            let bound = bind_lifetimes(t, &mut trt.lifetimes, generics, bound);
            relax_lifetimes_path(t, &mut trt.path, generics, &bound)?;
        }
    }
    Ok(())
}
fn relax_lifetimes_iter(
    t: RelaxType, ty: &mut Type, generics: &mut HashSet<Ident>, bound: &HashSet<Ident>,
) -> Result<(), SynTokenStream> {
    match ty {
        Type::Array(a) => relax_lifetimes_iter(t, &mut a.elem, generics, bound),
        Type::Group(g) => relax_lifetimes_iter(t, &mut g.elem, generics, bound),
        Type::Paren(p) => relax_lifetimes_iter(t, &mut p.elem, generics, bound),
        Type::Path(p) => relax_lifetimes_path(t, &mut p.path, generics, bound),
        Type::Ptr(p) => relax_lifetimes_iter(t, &mut p.elem, generics, bound),
        Type::Slice(s) => relax_lifetimes_iter(t, &mut s.elem, generics, bound),

        Type::BareFn(f) => {
            let bound = bind_lifetimes(t, &mut f.lifetimes, generics, bound);
            for arg in &mut f.inputs {
                relax_lifetimes_iter(t, &mut arg.ty, generics, &bound)?;
            }
            if let ReturnType::Type(_, ty) = &mut f.output {
                relax_lifetimes_iter(t, ty, generics, &bound)?;
            }
            Ok(())
        }
        Type::ImplTrait(i) => {
            for impl_bound in &mut i.bounds {
                relax_lifetimes_bound(t, impl_bound, generics, bound)?;
            }
            Ok(())
        }
        Type::Reference(r) => {
            if let Some(lt) = &mut r.lifetime {
                relax_lifetime_instance(t, lt, generics, bound);
            }
            relax_lifetimes_iter(t, &mut r.elem, generics, bound)
        }
        Type::TraitObject(o) => {
            for trait_bound in &mut o.bounds {
                relax_lifetimes_bound(t, trait_bound, generics, bound)?;
            }
            Ok(())
        }
        Type::Tuple(u) => {
            for ty in &mut u.elems {
                relax_lifetimes_iter(t, ty, generics, bound)?;
            }
            Ok(())
        }

        _ => Ok(())
    }
}

fn as_lt(i: &Ident) -> Lifetime {
    Lifetime {
        apostrophe: i.span(),
        ident: i.clone(),
    }
}
pub fn relax_lifetimes(
    value_ty: &Type, container_ty: &Type,
) -> Result<(SynTokenStream, SynTokenStream, Type, Type), SynTokenStream> {
    let mut generics = HashSet::new();
    let mut value_ty = value_ty.clone();
    relax_lifetimes_iter(RelaxType::Input, &mut value_ty, &mut generics, &HashSet::new())?;

    if generics.is_empty() {
        Ok((SynTokenStream::new(), SynTokenStream::new(), value_ty, container_ty.clone()))
    } else {
        let mut container_ty = container_ty.clone();
        relax_lifetimes_iter(RelaxType::Output, &mut container_ty, &mut generics, &HashSet::new())?;

        let mut lifetimes = SynTokenStream::new();
        let mut where_bounds = SynTokenStream::new();
        for lt in generics {
            let lt_base = as_lt(&lt);
            let lt_in = as_lt(&relaxed_lifetime_ident(RelaxType::Input, &lt));
            let lt_out = as_lt(&relaxed_lifetime_ident(RelaxType::Output, &lt));
            lifetimes.extend(quote! { #lt_in: #lt_out, #lt_out, });
            where_bounds.extend(quote! { #lt_base: #lt_out, });
        }
        Ok((quote! { <#lifetimes> }, quote! { where #where_bounds }, value_ty, container_ty))
    }
}