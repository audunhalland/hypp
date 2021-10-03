//!
//! Callback utilities
//!

use syn::spanned::Spanned;

pub enum Callback {
    Param(syn::Ident),
    SelfMethod(syn::Ident),
}

pub fn parse_callback(expr: syn::Expr) -> Result<Callback, syn::Error> {
    let expr_path = match expr {
        syn::Expr::Path(path) => path,
        other => return Err(syn::Error::new(other.span(), "Not a path")),
    };

    if let Some(_qself) = &expr_path.qself {
        return Err(syn::Error::new(
            expr_path.span(),
            "No \"qself\" in path allowed",
        ));
    }

    if let Some(_colon) = &expr_path.path.leading_colon {
        return Err(syn::Error::new(
            expr_path.span(),
            "No leading colon allowed",
        ));
    }

    path_to_callback(expr_path.path)
}

fn path_to_callback(path: syn::Path) -> Result<Callback, syn::Error> {
    let span = path.span();
    let mut iterator = path.segments.into_iter();

    let first = iterator
        .next()
        .ok_or_else(|| syn::Error::new(span, "Expected a path with at least one segment"))?;

    if first.ident == "Self" {
        let method = iterator
            .next()
            .ok_or_else(|| syn::Error::new(span, "Expected method name"))?;

        if let Some(_) = iterator.next() {
            return Err(syn::Error::new(span, "Expected only two segments in path"));
        }

        Ok(Callback::SelfMethod(method.ident))
    } else {
        Ok(Callback::Param(first.ident))
    }
}
