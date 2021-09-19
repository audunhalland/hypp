use syn::spanned::Spanned;

pub struct Callback {
    pub ident: syn::Ident,
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

    let ident = path_to_self_ident(expr_path.path)?;

    Ok(Callback { ident })
}

fn path_to_self_ident(path: syn::Path) -> Result<syn::Ident, syn::Error> {
    let span = path.span();
    let mut iterator = path.segments.into_iter();

    let _self = iterator
        .next()
        .and_then(|segment| {
            if segment.ident != "Self" {
                None
            } else {
                Some(segment.ident)
            }
        })
        .ok_or_else(|| syn::Error::new(span, "No leading colon allowed"))?;

    let fn_ident = iterator
        .next()
        .and_then(|segment| Some(segment.ident))
        .ok_or_else(|| syn::Error::new(span, "Expected a function name"))?;

    if let Some(_) = iterator.next() {
        return Err(syn::Error::new(span, "Expected only two elements in path"));
    }

    Ok(fn_ident)
}
