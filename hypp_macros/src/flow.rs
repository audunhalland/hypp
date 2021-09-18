//! Param flow analysis
//!

use std::collections::{BTreeMap, BTreeSet};

use crate::ir;
use crate::param;

pub struct FlowScope<'p> {
    pub parent: Option<&'p FlowScope<'p>>,
    pub variables: BTreeMap<syn::Ident, BTreeSet<Dep>>,
}

impl<'p> FlowScope<'p> {
    pub fn new() -> FlowScope<'p> {
        Self {
            parent: None,
            variables: BTreeMap::new(),
        }
    }

    pub fn with_parent(parent: &'p FlowScope<'p>) -> Self {
        Self {
            parent: Some(parent),
            variables: BTreeMap::new(),
        }
    }

    pub fn from_params(params: &'p [param::Param]) -> Self {
        let mut scope = Self::new();

        for param in params {
            let mut deps = BTreeSet::new();
            deps.insert(Dep::Param(param.id));

            scope.variables.insert(param.ident.clone(), deps);
        }

        scope
    }

    pub fn lookup_param_deps_for_ident(&self, ident: &syn::Ident) -> ir::ParamDeps {
        match self.lookup_deps(ident) {
            None => ir::ParamDeps::Const,
            Some(deps) => deps_to_param_deps(deps),
        }
    }

    pub fn lookup_params_deps_for_expr(&self, expr: &syn::Expr) -> ir::ParamDeps {
        // expr should not introduce new variables
        let mut deps = BTreeSet::new();
        collect_expr_deps(expr, &mut deps, self);

        deps_to_param_deps(deps)
    }

    fn lookup_deps(&self, variable: &syn::Ident) -> Option<BTreeSet<Dep>> {
        let mut scope = &self;
        loop {
            if let Some(deps) = scope.variables.get(variable) {
                return Some(deps.clone());
            } else if let Some(parent) = scope.parent.as_ref() {
                scope = parent;
            } else {
                return None;
            }
        }
    }

    pub fn bind_expr(&mut self, pat: &syn::Pat, expr: &syn::Expr) {
        let mut expr_deps = BTreeSet::new();
        collect_expr_deps(expr, &mut expr_deps, self);
        self.bind(pat, expr_deps);
    }

    pub fn bind(&mut self, pat: &syn::Pat, deps: BTreeSet<Dep>) {
        let mut pat_idents = BTreeSet::new();
        collect_pat_idents(pat, &mut pat_idents);

        for pat_ident in pat_idents {
            self.variables.insert(pat_ident.clone(), deps.clone());
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Dep {
    Undecidable,
    Param(u16),
}

fn deps_to_param_deps(deps: BTreeSet<Dep>) -> ir::ParamDeps {
    let mut ids = BTreeSet::new();

    for dep in deps {
        match dep {
            Dep::Undecidable => return ir::ParamDeps::All,
            Dep::Param(id) => {
                ids.insert(id);
            }
        }
    }

    if ids.is_empty() {
        ir::ParamDeps::Const
    } else {
        ir::ParamDeps::Some(ids)
    }
}

/*
fn analyze_block<'p>(block: &syn::Block, parent_scope: &'p FlowScope<'p>) -> FlowScope<'p> {
    let mut scope = FlowScope::with_parent(parent_scope);

    for stmt in &block.stmts {
        analyze_statement(stmt, &mut scope);
    }

    scope
}

fn analyze_statement<'p>(stmt: &syn::Stmt, scope: &mut FlowScope<'p>) {
    match stmt {
        syn::Stmt::Local(local) => {
            if let Some((_, expr)) = &local.init {
                scope.bind_expr(&local.pat, expr);
            }
        }
        syn::Stmt::Item(_) => {
            // not analyzing items
        }
        syn::Stmt::Expr(expr) => {
            panic!("SICJ")
            // collect_expr_deps(expr, collect, None)?;
        }
        syn::Stmt::Semi(semi, _) => {
            panic!("SICJ2")
            // collect_expr_deps(semi, collect, None)?;
        }
    }
}
*/

fn collect_expr_deps<'p>(expr: &syn::Expr, collect: &mut BTreeSet<Dep>, scope: &FlowScope<'p>) {
    match expr {
        syn::Expr::Array(e) => {
            for elem in &e.elems {
                collect_expr_deps(elem, collect, scope);
            }
        }
        syn::Expr::Assign(e) => {
            collect_expr_deps(&e.left, collect, scope);
            collect_expr_deps(&e.right, collect, scope);
        }
        syn::Expr::AssignOp(e) => {
            collect_expr_deps(&e.left, collect, scope);
            collect_expr_deps(&e.right, collect, scope);
        }
        syn::Expr::Async(e) => {
            collect_block_deps_in_new_scope(&e.block, collect, scope);
        }
        syn::Expr::Await(e) => {
            collect_expr_deps(&e.base, collect, scope);
        }
        syn::Expr::Binary(e) => {
            collect_expr_deps(&e.left, collect, scope);
            collect_expr_deps(&e.right, collect, scope);
        }
        syn::Expr::Block(e) => {
            collect_block_deps_in_new_scope(&e.block, collect, scope);
        }
        syn::Expr::Box(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Break(e) => {
            if let Some(expr) = e.expr.as_ref() {
                collect_expr_deps(expr, collect, scope);
            }
        }
        syn::Expr::Call(e) => {
            for arg in &e.args {
                collect_expr_deps(arg, collect, scope);
            }
        }
        syn::Expr::Cast(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Closure(_) => {
            // don't analyze closures internally, but function calls themselves
        }
        syn::Expr::Continue(_) => {
            // no expr
        }
        syn::Expr::Field(e) => {
            collect_expr_deps(&e.base, collect, scope);
        }
        syn::Expr::ForLoop(e) => {
            collect_expr_deps(&e.expr, collect, scope);
            collect_block_deps_in_new_scope(&e.body, collect, scope);
        }
        syn::Expr::Group(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::If(e) => {
            match e.cond.as_ref() {
                syn::Expr::Let(the_let) => {
                    // Handle new scope
                    let mut new_scope = FlowScope::with_parent(scope);
                    new_scope.bind_expr(&the_let.pat, &the_let.expr);

                    collect_block_deps(&e.then_branch, collect, &new_scope);
                }
                _ => {
                    collect_expr_deps(&e.cond, collect, scope);
                    collect_block_deps_in_new_scope(&e.then_branch, collect, scope);
                    if let Some((_, els)) = e.else_branch.as_ref() {
                        collect_expr_deps(els, collect, scope);
                    }
                }
            }
        }
        syn::Expr::Index(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Let(_) => {
            panic!("Let expression cannot be analyzed in isolation");
        }
        syn::Expr::Lit(_) => {}
        syn::Expr::Loop(e) => {
            collect_block_deps_in_new_scope(&e.body, collect, scope);
        }
        syn::Expr::Macro(_) => {
            // TODO: emit warning?
            collect.insert(Dep::Undecidable);
        }
        syn::Expr::Match(e) => {
            for arm in &e.arms {
                let mut new_scope = FlowScope::with_parent(scope);
                new_scope.bind_expr(&arm.pat, &e.expr);

                if let Some((_, guard)) = &arm.guard {
                    collect_expr_deps(guard, collect, &new_scope);
                }

                collect_expr_deps(&arm.body, collect, &new_scope);
            }
        }
        syn::Expr::MethodCall(e) => {
            collect_expr_deps(&e.receiver, collect, scope);
            for arg in &e.args {
                collect_expr_deps(&arg, collect, scope);
            }
        }
        syn::Expr::Paren(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Path(e) => {
            // this is where the "real" ident is..
            if e.qself.is_none() && e.path.leading_colon.is_none() && e.path.segments.len() == 1 {
                let segment0 = &e.path.segments[0];

                // and we may look up the deps
                if let Some(deps) = scope.lookup_deps(&segment0.ident) {
                    for dep in deps.into_iter() {
                        collect.insert(dep);
                    }
                }
            }
        }
        syn::Expr::Range(_) => {}
        syn::Expr::Reference(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Repeat(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Return(e) => {
            if let Some(expr) = e.expr.as_ref() {
                collect_expr_deps(expr, collect, scope);
            }
        }
        syn::Expr::Struct(e) => {
            for field in &e.fields {
                collect_expr_deps(&field.expr, collect, scope);
            }
        }
        syn::Expr::Try(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::TryBlock(e) => {
            collect_block_deps_in_new_scope(&e.block, collect, scope);
        }
        syn::Expr::Tuple(e) => {
            for elem in &e.elems {
                collect_expr_deps(elem, collect, scope);
            }
        }
        syn::Expr::Type(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Unary(e) => {
            collect_expr_deps(&e.expr, collect, scope);
        }
        syn::Expr::Unsafe(e) => {
            collect_block_deps_in_new_scope(&e.block, collect, scope);
        }
        syn::Expr::Verbatim(_) => {
            // TODO: Emit warning
            collect.insert(Dep::Undecidable);
        }
        syn::Expr::While(e) => {
            collect_expr_deps(&e.cond, collect, scope);
            collect_block_deps_in_new_scope(&e.body, collect, scope);
        }
        syn::Expr::Yield(e) => {
            if let Some(expr) = e.expr.as_ref() {
                collect_expr_deps(expr, collect, scope);
            }
        }
        _ => {}
    };
}

/// Patterns are used for defining new variables.
fn collect_pat_idents<'p>(pat: &'p syn::Pat, idents: &mut BTreeSet<&'p syn::Ident>) {
    match pat {
        syn::Pat::Box(p) => {
            collect_pat_idents(&p.pat, idents);
        }
        syn::Pat::Ident(p) => {
            idents.insert(&p.ident);
        }
        syn::Pat::Lit(_) => {}
        syn::Pat::Macro(_) => {
            // Don't analyze.
            // The worst thing that can happen for _patterns_ is that
            // more idents can leak out of the scope (and we do more work than necessary,
            // but that's not a bug)
        }
        syn::Pat::Or(p) => {
            for case in &p.cases {
                collect_pat_idents(case, idents);
            }
        }
        syn::Pat::Path(_) => {}
        syn::Pat::Range(_) => {}
        syn::Pat::Reference(p) => {
            collect_pat_idents(&p.pat, idents);
        }
        syn::Pat::Rest(_) => {}
        syn::Pat::Slice(p) => {
            for elem in &p.elems {
                collect_pat_idents(elem, idents);
            }
        }
        syn::Pat::Struct(p) => {
            for field in &p.fields {
                collect_pat_idents(&field.pat, idents);
            }
        }
        syn::Pat::Tuple(p) => {
            for elem in &p.elems {
                collect_pat_idents(elem, idents);
            }
        }
        syn::Pat::TupleStruct(p) => {
            for elem in &p.pat.elems {
                collect_pat_idents(elem, idents);
            }
        }
        syn::Pat::Type(p) => {
            collect_pat_idents(&p.pat, idents);
        }
        syn::Pat::Verbatim(token_stream) => {
            panic!("unable to analyze pattern: {:?}", token_stream);
        }
        syn::Pat::Wild(_) => {}
        _ => {}
    };
}

// Analyze block in a new scope with no new parameters
fn collect_block_deps_in_new_scope<'p>(
    block: &syn::Block,
    collect: &mut BTreeSet<Dep>,
    parent_scope: &FlowScope<'p>,
) {
    let mut new_scope = FlowScope::with_parent(parent_scope);
    collect_block_deps(block, collect, &mut new_scope);
}

fn collect_block_deps<'p>(block: &syn::Block, collect: &mut BTreeSet<Dep>, scope: &FlowScope<'p>) {
    for stmt in &block.stmts {
        match stmt {
            syn::Stmt::Local(local) => {
                // Don't care about new variable introductions, we only want usages
                // from scope
                if let Some(init) = &local.init {
                    collect_expr_deps(&init.1, collect, scope);
                }
            }
            syn::Stmt::Expr(expr) => {
                collect_expr_deps(expr, collect, scope);
            }
            syn::Stmt::Item(_) => {}
            syn::Stmt::Semi(expr, _) => {
                collect_expr_deps(expr, collect, scope);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::flow::analyze_block;

    use super::*;
    use maplit::*;

    fn ident(string: &'static str) -> syn::Ident {
        quote::format_ident!("{}", string)
    }

    #[test]
    fn test_analyze_basic_block() {
        let parent_scope = FlowScope {
            parent: None,
            variables: btreemap! {
                ident("i0") => btreeset! { Dep::Param(0) },
                ident("i1") => btreeset! { Dep::Param(1) }
            },
        };

        let block = syn::parse_quote!({
            let a = i0;
            let b = i0.clone();

            let c = if let Some(q) = i1 { q.foo } else { None };
        });

        let scope = analyze_block(&block, &parent_scope);

        assert_eq!(
            scope.variables,
            btreemap! {
                ident("a") => btreeset! { Dep::Param(0) },
                ident("b") => btreeset! { Dep::Param(0) },
                ident("c") => btreeset! { Dep::Param(1) },
            }
        );
    }

    #[test]
    fn test_analyze_if_let_with_shadowing_scope() {
        let parent_scope = FlowScope {
            parent: None,
            variables: btreemap! {
                ident("i0") => btreeset! { Dep::Param(0) },
                ident("i1") => btreeset! { Dep::Param(1) }
            },
        };

        let block = syn::parse_quote!({
            // This does not depend on i0:
            let q = if let Some(i0) = stuff {
                return i1;
            } else {
                None
            };
        });

        let scope = analyze_block(&block, &parent_scope);

        assert_eq!(
            scope.variables,
            btreemap! {
                ident("q") => btreeset! { Dep::Param(1) },
            }
        );
    }
}
