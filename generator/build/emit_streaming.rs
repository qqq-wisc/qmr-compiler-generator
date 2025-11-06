use crate::ast::*;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use std::io::Write;
use std::path::Path;
use syn::Ident;

pub fn emit_program(p: &ProblemDefinition, out_path: &std::path::Path) -> std::io::Result<()> {
    let mut f = std::fs::File::create(out_path)?;
    let use_statements = quote! {
        use solver::structures::*;
        use solver::structures::GateType::*;
        use solver::utils::*;
        use solver::backend;
        use serde_json::{Value, from_value};
        use petgraph::{graph::NodeIndex, Graph};
        use std::collections::{HashMap, HashSet};
        use std::fs::File;
    };
    writeln!(f, "{}", use_statements)?;
    emit_gate_types(&p.imp.routed_gates, &mut f)?;
    emit_define_struct(&p.imp.data, &mut f)?;
    emit_define_arch_struct(&p.arch, &mut f)?;
    emit_define_struct(&p.trans.data, &mut f)?;
    emit_impl_gate(&p.imp.data, &mut f)?;
    emit_impl_gate_methods(&p.imp.data, &mut f)?;
    emit_impl_arch(&p.arch, &mut f)?;
    emit_impl_arch_methods(&p.arch, &mut f)?;
    emit_impl_trans(&p.trans, &p.imp, &mut f)?;
    emit_available_transitions(&p.trans, &p.imp, &mut f)?;
    emit_realize_gate_function(&p.imp, &mut f)?;
    emit_solve_function(&p.imp, &mut f)?;
    emit_sabre_solve_function(&p.imp, &mut f)?;
    emit_joint_optimize_parallel_function(&p.imp, &mut f)?;
    emit_step_cost(p, &mut f)?;
    emit_mapping_heuristic(&mut f)?;
    Ok(())
}

fn contains_subexpr(e: &Expr, subexpr: &Expr) -> bool {
    if e == subexpr {
        return true;
    }
    match e {
        Expr::SwapPair(left, right) => {
            contains_subexpr(left, subexpr) || contains_subexpr(right, subexpr)
        }
        Expr::GetData { d: _, access: _ } => false,
        Expr::FloatLiteral(_) => false,
        Expr::ITE { cond, then, els } => {
            contains_subexpr(cond, subexpr)
                || contains_subexpr(then, subexpr)
                || contains_subexpr(els, subexpr)
        }
        Expr::CallMethod {
            d: _,
            method: _,
            args,
        } => args.iter().any(|arg| contains_subexpr(arg, subexpr)),
        Expr::Append { vec, elem } => {
            contains_subexpr(vec, subexpr) || contains_subexpr(elem, subexpr)
        }
        Expr::LocationLiteral(_) => false,
        Expr::TransitionConstructor(vec) => {
            vec.iter().any(|(_, expr)| contains_subexpr(expr, subexpr))
        }
        Expr::Tuple(vec) => vec.iter().any(|expr| contains_subexpr(expr, subexpr)),
        Expr::MapAccess(expr) => contains_subexpr(expr, subexpr),
        Expr::NoneExpr => false,
        Expr::SomeExpr(expr) => contains_subexpr(expr, subexpr),
        Expr::MapIterExpr {
            container: c,
            bound_var: _,
            func: f,
        } => contains_subexpr(c, subexpr) || contains_subexpr(f, subexpr),
        Expr::ImplConstructorExpr(vec) => {
            vec.iter().any(|(_, expr)| contains_subexpr(expr, subexpr))
        }
        Expr::Ident(_) => false,
        Expr::BinOp(op, expr, expr1) => {
            contains_subexpr(expr, subexpr) || contains_subexpr(expr1, subexpr)
        }
        Expr::IndexLiteral(_) => false,
        Expr::EmptyVec => false,
        Expr::FoldExpr {
            container,
            init,
            func,
        } => {
            contains_subexpr(container, subexpr)
                || contains_subexpr(init, subexpr)
                || contains_subexpr(func, subexpr)
        }
        Expr::Extend { vec1, vec2 } => {
            contains_subexpr(vec1, subexpr) || contains_subexpr(vec2, subexpr)
        }
        Expr::CallFunction { func: _, args } => {
            args.iter().any(|arg| contains_subexpr(arg, subexpr))
        }
        Expr::OptionMatch {
            expr,
            some_arm,
            none_arm,
        } => {
            contains_subexpr(expr, subexpr)
                || contains_subexpr(some_arm, subexpr)
                || contains_subexpr(none_arm, subexpr)
        }
        Expr::GetAnonData {
            ident: _,
            access: _,
        } => false,
        Expr::RangeExpr { bot, top } => {
            contains_subexpr(bot, subexpr) || contains_subexpr(top, subexpr)
        }
    }
}

fn emit_define_struct(data: &NamedTuple, out: &mut impl std::io::Write) -> std::io::Result<()> {
    let span: Span = Span::call_site();        // keep the original span
    let name = data.name.to_string();         // make a String once

    let struct_name: Ident = match name.as_str() {
        "GateRealization" => Ident::new("CustomRealization", span),
        "Transition"      => Ident::new("CustomTransition", span),
        "Arch"            => Ident::new("CustomArch", span),
        other             => Ident::new(other, span),
    };
    let fields = data.fields.iter().map(|(name, ty)| {
        let field_name = syn::Ident::new(name, Span::call_site());
        let field_ty: syn::Type = emit_type(ty);
        quote! { #field_name : #field_ty }
    });
    let struct_def = quote! {
        #[derive(Hash, PartialEq, Eq, Clone, serde::Serialize, Debug)]
        pub struct #struct_name {
            #(#fields),*
        }
    };
    writeln!(out, "{}", struct_def)?;
    Ok(())
}

fn emit_gate_type(g: &GateType) -> TokenStream {
    match g {
        GateType::CX => quote! {"CX"},
        GateType::T => quote! {"T"},
        GateType::Pauli => quote! {"Pauli"},
    }
}

fn emit_gate_types(
    routed_gates: &Vec<GateType>,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let gate_types = routed_gates.iter().map(|g| emit_gate_type(g));
    let gt_quote = quote! {const GATE_TYPES : &[&str] = &[#(#gate_types),*];};
    writeln!(out, "{}", gt_quote)?;
    Ok(())
}
fn emit_type(ty: &Ty) -> syn::Type {
    match ty {
        Ty::LocationTy => syn::parse_quote!(Location),
        Ty::TupleTy(vec) => {
            let tuple_fields = vec.iter().map(|ty| emit_type(ty));
            syn::parse_quote!((#(#tuple_fields),*))
        }
        Ty::VectorTy(ty) => {
            let inner_ty = emit_type(ty);
            syn::parse_quote!(Vec<#inner_ty>)
        }
        Ty::IntTy => syn::parse_quote!(usize),
        Ty::FloatTy => syn::parse_quote!(f64),
    }
}

fn emit_define_arch_struct(
    arch: &Option<ArchitectureBlock>,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let extra_fields_quote = match arch {
        Some(ref arch) => {
            let extra_fields = arch.data.fields.iter().map(|(name, ty)| {
                let field_name = syn::Ident::new(name, Span::call_site());
                let field_ty = emit_type(ty);
                quote! { #field_name : #field_ty }
            });
            quote! {#(#extra_fields),*}
        }
        None => {
            quote! {}
        }
    };
    let arch_name = syn::Ident::new("CustomArch", Span::call_site());
    let arch_struct_def_quote = quote! {
            #[derive(Clone)]
            struct #arch_name {
                graph: Graph<Location, ()>,
                index_map: HashMap<Location, NodeIndex>,
                #extra_fields_quote

            }
    };
    writeln!(out, "{}", arch_struct_def_quote)?;
    Ok(())
}
fn emit_impl_gate(imp_data: &NamedTuple, out: &mut impl std::io::Write) -> std::io::Result<()> {
    let struct_name = syn::Ident::new(&"CustomRealization", Span::call_site());
    let impl_gate_quote = quote! {impl GateImplementation for #struct_name {}};
    writeln!(out, "{}", impl_gate_quote)?;
    Ok(())
}

fn emit_impl_gate_methods(
    imp_data: &NamedTuple,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let struct_name = syn::Ident::new(&imp_data.name, Span::call_site());
    let getters = imp_data.fields.iter().map(|(name, ty)| {
        if let Ty::VectorTy(_) = ty {
            let field_name = syn::Ident::new(name, Span::call_site());
            let ty_quote = emit_type(ty);
            let getter = quote! {
                pub fn #field_name(&self) -> #ty_quote {
                    return self.#field_name.clone();
                }
            };
            getter
        } else {
            quote! {}
        }
    });
    let methods_quote = quote! {
        impl CustomRealization{
            #(#getters)*
        }
    };
    writeln!(out, "{}", methods_quote)?;
    Ok(())
}

fn emit_impl_arch(arch: &Option<ArchitectureBlock>, out: &mut impl Write) -> std::io::Result<()> {
    let arch_name = Ident::new("CustomArch", Span::call_site());

    // impl header
    writeln!(out, "impl Architecture for {arch_name} {{")?;

    // fn locations
    writeln!(out, "    fn locations(&self) -> Vec<Location> {{")?;
    match arch {
        Some(ArchitectureBlock {
            data: _d,
            get_locations: Some(expr),
        }) => {
            // Preserve your original `return #get_locations;`
            write!(out, "        return ")?;
            emit_expr_streaming(
                expr,
                &Context::DataTypeContext(DataType::Arch),
                &arch_name,
                &arch_name,
                None,
                out,
            )?;
            writeln!(out, ";")?;
        }
        _ => {
            writeln!(out, "        let mut locations = Vec::new();")?;
            writeln!(out, "        for node in self.graph.node_indices() {{")?;
            writeln!(out, "            locations.push(self.graph[node]);")?;
            writeln!(out, "        }}")?;
            writeln!(out, "        return locations;")?;
        }
    }
    writeln!(out, "    }}")?; // end locations

    // fn graph
    writeln!(
        out,
        "    fn graph(&self) -> (Graph<Location, ()>, HashMap<Location, NodeIndex>) {{"
    )?;
    writeln!(
        out,
        "        return (self.graph.clone(), self.index_map.clone());"
    )?;
    writeln!(out, "    }}")?; // end graph

    // end impl
    writeln!(out, "}}")?;
    Ok(())
}

fn emit_impl_arch_methods(
    arch: &Option<ArchitectureBlock>,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let struct_name = syn::Ident::new("CustomArch", Span::call_site());
    let getters = match arch {
        Some(arch) => {
            let mut getters = Vec::new();
            for (name, ty) in &arch.data.fields {
                match ty {
                    Ty::VectorTy(_) => {
                        let field_name = syn::Ident::new(name, Span::call_site());
                        let ty_quote = emit_type(ty);
                        let getter = quote! {
                            pub fn #field_name(&self) -> #ty_quote{
                                return self.#field_name.clone();
                            }
                        };
                        getters.push(getter);
                    }
                    _ => {}
                }
            }
            getters
        }
        None => Vec::new(),
    };
    let universal_from_file_body = quote! {
        let file = File::open(path).expect("Opening architecture file");
        let parsed: Value = serde_json::from_reader(file).expect("Parsing architecture file");
        let graph = graph_from_json_entry(parsed["graph"].clone());
        let mut index_map = HashMap::new();
        for ind in graph.node_indices() {
            index_map.insert(graph[ind], ind);
        }
    };

    let custom_from_file_body = match arch {
        None => {
            quote! {
                return #struct_name { graph, index_map };
            }
        }
        Some(arch) => {
            let set_extra_fields = arch.data.fields.iter().map(|(name, _)| {
                let field_name = syn::Ident::new(name, Span::call_site());
                quote! {
                    #field_name : from_value(parsed[#name].clone()).expect(&format!("Parsing {} field", #name.to_string())),
                }
            });
            quote! {
                return #struct_name { graph, index_map, #(#set_extra_fields)* };
            }
        }
    };
    let arch_methods_quote = quote! {
        impl #struct_name {
            fn from_file(path: &str) -> Self {
                #universal_from_file_body
                #custom_from_file_body
            }

            fn contains_edge(&self, edge: (Location, Location)) -> bool {
                self.graph.contains_edge(self.index_map[&edge.0], self.index_map[&edge.1])
            }
        pub fn edges(&self) -> Vec<(Location, Location)> {
            let mut edges = Vec::new();
            for edge in self.graph.edge_indices() {
                let (u, v) = self.graph.edge_endpoints(edge).unwrap();
                edges
                    .push((self.graph[u], self.graph[v]),
                    );
            }
            return edges;
        }
        #(#getters)*
    }
    };
    writeln!(out, "{}", arch_methods_quote)?;
    Ok(())
}

fn emit_impl_trans(
    t: &TransitionBlock,
    imp: &ImplBlock,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let trans_struct_name = syn::Ident::new(&t.data.name, Span::call_site());
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    writeln!(
        out,
        "impl Transition<CustomRealization, CustomArch> for CustomTransition{{",
    )?;
    writeln!(
        out,
        " fn apply(&self, step: &Step<CustomRealization>) -> Step<CustomRealization> {{"
    )?;
    emit_expr_streaming(
        &t.apply,
        &Context::DataTypeContext(DataType::Transition),
        &trans_struct_name,
        &imp_struct_name,
        None,
        out,
    )?;
    writeln!(out, "}}")?;
    writeln!(
        out,
        "fn repr(&self) -> String {{
                return format!(\"{{:?}}\", self);
}}"
    )?;
    writeln!(out, "fn cost(&self, arch: &CustomArch) -> f64 {{")?;
     emit_expr_streaming(
        &t.cost,
        &Context::DataTypeContext(DataType::Transition),
        &trans_struct_name,
        &imp_struct_name,
        None,
        out,
    )?;
    writeln!(out, "}}")?;
    writeln!(out, "}}")?;
    Ok(())
}

fn emit_available_transitions(
    t: &TransitionBlock,
    imp: &ImplBlock,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let trans_struct_name = syn::Ident::new(&t.data.name, Span::call_site());
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    writeln!(out, "fn available_transitions(arch : &CustomArch, step : &Step<CustomRealization>) -> Vec<CustomTransition> {{")?;
    emit_expr_streaming(
        &t.get_transitions,
        &Context::Free,
        &trans_struct_name,
        &imp_struct_name,
        None,
        out,
    )?;
    writeln!(out, "}}")?;
    Ok(())
}

fn emit_realize_gate_function(
    imp: &ImplBlock,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    writeln!(out, "fn realize_gate(
            step: &Step<CustomRealization>,
            arch: &CustomArch,
            gate: &Gate,
        ) -> impl IntoIterator<Item = CustomRealization> {{")?;
    emit_expr_streaming(
        &imp.realize,
        &Context::Free,
        &imp_struct_name,
        &imp_struct_name,
        None,
        out
    )?;
    writeln!(out, "}}")?;
    Ok(())
}

fn emit_mapping_heuristic(out: &mut impl std::io::Write) -> std::io::Result<()> {
    let map_heuristic_quote = quote! {
        fn mapping_heuristic(arch: &CustomArch, c: &Circuit, map: &HashMap<Qubit, Location>) -> f64 {
            let graph = &arch.graph;
            let mut cost = 0;
            for gate in &c.gates {
                if gate.qubits.len() < 2 {
                    continue
                }
                let (cpos, tpos) = (map.get(&gate.qubits[0]), map.get(&gate.qubits[1]));
                let (cind, tind) = (arch.index_map[cpos.unwrap()], arch.index_map[tpos.unwrap()]);
                let sp_res = petgraph::algo::astar(graph, cind, |n| n == tind, |_| 1, |_| 1);
                match sp_res {
                    Some((c, _)) => cost += c,
                    None => panic!("Disconnected graph. No path found from {:?} to {:?}", cpos, tpos)
                }
            }
            return cost as f64;
        }
    };
    writeln!(out, "{}", map_heuristic_quote)?;
    Ok(())
}


fn emit_step_cost(p: &ProblemDefinition, out: &mut impl Write) -> std::io::Result<()> {
    let imp_struct_name   = Ident::new(&p.imp.data.name,   Span::call_site());
    let trans_struct_name = Ident::new(&p.trans.data.name, Span::call_site());

    // fn header
    writeln!(
        out,
        "fn custom_step_cost(step: &Step<CustomRealization>, arch: &CustomArch) -> f64 {{",
    )?;

    match &p.step {
        Some(s) => {
            write!(out, "    return ")?;
            emit_expr_streaming(
                &s.cost,
                &Context::DataTypeContext(DataType::Step),
                &trans_struct_name,
                &imp_struct_name,
                None,
                out,
            )?;
            writeln!(out, ";")?;
        }
        None => {
            writeln!(out, "    return 0.0;")?;
        }
    }

    writeln!(out, "}}")?;
    Ok(())
}


fn emit_solve_function(imp: &ImplBlock, out: &mut impl std::io::Write) -> std::io::Result<()> {
    let sub_expr = Expr::CallMethod {
        d: DataType::Step,
        method: "implemented_gates".to_string(),
        args: vec![],
    };
    let explore_orders = contains_subexpr(&imp.realize, &sub_expr);
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    let solve_quote = quote! {
        fn my_solve(c : &Circuit, a : &CustomArch) -> CompilerResult<CustomRealization> {
            return backend::solve(c, a, &|s| available_transitions(a, s), &realize_gate, custom_step_cost, Some(mapping_heuristic), #explore_orders);
    }
    };
    writeln!(out, "{}", solve_quote)?;
    Ok(())
}

fn emit_sabre_solve_function(
    imp: &ImplBlock,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let sub_expr = Expr::CallMethod {
        d: DataType::Step,
        method: "implemented_gates".to_string(),
        args: vec![],
    };
    let explore_orders = contains_subexpr(&imp.realize, &sub_expr);
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    let sabre_solve_quote = quote! {
        fn my_sabre_solve(c : &Circuit, a : &CustomArch) -> CompilerResult<CustomRealization> {
            return backend::sabre_solve(c, a, &|s| available_transitions(a, s), &realize_gate, custom_step_cost, Some(mapping_heuristic), #explore_orders);
    }
    };
    writeln!(out, "{}", sabre_solve_quote)?;
    Ok(())
}

fn emit_joint_optimize_parallel_function(
    imp: &ImplBlock,
    out: &mut impl std::io::Write,
) -> std::io::Result<()> {
    let sub_expr = Expr::CallMethod {
        d: DataType::Step,
        method: "implemented_gates".to_string(),
        args: vec![],
    };
    let explore_orders = contains_subexpr(&imp.realize, &sub_expr);
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    let joint_optimize_par_quote = quote! {
        fn my_joint_solve_parallel(c : &Circuit, a : &CustomArch) -> CompilerResult<CustomRealization> {
            return backend::solve_joint_optimize_parallel(c, a, &|s| available_transitions(a, s), &realize_gate, custom_step_cost, Some(mapping_heuristic), #explore_orders);
    }
    };
    writeln!(out, "{}", joint_optimize_par_quote)?;
    Ok(())
}

/// Streaming version of `emit_expr`: writes Rust source directly to `out`.
pub fn emit_expr_streaming<W: Write>(
    e: &Expr,
    context: &Context,
    trans_struct_name: &Ident,
    imp_struct_name: &Ident,
    bound_var: Option<&str>,
    out: &mut W,
) -> std::io::Result<()> {
    match e {
        Expr::FloatLiteral(n) =>  write!(out, "{}f64", n)?,
        Expr::LocationLiteral(n) => write!(out, "Location::new({n})")?,
        Expr::IndexLiteral(i) => write!(out, "{i}")?,
        Expr::Ident(s) => write!(out, "{s}")?,
        Expr::EmptyVec => write!(out, "Vec::new()")?,
        Expr::Tuple(vec) => {
            write!(out, "(")?;
            for (i, ex) in vec.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                emit_expr_streaming(
                    ex,
                    context,
                    trans_struct_name,
                    imp_struct_name,
                    bound_var,
                    out,
                )?;
            }
            write!(out, ")")?;
        }
        Expr::NoneExpr => write!(out, "None")?,
        Expr::SomeExpr(inner) => {
            write!(out, "Some(")?;
            emit_expr_streaming(
                inner,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ")")?;
        }
        Expr::SwapPair(left, right) => {
            writeln!(out, "{{")?;
            writeln!(out, "    let mut new_step = step.clone();")?;
            write!(out, "    let left = ")?;
            emit_expr_streaming(
                left,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, ";")?;
            write!(out, "    let right = ")?;
            emit_expr_streaming(
                right,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, ";")?;
            writeln!(out, "    new_step.map = swap_keys(&step.map, left, right);")?;
            writeln!(out, "    new_step.implemented_gates = HashSet::new();")?;
            writeln!(out, "    return new_step;")?;
            write!(out, "}}")?;
        }
        Expr::GetData { d, access } => {
            let field_name = emit_access_expr(
                access,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
            );
            let name = match (context, d) {
                (Context::DataTypeContext(dc), d) if dc == d => "self",
                (_, DataType::Arch) => "arch",
                (_, DataType::Transition) => "t",
                (_, DataType::Step) => "step",
                (_, DataType::Impl) => "gi",
                (_, DataType::Gate) => "gate",
            };
            write!(out, "{name}.{field_name}")?;
        }
        Expr::GetAnonData { ident, access } => {
            let field_name = emit_access_expr(
                access,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
            );
            write!(out, "{}.{}", ident, field_name)?;
        }
        Expr::CallMethod { d, method, args } => {
            let target = match (context, d) {
                (Context::DataTypeContext(dc), d) if dc == d => "self",
                (_, DataType::Arch) => "arch",
                (_, DataType::Transition) => "t",
                (_, DataType::Step) => "step",
                (_, DataType::Impl) => "gi",
                (_, DataType::Gate) => "gate",
            };
            write!(out, "{target}.{method}(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                emit_expr_streaming(
                    arg,
                    context,
                    trans_struct_name,
                    imp_struct_name,
                    bound_var,
                    out,
                )?;
            }
            write!(out, ")")?;
        }
        Expr::CallFunction { func, args } => {
            write!(out, "{func}(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                emit_expr_streaming(
                    arg,
                    context,
                    trans_struct_name,
                    imp_struct_name,
                    bound_var,
                    out,
                )?;
            }
            write!(out, ")")?;
        }
        Expr::ITE { cond, then, els } => {
            write!(out, "if ")?;
            emit_expr_streaming(
                cond,
                context,
                imp_struct_name,
                trans_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, " {{")?;
            emit_expr_streaming(
                then,
                context,
                imp_struct_name,
                trans_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, "}} else {{")?;
            emit_expr_streaming(
                els,
                context,
                imp_struct_name,
                trans_struct_name,
                bound_var,
                out,
            )?;
            write!(out, "}}")?;
        }
        Expr::OptionMatch {
            expr,
            some_arm,
            none_arm,
        } => {
            write!(out, "match ")?;
            emit_expr_streaming(
                expr,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, " {{")?;
            writeln!(out, "    Some(x) => ")?;
            emit_expr_streaming(
                some_arm,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, ",")?;
            writeln!(out, "    None => ")?;
            emit_expr_streaming(
                none_arm,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            writeln!(out, ",")?;
            write!(out, "}}")?;
        }
        Expr::MapIterExpr {
            container,
            bound_var: bv,
            func,
        } => {
            emit_expr_streaming(
                container,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ".into_iter().map(|{bv}| ")?;
            emit_expr_streaming(
                func,
                context,
                trans_struct_name,
                imp_struct_name,
                Some(bv.as_str()),
                out,
            )?;
            write!(out, ")")?;
        }
        Expr::FoldExpr {
            container,
            init,
            func,
        } => {
            emit_expr_streaming(
                container,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ".into_iter().fold(")?;
            emit_expr_streaming(
                init,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ", |acc, x| ")?;
            emit_expr_streaming(
                func,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ")")?;
        }
        Expr::Append { vec, elem } => {
            write!(out, "push_and_return(")?;
            emit_expr_streaming(
                vec,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ", ")?;
            emit_expr_streaming(
                elem,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ")")?;
        }
        Expr::Extend { vec1, vec2 } => {
            write!(out, "extend_and_return(")?;
            emit_expr_streaming(
                vec1,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ", ")?;
            emit_expr_streaming(
                vec2,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ")")?;
        }
        Expr::TransitionConstructor(fields) => {
            write!(out, "{} {{ ", "CustomTransition")?;
            for (i, (name, expr)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write!(out, "{}: ", name)?;
                emit_expr_streaming(
                    expr,
                    context,
                    trans_struct_name,
                    imp_struct_name,
                    bound_var,
                    out,
                )?;
            }
            write!(out, " }}")?;
        }
        Expr::ImplConstructorExpr(fields) => {
            write!(out, "{} {{ ", "CustomRealization")?;
            for (i, (name, expr)) in fields.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write!(out, "{}: ", name)?;
                emit_expr_streaming(
                    expr,
                    context,
                    trans_struct_name,
                    imp_struct_name,
                    bound_var,
                    out,
                )?;
            }
            write!(out, " }}")?;
        }
        Expr::BinOp(op, lhs, rhs) => {
            emit_expr_streaming(
                lhs,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            let sym = match op {
                BinOp::Equals => "==",
                BinOp::Div => "/",
                BinOp::Mult => "*",
                BinOp::Plus => "+",
                BinOp::Minus => "-",
            };
            write!(out, " {sym} ")?;
            emit_expr_streaming(
                rhs,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
        }
        Expr::RangeExpr { bot, top } => {
            write!(out, "(")?;
            emit_expr_streaming(
                bot,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, "..")?;
            emit_expr_streaming(
                top,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, ")")?;
        }
        Expr::MapAccess(expr) => {
            write!(out, "step.map[&")?;
            emit_expr_streaming(
                expr,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
                out,
            )?;
            write!(out, "]")?;
        }
    }
    Ok(())
}

fn emit_expr(
    e: &Expr,
    context: &Context,
    trans_struct_name: &Ident,
    imp_struct_name: &Ident,
    bound_var: Option<&str>,
) -> TokenStream {
    match e {
        Expr::SwapPair(left, right) => {
            let emit_left = emit_expr(left, context, trans_struct_name, imp_struct_name, bound_var);
            let emit_right = emit_expr(
                right,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
            );
            quote! {
                let mut new_step = step.clone();
                let left = #emit_left;
                let right = #emit_right;
                new_step.map = swap_keys(&step.map, left, right);
                new_step.implemented_gates = HashSet::new();
                return new_step;
            }
        }
        Expr::GetData { d, access } => {
            let field_name = emit_access_expr(
                access,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
            );
            let name = match (context, d) {
                (Context::DataTypeContext(dc), d) if dc == d => "self",
                (_, DataType::Arch) => "arch",
                (_, DataType::Transition) => "t",
                (_, DataType::Step) => "step",
                (_, DataType::Impl) => "gi",
                (_, DataType::Gate) => "gate",
            };
            let data_name = syn::Ident::new(name, Span::call_site());
            quote! {
                #data_name.#field_name
            }
        }
        Expr::FloatLiteral(n) => quote! {#n},
        Expr::ITE { cond, then, els } => {
            let emit_cond = emit_expr(
                cond,
                context,
                &imp_struct_name,
                &trans_struct_name,
                bound_var,
            );
            match (&**then, &**els) {
                (
                    Expr::MapIterExpr {
                        container: container_then,
                        bound_var: bv_then,
                        func: func_then,
                    },
                    Expr::MapIterExpr {
                        container: container_els,
                        bound_var: bv_els,
                        func: func_els,
                    },
                ) if func_then == func_els && bv_then == bv_els => {
                    let emit_container_then = emit_expr(
                        container_then,
                        context,
                        &imp_struct_name,
                        &trans_struct_name,
                        bound_var,
                    );
                    let emit_container_else = emit_expr(
                        container_els,
                        context,
                        &imp_struct_name,
                        &trans_struct_name,
                        bound_var,
                    );
                    let emit_func = emit_expr(
                        &func_els,
                        context,
                        &trans_struct_name,
                        &imp_struct_name,
                        Some(bv_els),
                    );
                    quote! {
                        let container = if #emit_cond {#emit_container_then} else{#emit_container_else};
                        container.into_iter().map(|x| #emit_func)

                    }
                }
                _ => {
                    let emit_then = emit_expr(
                        then,
                        context,
                        &imp_struct_name,
                        &trans_struct_name,
                        bound_var,
                    );
                    let emit_else = emit_expr(
                        els,
                        context,
                        &imp_struct_name,
                        &trans_struct_name,
                        bound_var,
                    );
                    quote! {
                        if #emit_cond {
                            #emit_then
                        } else {
                            #emit_else
                        }
                    }
                }
            }
        }
        Expr::CallMethod { d, method, args } => {
            let method_identifier = syn::Ident::new(method, Span::call_site());
            let args = args.iter().map(|arg| {
                emit_expr(
                    arg,
                    context,
                    &trans_struct_name,
                    &imp_struct_name,
                    bound_var,
                )
            });
            let name = match (context, d) {
                (Context::DataTypeContext(dc), d) if dc == d => "self",
                (_, DataType::Arch) => "arch",
                (_, DataType::Transition) => "t",
                (_, DataType::Step) => "step",
                (_, DataType::Impl) => "gi",
                (_, DataType::Gate) => "gate",
            };
            let data_name = syn::Ident::new(name, Span::call_site());

            quote! {
                #data_name.#method_identifier(#(#args),*)
            }
        }
        Expr::Append { vec, elem } => {
            let emit_vec = emit_expr(vec, context, trans_struct_name, imp_struct_name, bound_var);
            let emit_elem = emit_expr(elem, context, trans_struct_name, imp_struct_name, bound_var);
            quote! {
                push_and_return(#emit_vec, #emit_elem)
            }
        }
        Expr::LocationLiteral(n) => {
            let unsuffixed = syn::Index::from(*n);
            quote! {Location::new(#unsuffixed)}
        }
        Expr::TransitionConstructor(vec) => {
            let fields = vec.iter().map(|(name, expr)| {
                let field_name = syn::Ident::new(name, Span::call_site());
                let emit_expr =
                    emit_expr(expr, context, trans_struct_name, imp_struct_name, bound_var);
                quote! {#field_name : #emit_expr}
            });
            quote! {
                #trans_struct_name {
                    #(#fields),*
                }
            }
        }
        Expr::Tuple(vec) => {
            let fields = vec.iter().map(|expr| {
                emit_expr(
                    expr,
                    context,
                    &trans_struct_name,
                    &imp_struct_name,
                    bound_var,
                )
            });
            quote! {
                (#(#fields),*)
            }
        }
        Expr::MapAccess(expr) => {
            let emit_inner = emit_expr(
                expr,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            quote! {
                step.map[&#emit_inner]
            }
        }
        Expr::NoneExpr => quote! {None},
        Expr::SomeExpr(expr) => {
            let emit_inner = emit_expr(
                expr,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            quote! {Some(#emit_inner)}
        }
        Expr::MapIterExpr {
            container: c,
            bound_var: bv,
            func: f,
        } => {
            let emit_container =
                emit_expr(c, context, &trans_struct_name, &imp_struct_name, bound_var);
            let var = syn::Ident::new(bv, Span::call_site());
            let emit_func = emit_expr(f, context, &trans_struct_name, &imp_struct_name, Some(bv));
            quote! {
                #emit_container.into_iter().map(|#var| #emit_func)
            }
        }
        Expr::ImplConstructorExpr(vec) => {
            let fields = vec.iter().map(|(name, expr)| {
                let field_name = syn::Ident::new(name, Span::call_site());
                let emit_expr =
                    emit_expr(expr, context, trans_struct_name, imp_struct_name, bound_var);
                quote! {#field_name : #emit_expr}
            });
            quote! {
                #imp_struct_name {
                    #(#fields),*
                }
            }
        }
        Expr::Ident(s) => {
            let var_name = syn::Ident::new(s, Span::call_site());
            match bound_var {
                Some(bv) if bv == s => quote! {#var_name},
                _ => quote! {#var_name},
            }
        }
        Expr::BinOp(op, expr, expr1) => {
            let left = emit_expr(
                expr,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let right = emit_expr(
                expr1,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            match op {
                BinOp::Equals => quote! {#left == #right},
                BinOp::Div => quote! {#left / #right},
                BinOp::Mult => quote! {#left * #right},
                BinOp::Plus => quote! {#left + #right},
                BinOp::Minus => quote! {#left - #right},
            }
        }
        Expr::IndexLiteral(i) => {
            let unsuffixed = syn::Index::from(*i);
            quote! {#unsuffixed}
        }
        Expr::EmptyVec => quote! { Vec::new() },
        Expr::FoldExpr {
            container,
            init,
            func,
        } => {
            let emit_container = emit_expr(
                container,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let emit_func = emit_expr(
                func,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let emit_init = emit_expr(
                init,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            quote! {
                #emit_container.into_iter().fold(#emit_init, |acc, x| #emit_func)
            }
        }
        Expr::Extend { vec1, vec2 } => {
            let emit_vec1 = emit_expr(
                vec1,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let emit_vec2 = emit_expr(
                vec2,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            quote! {
                extend_and_return(#emit_vec1, #emit_vec2)
            }
        }
        Expr::CallFunction { func, args } => {
            let func_identifer = syn::Ident::new(func, Span::call_site());
            let args = args.into_iter().map(|arg| {
                emit_expr(
                    arg,
                    context,
                    &trans_struct_name,
                    &imp_struct_name,
                    bound_var,
                )
            });
            quote! {
                #func_identifer(#(#args),*)
            }
        }
        Expr::OptionMatch {
            expr,
            some_arm,
            none_arm,
        } => {
            let expr = emit_expr(
                expr,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let emit_some_arm = emit_expr(
                some_arm,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let emit_none_arm = emit_expr(
                none_arm,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            quote! {
                match #expr {
                    Some(x) => #emit_some_arm,
                    None => #emit_none_arm
                }
            }
        }
        Expr::GetAnonData { ident, access } => {
            let field_name = emit_access_expr(
                access,
                context,
                trans_struct_name,
                imp_struct_name,
                bound_var,
            );
            let name = syn::Ident::new(ident, Span::call_site());
            quote! {
                #name.#field_name
            }
        }
        Expr::RangeExpr { bot, top } => {
            let left = emit_expr(
                bot,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            let right = emit_expr(
                top,
                context,
                &trans_struct_name,
                &imp_struct_name,
                bound_var,
            );
            quote! {
                (#left..#right)
            }
        }
    }
}

fn emit_access_chain(
    chain: &AccessChain,
    c: &Context,
    trans_struct_name: &Ident,
    imp_struct_name: &Ident,
    bound_var: Option<&str>,
) -> TokenStream {
    match chain {
        AccessChain::Nil => TokenStream::default(),
        AccessChain::TupleAccess(expr, rest_chain) => {
            let emit_right =
                emit_access_chain(rest_chain, c, trans_struct_name, imp_struct_name, bound_var);
            let emit_expr = emit_expr(expr, c, trans_struct_name, imp_struct_name, bound_var);
            quote! {.#emit_expr #emit_right}
        }
        AccessChain::ArrayAccess(expr, rest_chain) => {
            let emit_right =
                emit_access_chain(rest_chain, c, trans_struct_name, imp_struct_name, bound_var);
            let emit_expr = emit_expr(expr, c, trans_struct_name, imp_struct_name, bound_var);
            quote! {[#emit_expr] #emit_right}
        }
    }
}

fn emit_access_expr(
    a: &AccessExpr,
    c: &Context,
    trans_struct_name: &Ident,
    imp_struct_name: &Ident,
    bound_var: Option<&str>,
) -> TokenStream {
    match a {
        AccessExpr::Access(field, access_chain) => {
            let field_name = syn::Ident::new(field, Span::call_site());
            let access_chain_emitted = emit_access_chain(
                access_chain,
                c,
                trans_struct_name,
                imp_struct_name,
                bound_var,
            );
            quote! {#field_name #access_chain_emitted}
        }
    }
}

pub fn write_to_file(p: &ProblemDefinition, filename: &str) -> std::io::Result<()> {
    let path = Path::new(filename);
    let tmp_path = path.with_extension("tmp");

    // Stream raw code directly to a temp file first
    {
        emit_program(p, &tmp_path)?; // new version below
    }

    // Now pretty-format in a streaming way using `rustfmt` if available
    let rustfmt = std::process::Command::new("rustfmt")
        .arg("--edition")
        .arg("2021")
        .arg(&tmp_path)
        .status();

    match rustfmt {
        Ok(status) if status.success() => {
            std::fs::rename(&tmp_path, path)?;
        }
        _ => {
            eprintln!("warning: rustfmt failed, keeping raw output");
            std::fs::rename(&tmp_path, path)?;
        }
    }

    Ok(())
}