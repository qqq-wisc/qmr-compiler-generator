use crate::ast::*;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

pub fn emit_program(p: &ProblemDefinition) -> TokenStream {
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
    let define_gate_types = emit_gate_types(&p.imp.routed_gates);
    let define_gi_struct = emit_define_struct(&p.imp.data);
    let define_arch_struct = emit_define_arch_struct(&p.arch);
    let define_transition_struct = emit_define_struct(&p.trans.data);
    let implement_gi_trait = emit_impl_gate(&p.imp.data);
    let implement_gi_getters = emit_impl_gate_methods(&p.imp.data);
    let implement_arch_trait = emit_impl_arch(&p.arch);
    let implement_arch_methods = emit_impl_arch_methods(&p.arch);
    let implement_trans_trait = emit_impl_trans(&p.trans, &p.imp);
    let define_available_transitions = emit_available_transitions(&p.trans, &p.imp);
    let define_realize_gate_function = emit_realize_gate_function(&p.imp);
    let define_solve_function = emit_solve_function(&p.imp);
    let define_sabre_solve_function = emit_sabre_solve_function(&p.imp);
    let define_joint_solve_parallel_function = emit_joint_optimize_parallel_function(&p.imp);
    let define_step_cost = emit_step_cost(&p);
    let define_mapping_heuristic = emit_mapping_heuristic();
    quote! {
        #use_statements
        #define_gate_types
        #define_gi_struct
        #define_arch_struct
        #define_transition_struct
        #implement_gi_trait
        #implement_gi_getters
        #implement_arch_trait
        #implement_arch_methods
        #implement_trans_trait
        #define_available_transitions
        #define_realize_gate_function
        #define_step_cost
        #define_mapping_heuristic
        #define_solve_function
        #define_sabre_solve_function
        #define_joint_solve_parallel_function


    }
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
        Expr::RangeExpr { bot, top } => contains_subexpr(bot, subexpr) || contains_subexpr(top, subexpr),
        Expr::LetExpr { ident, bound, body } => contains_subexpr(bound, subexpr) || contains_subexpr(body, subexpr),
    }
}

fn emit_define_struct(data: &NamedTuple) -> TokenStream {
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
    quote! {
        #[derive(Hash, PartialEq, Eq, Clone, serde::Serialize, Debug)]
        pub struct #struct_name {
            #(#fields),*
        }
    }
}

fn emit_gate_type(g: &GateType) -> TokenStream {
    match g {
        GateType::CX => quote! {"CX"},
        GateType::T => quote! {"T"},
        GateType::Pauli => quote! {"Pauli"},
    }
}

fn emit_gate_types(routed_gates: &Vec<GateType>) -> TokenStream {
    let gate_types = routed_gates.iter().map(|g| emit_gate_type(g));
    quote! {const GATE_TYPES : &[&str] = &[#(#gate_types),*];}
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

fn emit_define_arch_struct(arch: &Option<ArchitectureBlock>) -> TokenStream {
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
    quote! {
            #[derive(Clone)]
            struct #arch_name {
                graph: Graph<Location, ()>,
                index_map: HashMap<Location, NodeIndex>,
                #extra_fields_quote

            }
    }
}
fn emit_impl_gate(imp_data: &NamedTuple) -> TokenStream {
    let struct_name = syn::Ident::new(&imp_data.name, Span::call_site());
    quote! {impl GateImplementation for CustomRealization {}}
}

fn emit_impl_gate_methods(imp_data: &NamedTuple) -> TokenStream {
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
    quote! {
        impl CustomRealization {
            #(#getters)*
        }
    }
}

fn emit_impl_arch(arch: &Option<ArchitectureBlock>) -> TokenStream {
    let arch_name = syn::Ident::new("CustomArch", Span::call_site());
    let body = match arch {
        Some(ArchitectureBlock {
            data: d,
            get_locations: Some(expr),
        }) => {
            let get_locations = emit_expr(
                expr,
                &Context::DataTypeContext(DataType::Arch),
                &arch_name,
                &arch_name,
                None,
            );
            quote! {
                return #get_locations;
            }
        }
        _ => {
            quote! {
                    let mut locations = Vec::new();
                    for node in self.graph.node_indices() {
                        locations.push(self.graph[node]);
                    }
                    return locations;
            }
        }
    };
    return quote! {

    impl Architecture for #arch_name {
        fn locations(&self) -> Vec<Location>{
            #body
        }

        fn graph(&self) -> (Graph<Location, ()>, HashMap<Location, NodeIndex>) {
            return (self.graph.clone(), self.index_map.clone());
        }
    }};
}

fn emit_impl_arch_methods(arch: &Option<ArchitectureBlock>) -> TokenStream {
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
    return quote! {
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
}

fn emit_impl_trans(t: &TransitionBlock, imp: &ImplBlock) -> TokenStream {
    let trans_struct_name = syn::Ident::new(&t.data.name, Span::call_site());
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    let apply_expr = emit_expr(
        &t.apply,
        &Context::DataTypeContext(DataType::Transition),
        &trans_struct_name,
        &imp_struct_name,
        None,
    );
    let cost_expr = emit_expr(
        &t.cost,
        &Context::DataTypeContext(DataType::Transition),
        &trans_struct_name,
        &imp_struct_name,
        None,
    );
    quote! {
        impl Transition<CustomRealization, CustomArch> for CustomTransition {
            fn apply(&self, step: &Step<CustomRealization>) -> Step<CustomRealization> {
               #apply_expr
            }
            fn repr(&self) -> String {
                return format!("{:?}", self);
            }

            fn cost(&self, arch :& CustomArch) -> f64 {
                #cost_expr
            }
        }


    }
}

fn emit_available_transitions(t: &TransitionBlock, imp: &ImplBlock) -> TokenStream {
    let trans_struct_name = syn::Ident::new(&t.data.name, Span::call_site());
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    let available_trans_expr = emit_expr(
        &t.get_transitions,
        &Context::Free,
        &trans_struct_name,
        &imp_struct_name,
        None,
    );
    let trans_struct_name = syn::Ident::new(&t.data.name, Span::call_site());
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    quote! {
            fn available_transitions(arch : &CustomArch, step : &Step<CustomRealization>) -> Vec<CustomTransition> {
               #available_trans_expr
            }
    }
}

fn emit_realize_gate_function(imp: &ImplBlock) -> TokenStream {
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    let realize_gate_expr = emit_expr(
        &imp.realize,
        &Context::Free,
        &imp_struct_name,
        &imp_struct_name,
        None,
    );
    quote! {
        fn realize_gate(
            step: &Step<CustomRealization>,
            arch: &CustomArch,
            gate: &Gate,
        ) -> impl IntoIterator<Item = CustomRealization> {
            #realize_gate_expr
        }
    }
}

fn emit_mapping_heuristic() -> TokenStream {
    quote! {
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

    }
}

fn emit_step_cost(p: &ProblemDefinition) -> TokenStream {
    let imp_struct_name = syn::Ident::new(&p.imp.data.name, Span::call_site());
    let trans_struct_name = syn::Ident::new(&p.trans.data.name, Span::call_site());
    let step_cost_body = match &p.step {
        Some(s) => {
            let step_cost_expr = emit_expr(
                &s.cost,
                &Context::Free,
                &trans_struct_name,
                &imp_struct_name,
                None,
            );
            quote! {
                return #step_cost_expr;
            }
        }
        None => {
            quote! {
                return 0.0;
            }
        }
    };
    quote! {
        fn custom_step_cost(step: &Step<CustomRealization>, arch: &CustomArch) -> f64 {
        #step_cost_body
        }
    }
}

fn emit_solve_function(imp: &ImplBlock) -> TokenStream {
    let sub_expr = Expr::CallMethod {
        d: DataType::Step,
        method: "implemented_gates".to_string(),
        args: vec![],
    };
    let explore_orders = contains_subexpr(&imp.realize, &sub_expr);
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    quote! {
        fn my_solve(c : &Circuit, a : &CustomArch) -> CompilerResult<CustomRealization> {
            return backend::solve(c, a, &|s| available_transitions(a, s), &realize_gate, custom_step_cost, Some(mapping_heuristic), #explore_orders);
    }
    }
}

fn emit_sabre_solve_function(imp: &ImplBlock) -> TokenStream {
    let sub_expr = Expr::CallMethod {
        d: DataType::Step,
        method: "implemented_gates".to_string(),
        args: vec![],
    };
    let explore_orders = contains_subexpr(&imp.realize, &sub_expr);
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    quote! {
        fn my_sabre_solve(c : &Circuit, a : &CustomArch) -> CompilerResult<CustomRealization> {
            return backend::sabre_solve(c, a, &|s| available_transitions(a, s), &realize_gate, custom_step_cost, Some(mapping_heuristic), #explore_orders);
    }
    }
}

fn emit_joint_optimize_parallel_function(imp: &ImplBlock) -> TokenStream {
    let sub_expr = Expr::CallMethod {
        d: DataType::Step,
        method: "implemented_gates".to_string(),
        args: vec![],
    };
    let explore_orders = contains_subexpr(&imp.realize, &sub_expr);
    let imp_struct_name = syn::Ident::new(&imp.data.name, Span::call_site());
    quote! {
        fn my_joint_solve_parallel(c : &Circuit, a : &CustomArch) -> CompilerResult<CustomRealization> {
            return backend::solve_joint_optimize_parallel(c, a, &|s| available_transitions(a, s), &realize_gate, custom_step_cost, Some(mapping_heuristic), #explore_orders);
    }
    }
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
                    CustomTransition {
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
                let var  = syn::Ident::new(bv, Span::call_site());
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
                    CustomRealization {
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
                match op{
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
        Expr::RangeExpr { bot, top } => 
        {
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
        },
        Expr::LetExpr { ident,  bound, body } => {
            let right = emit_expr(bound, context, trans_struct_name, imp_struct_name, bound_var);
            let body_quote =  emit_expr(body, context, trans_struct_name, imp_struct_name, bound_var);
            let name = syn::Ident::new(ident, Span::call_site());
            quote! {let #name = #right; #body_quote}
        },
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

pub fn write_to_file(p: &ProblemDefinition, filename: &str) {
    let s = emit_program(p);
    let raw_str = s.to_string();
    let parse_res = syn::parse2(s);
    let formatted = match parse_res {
        Ok(syntax_tree) => prettyplease::unparse(&syntax_tree),
        Err(e) => {
            eprintln!("Error: {:?}", e);
            raw_str
        }
    };
    let _ = std::fs::write(filename, formatted.as_bytes());
}