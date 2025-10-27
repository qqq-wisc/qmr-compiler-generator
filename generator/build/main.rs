mod ast;
mod emit;
mod parse;
use std::{env, path::Path, vec};

use ast::*;
use emit::write_to_file;


fn rss_kb() -> usize {
    // Linux: read VmRSS from /proc
    let contents = std::fs::read_to_string("/proc/self/status").unwrap_or_default();
    for line in contents.lines() {
        if let Some(val) = line.strip_prefix("VmRSS:") {
            // "VmRSS:\t  123456 kB"
            return val.trim().split_whitespace().next().unwrap_or("0").parse().unwrap_or(0);
        }
    }
    0
}
macro_rules! mem {
    ($label:expr) => {{
        println!("cargo:warning=[mem] {:>24}: {:>10} KB", $label, rss_kb());
    }};
}

fn test_program() -> ProblemDefinition {
    ProblemDefinition {
        imp: ImplBlock {
            routed_gates : vec![GateType::CX],
            data: NamedTuple {
                name: "NisqGateImplementation".to_string(),
                fields: vec![
                    ("u".to_string(), Ty::LocationTy),
                    ("v".to_string(), Ty::LocationTy),
                ],
            },
            realize: Expr::ITE {
                cond: Box::new(Expr::CallMethod {
                    d: DataType::Arch,
                    method: "contains_edge".to_string(),
                    args: vec![Expr::Tuple(vec![
                        Expr::MapAccess(Box::new(Expr::GetData {
                            d: DataType::Gate,
                            access: AccessExpr::Access(
                                "qubits".to_string(),
                                Box::new(AccessChain::ArrayAccess(
                                    Box::new(Expr::IndexLiteral(0)),
                                    Box::new(AccessChain::Nil),
                                )),
                            ),
                        })),
                        Expr::MapAccess(Box::new(Expr::GetData {
                            d: DataType::Gate,
                            access: AccessExpr::Access(
                                "qubits".to_string(),
                                Box::new(AccessChain::ArrayAccess(
                                    Box::new(Expr::IndexLiteral(1)),
                                    Box::new(AccessChain::Nil),
                                )),
                            ),
                        })),
                    ])],
                }),
                then: Box::new(Expr::SomeExpr(Box::new(Expr::ImplConstructorExpr(vec![
                    (
                        "u".to_string(),
                        Expr::MapAccess(Box::new(Expr::GetData {
                            d: DataType::Gate,
                            access: AccessExpr::Access(
                                "qubits".to_string(),
                                Box::new(AccessChain::ArrayAccess(
                                    Box::new(Expr::IndexLiteral(0)),
                                    Box::new(AccessChain::Nil),
                                )),
                            ),
                        })),
                    ),
                    (
                        "v".to_string(),
                        Expr::MapAccess(Box::new(Expr::GetData {
                            d: DataType::Gate,
                            access: AccessExpr::Access(
                                "qubits".to_string(),
                                Box::new(AccessChain::ArrayAccess(
                                    Box::new(Expr::IndexLiteral(0)),
                                    Box::new(AccessChain::Nil),
                                )),
                            ),
                        })),
                    ),
                ])))),
                els: Box::new(Expr::NoneExpr),
            },
        },
        trans: TransitionBlock {
            data: NamedTuple {
                name: "Swap".to_string(),
                fields: vec![(
                    "edge".to_string(),
                    Ty::TupleTy(vec![Ty::LocationTy, Ty::LocationTy]),
                )],
            },
            apply: Expr::SwapPair(
                Box::new(Expr::GetData {
                    d: DataType::Transition,
                    access: AccessExpr::Access(
                        "edge".to_string(),
                        Box::new(AccessChain::TupleAccess(
                            Box::new(Expr::IndexLiteral(0)),
                            Box::new(AccessChain::Nil),
                        )),
                    ),
                }),
                Box::new(Expr::GetData {
                    d: DataType::Transition,
                    access: AccessExpr::Access(
                        "edge".to_string(),
                        Box::new(AccessChain::TupleAccess(
                            Box::new(Expr::IndexLiteral(1)),
                            Box::new(AccessChain::Nil),
                        )),
                    ),
                }),
            ),
            cost: Expr::ITE {
                cond: Box::new(Expr::BinOp(
                    BinOp::Equals,
                    Box::new(Expr::GetData {

                        d: DataType::Transition,
                        access: AccessExpr::Access("edge".to_string(), Box::new(AccessChain::Nil)),
                    }),
                    Box::new(Expr::Tuple(vec![Expr::Tuple(vec![
                        Expr::LocationLiteral(0),
                        Expr::LocationLiteral(0),
                    ])])),
                )),
                then: Box::new(Expr::FloatLiteral(0f64)),
                els: Box::new(Expr::FloatLiteral(1f64)),
            },
            get_transitions: Expr::Append {
                vec: Box::new(Expr::MapIterExpr {
                    container: Box::new(Expr::CallMethod {
                        d: DataType::Arch,
                        method: "edges".to_string(),
                        args: vec![],
                    }),
                    bound_var: "x".to_string(),
                    func: Box::new(Expr::TransitionConstructor(vec![(
                        "edge".to_string(),
                        Expr::Ident("x".to_string()),
                    )])),
                }),
                elem: Box::new(Expr::TransitionConstructor(vec![(
                    "edge".to_string(),
                    Expr::Tuple(vec![Expr::LocationLiteral(0), Expr::LocationLiteral(0)]),
                )])),
            },
        },
        arch: None,
        step: None,
    }
}

fn from_file() {
    let path = env::var("QMRL_PATH").unwrap_or("/home/abtin/qmrsl/qmrl/problem-descriptions/nisq.qmrl".to_string());
    let p = parse::read_file(&path);
    mem!("after chumsky parse");
    let ast = format!("{:?}", p);
    let _ = std::fs::write("debug", ast.as_bytes());
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("custom.rs");
    write_to_file(&p, dest_path.to_str().unwrap());
    mem!("after writing file");
}

fn main() {
    mem!("initial");
    from_file();
}
