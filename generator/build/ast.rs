#[derive(Debug)]
pub struct ProblemDefinition {
    pub imp: ImplBlock,
    pub trans: TransitionBlock,
    pub arch: Option<ArchitectureBlock>,
    pub step: Option<StepBlock>,
}
#[derive(Debug)]
pub enum GateType {
    CX,
    T,
    Pauli,
}
#[derive(Debug)]
pub struct ImplBlock {
    pub routed_gates: Vec<GateType>,
    pub data: NamedTuple,
    pub realize: Expr,
}

#[derive(Debug)]
pub struct StepBlock {
    pub cost: Expr,
}

#[derive(Debug)]
pub struct ArchitectureBlock {
    pub data: NamedTuple,
    pub get_locations: Option<Expr>,
}

#[derive(Debug)]
pub struct TransitionBlock {
    pub data: NamedTuple,
    pub apply: Expr,
    pub cost: Expr,
    pub get_transitions: Expr,
}
#[derive(Debug)]
pub struct NamedTuple {
    pub name: String,
    pub fields: Vec<(String, Ty)>,
}
#[derive(Debug)]
pub enum Ty {
    LocationTy,
    IntTy,
    FloatTy,
    TupleTy(Vec<Ty>),
    VectorTy(Box<Ty>),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Expr {
    FloatLiteral(f64),
    LocationLiteral(usize),
    IndexLiteral(usize),
    Ident(String),
    EmptyVec,

    Tuple(Vec<Expr>),

    SomeExpr(Box<Expr>),
    NoneExpr,

    SwapPair(Box<Expr>, Box<Expr>),
    GetData {
        d: DataType,
        access: AccessExpr,
    },
    GetAnonData {
        ident: String,
        access: AccessExpr,
    },
    CallMethod {
        d: DataType,
        method: String,
        args: Vec<Expr>,
    },

    CallFunction {
        func: String,
        args: Vec<Expr>,
    },
    ITE {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    },
    LetExpr {
        ident: String,
        bound : Box<Expr>,
        body : Box<Expr> 
    },
    OptionMatch {
        expr: Box<Expr>,
        some_arm: Box<Expr>,
        none_arm: Box<Expr>,
    },

    MapIterExpr {
        container: Box<Expr>,
        bound_var: String,
        func: Box<Expr>,
    },
    FoldExpr {
        container: Box<Expr>,
        init: Box<Expr>,
        func: Box<Expr>,
    },

    Append {
        vec: Box<Expr>,
        elem: Box<Expr>,
    },
    Extend {
        vec1: Box<Expr>,
        vec2: Box<Expr>,
    },

    RangeExpr {
        bot: Box<Expr>,
        top: Box<Expr>,
    },

    TransitionConstructor(Vec<(String, Expr)>),
    ImplConstructorExpr(Vec<(String, Expr)>),

    MapAccess(Box<Expr>),

    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum BinOp {
    Equals,
    Div,
    Mult,
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum AccessExpr {
    Access(String, Box<AccessChain>),
}
#[derive(PartialEq, PartialOrd, Debug)]
pub enum AccessChain {
    Nil,
    TupleAccess(Box<Expr>, Box<AccessChain>),
    ArrayAccess(Box<Expr>, Box<AccessChain>),
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum DataType {
    Arch,
    Transition,
    Step,
    Impl,
    Gate,
}

pub enum Context {
    DataTypeContext(DataType),
    Free,
}
