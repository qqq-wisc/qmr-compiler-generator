*This writeup is not final*
# MAROL 
**MAROL** is a toolkit designed for generating compilers for different quantum architectures. **MAROL** supplies a language for specifying quantum architectures based on Rust.

# Dependencies
MAROL requires **Cargo** 1.90.0, **Python** 3.8 and a bash shell. 


# Installation
Clone the repo with 
```
git clone https://github.com/qqq-wisc/qmr-compiler-generator
```
and build with
```
cargo build
```

# Usage 
To compile a `.qmrl` file run
```
./qmrl compile $FILE
```
where `$FILE` is the relative path to your MAROL file ending in `.qmrl`. The compiled program will be at `generated-solvers/$BASE` where `$BASE` is the filename without the `.qmrl` ending.

To debug a `.qmrl` file run 
```
./qmrl debug $FILE
```
where `$FILE` is the relative path to your MAROL file ending in `.qmrl`. The debug output will be at `generator/debug`

To run a `.qmrl` run
```
./qmrl run $FILE <circuit> <graph> --<solve-mode>
```
where `$FILE` is the relative path to your MAROL file ending in `.qmrl`. `<circuit>` expects a `.qasm` file, `<graph>` expects a JSON file decribing the shape of the architecture, and `--<solve-mode>` expects one of the following:
* `--sabre`: SABRE search algorithm from [\[2\]](#references).
* `--onepass`: ??
* `--joint_optimize-par`: MaxState algorithm from [\[1\]](#references).

# Writing MAROL
This section is a guide to defining a MAROL problem description. Let's look at `problem-descriptions/nisq.qmrl`:
```
GateRealization[
    routed_gates = CX
    name = 'NisqCnot'
    data = (u : Location, v : Location)
    realize_gate = if Arch.contains_edge((Step.map[Gate.qubits[0]],Step.map[Gate.qubits[1]]))
            then Some(GateRealization{u = Step.map[Gate.qubits[0]],v = Step.map[Gate.qubits[1]]})
            else None
]

Transition[
    name = 'Swap'
    data = (edge : (Location,Location))
    get_transitions = (map(|x| -> Transition{ edge = x}, Arch.edges())).push(Transition{edge = (Location(0),Location(0))})
    apply = value_swap(Transition.edge.(0), Transition.edge.(1))
    cost = if (Transition.edge)==(Location(0), Location(0))
            then 0.0
            else 1.0
]
```

TODO: figure out what's going on with steps and states, my understanding is that they were the same thing, but I believe that in the paper they use `State.map`

The two most basic components of a MAROL file are
    1. `GateRealization`: A gate realization is the way a specific gate needs to be routed on a given architecture. In the above see that in order to realize a CX gate in NISQ, the routed gate must be adjacent. A more interesting example of gate realization in the SCMR archicature is given after this.
    2. `Transition`: A transition is the operation between steps, which are like atomic "pieces" of mapping and routing.  Transitions are generally expensive and in the above example represent a swap between adjacent qubits in the architecture, as in NISQ CX gates need to occur between edge-adjacent qubits. Because of their cost, a cost function is used to measure how expensive a transition is for a given architecture.

MAROL in fact mostly uses a functional paradigm. So, in the above, the attributes `GateRealization.realize_gate`, `Transition.apply` and `Transition.cost` are in fact functions, and take implicit parameters, for example `Location`. 

Now let's look at `problem-descriptions/scmr.qmrl`:
```
GateRealization[
    routed_gates = CX, T
    name='ScmrGate'
    data= (path : Vec<Location>)
    realize_gate =  
    if (Gate.gate_type()) == CX 
        then 
            map(|x| -> GateRealization{path = x}, 
            all_paths(arch, vertical_neighbors(Step.map[Gate.qubits[0]], Arch.width, Arch.height), 
            horizontal_neighbors(Step.map[Gate.qubits[1]], arch.width), 
            ((values(Step.map())).extend(Arch.magic_state_qubits()))
            .extend(fold(Vec(), |x, acc| -> acc.extend(x), 
            map(|x| -> x.implementation.(path()), Step.implemented_gates())))))
        else   
            map(|x| -> GateRealization{path = x}, 
                all_paths(arch, 
                          vertical_neighbors(Step.map[Gate.qubits[0]], Arch.width, Arch.height), 
                          fold(Vec(), |x, acc| -> acc.extend(x), 
                          map(|x| -> horizontal_neighbors(x, arch.width), Arch.magic_state_qubits())), 
                          ((values(Step.map()))
                          .extend(Arch.magic_state_qubits()))
                          .extend(fold(Vec(), |x, acc| -> acc.extend(x),
                           map(|x| -> x.implementation.(path()), Step.implemented_gates())))))
 ]

Transition[
    name = 'Id'
    data = (na : Location)
    get_transitions = (Vec()).push(Transition{na=Location(0)})
    apply = identity_application(step)
    cost = 0.0

]

Architecture[
    name = 'ScmrArch'
    data = (magic_state_qubits : Vec<Location>, alg_qubits : Vec<Location>, width : Int, height : Int)
    get_locations = Arch.alg_qubits()
]

Step[
    cost = 1.0
]
```
Here we see two more top level components, `Architecture` and `Step`. `Architecture` allows us to specify more information about the architecture, for instance in SCMR that's where the magic qubits, used for T gates, and where the other remaining algorithmic qubits are. `Step` allows us to assign a cost to each step, as in SCMR a step itself is more expensive than the transition between steps, where as in NISQ it is the opposite. Here the `GateRealization` portion is much more interesting than the previous example. We can specify multiple gates with `routed_gates` and check the implicit paramater `Gate`'s type with `.gate_type()`. 
# Packages and Files
* `generator`: Contains the main code that is ran when you call `.qmrl`.
  * `src/main.rs`: Turns the `.qmrl` file into rust code. The line `include!(concat!(env!("OUT_DIR"), "/custom.rs"));` specifically adds `custom.rs` to `main.rs`, once compiled it uses MAROL description to solve the given mapping and routing problem.
  * `build`: Parses the MAROL file and turns it into Rust code.
    * `ast.rs`: Defines some data structures for parsing the MAROL code.
    * `main.rs`: Parses the MAROL description, emits the tokens as rust into `custom.rs`.
    * `emit.rs`: Emits a list of parsed tokens from a MAROL file description as rust code.
    * `parse.rs`: Parses and interprets the MAROL file.
* `solver`: This code does the main heavy lifting for solving the mapping and routing problems based on the MAROL description.
  * `src/backend.rs`: Here are the functions for solving for solving the mapping and routing problem based on the `--<solve-mode>`.
  * `src/config.rs`: Configurations for the solver.
  * `src/lib.rs`: Describes the modules for the solver.
  * `src/structures.rs`: Defines some data structures for the solver.
  * `src/utils.rs`: Utility functions for the solver.
* `generated-solvers`: This directory will hold any generated compilers.
* `problem_descriptions`: This directory has some example MAROL files.
* `builtin`: Contains some example tests from page 3 from the paper [\[1\]](#references).


# Notes
Depending on what version of Python you have and how it is installed, you may need to add an alias to your bash config file, which should be at either `~/.bashrc`, `~/.bash\_profile` or `~/.zshrc`

# References 
MAROL is primarily based on the following paper:

[1] Abtin Molavi, Amanda Xu, Ethan Cecchetti, Swamit Tannu, Aws Albarghouthi. "[Generating Compilers for Qubit Mapping and Routing](https://arxiv.org/pdf/2508.10781)" 

The codebase also implements the SABRE algorithm proposed by the following paper:

[2] Gushu Li, Yufei Ding, Yuan Xie. "[Tackling the Qubit Mapping Problem for NISQ-Era Quantum Devices](https://arxiv.org/abs/1809.02573)"

