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
* `--sabre`: What do these do
* `--onepass`:
* `--joint_optimize-par`: 

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
The two most basic components of a MAROL file are
(1) `GateRealization`:
(2) `Transition
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
* `builtin`: Contains some example tests from page 3 from [the paper](#references).


# Notes
Depending on what version of Python you have and how it is installed, you may need to add an alias to your bash config file, which should be at either `~/.bashrc`, `~/.bash\_profile` or `~/.zshrc`

# References 
MAROL is based on the following paper:

[1] Abtin Molavi, Amanda Xu, Ethan Cecchetti, Swamit Tannu, Aws Albarghouthi. "[Generating Compilers for Qubit Mapping and Routing](https://arxiv.org/pdf/2508.10781)" 

