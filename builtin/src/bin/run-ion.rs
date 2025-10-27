use builtin::ion::{ion_solve, ion_solve_joint_optimize_parallel, ion_solve_joint_optimize_parallel_no_opt, IonArch};
use serde_json;
use solver::utils::{self, IOError};

fn run_ion(circ_path: &str, width_arg: &str, solve_mode: &str) -> Result<(), IOError> {
    let circ = utils::extract_gates(circ_path, &["CX"]);
    let width = width_arg.parse().expect("width arg should be usize");
    let trap_size = circ.qubits.len().div_ceil(2*width).max(2);
    let arch = IonArch {
        trap_size,
        width
    };
    let res = match solve_mode {
        "--onepass" => Ok(ion_solve(&circ, &arch)),
        "--joint-optimize-par" => Ok(ion_solve_joint_optimize_parallel(&circ, &arch)),
        "--joint-optimize-par-no-opt" => Ok(ion_solve_joint_optimize_parallel_no_opt(&circ, &arch)),
        _ => Err(IOError::InputErr),
    }?;
    serde_json::to_writer(std::io::stdout(), &res).map_err(IOError::OutputErr)
}

fn main() -> Result<(), IOError> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 4 {
        println!("Usage: run-ilq <circuit> <trap-size> <mode>");
    }
    run_ion(&args[1], &args[2], &args[3])
}
