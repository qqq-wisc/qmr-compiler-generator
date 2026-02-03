use solver::utils;
include!(concat!(env!("OUT_DIR"), "/custom.rs"));
fn run_custom(circ_path: &str, graph_path: &str, solve_mode: &str) {
    let circ = utils::extract_gates(circ_path, GATE_TYPES);
    let arch = CustomArch::from_file(graph_path);
    let res = match solve_mode {
        "--sabre" => my_sabre_solve(&circ, &arch),
        "--onepass" => my_solve(&circ, &arch),
        "--amaro" => my_joint_solve_parallel(&circ, &arch),
        _ => panic!("Unrecognized solve mode"),
    };
    match serde_json::to_writer(std::io::stdout(), &res) {
        Ok(_) => (),
        Err(e) => panic!("Error writing compilation to stdout: {}", e),
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 4 {
        println!("Usage: amaro run <circuit> <graph> --<solve-mode>");
        return;
    }
    run_custom(&args[1], &args[2], &args[3]);
}
