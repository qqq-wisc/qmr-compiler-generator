# Demo Commands
This file walks through the basic usage of Amaro with some example commands.
We will interact with Amaro using the script named `amaro.` This script provides an easy interface for the system, but it's really just running some basic `cargo` commands (which you can see by inspecting its contents).
# NISQMR 
To compile the nisq program run

`./amaro compile problem-descriptions/nisq.qmrl `

This adds an executable to the `generated-solvers` directory. Invoke it directly with

`./generated-solvers/nisq`

If everything went as expected, this should print 

``Usage: amaro run <circuit> <graph> --<solve-mode>``

since we didn't provide the three required arguments.

Now let's use the `amaro` script to run on a simple circuit and the IBM 127 qubit architecture. The command below will search for a good mapping and routing solution until the timeout (1hr by default) or user interrupt. You can let it run for a few seconds and then interrupt with `Ctrl+C.`

`./amaro run nisq demo/3_17_13.qasm demo/arch.json --amaro > out.json`

# SCMR
We can follow the same build process with SCMR
```
./amaro compile problem-descriptions/scmr.qmrl
./amaro run scmr demo/3_17_13.qasm demo/arch_scmr.json --amaro
```