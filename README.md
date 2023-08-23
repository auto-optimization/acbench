# acbench
Algorithm Configuration Benchmarks

## Running

 1. Edit `01-run_scenario.R` to select the appropriate setup file and run it with `Rscript 01-run_scenario.R`
 2. Once all the experiments have finished, edit `02-collect_train_results.R` to select the appropriate setup file and run it.
 3. Run `03-run_test.R`
 4. Once all the experiments have finished, run `04-collect_test_results.R`
 5. Run `05-analyse_results.R` to generate a report.
 

## Adding a new benchmark

 1. Add a new scenario to `scenarios/`

 2. Add the source code to `algorithms/` or add a function to `setups.R` that downloads and compiles the code.

 3. Add `parameters.txt`, `target-runner`, and (optionally) `default.txt` to `algorithms/`

 3. Add problem instances under `instances/` or add a function to `setups.R` that downloads the instances.
 
 4. If you wish to run on a cluster, create a template similar to `launch_sge.tmpl`
 
   


