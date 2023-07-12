# acbench
Algorithm Configuration Benchmarks


## Adding a new benchmark

1. Add the source code to `algorithms/` or a script `get.sh` that downloads (and possibly patches) the source code.

2. Add `parameters.txt`, `target-runner`, and (optionally) `default.txt`.

3. Instances

4. Scenario

5. Extend the setup script `setup` to handle the algorithm and the instances.
   The setup script is called with the name of the scenario.
   


