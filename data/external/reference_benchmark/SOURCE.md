# Internal validation data - do not include in shared deliverables

This folder holds a published external dataset used only to validate the
constrained-optimization engine (`R/18_constrained_optimization.R`) against
independently published results, before it is trusted on this project's own
data. It is a one-off benchmark, not a project input: nothing under
`data/external/` is read by `main.R`, `main_dcea.R`, or any shared output in
`output/`.

- `data/benchmark_dataset.xlsx`: a published health-benefit-package costing
  and effectiveness dataset (interventions, DALYs averted per case, cost per
  case, drug/commodities cost, health-worker time needs by cadre) from a
  peer-reviewed constrained-optimization study.
- `data/published_reference_results.csv`: that same study's own published
  headline results, used as the target `main_optimization_external_validation.R`
  reproduces.

Kept out of `output/` and out of anything sent externally - this is a private
engineering reference, not a citation to present to project partners.
