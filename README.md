# HBP Senegal – R Analysis Pipeline

Modular R pipeline for the analysis of Senegal's Health Benefits Package (HBP). Instead of one large script, the pipeline is split into small, single-purpose R scripts under `R/` that are all sourced from a single entry point (`main.R`). This keeps each stage (import, cleaning, effectiveness, cost, league table, export) isolated, testable, and easy to extend.

## Project structure

```
HBP_Senegal_R/
├── main.R                      # Entry point: sources all R/ scripts and runs the pipeline
├── config.R                     # Paths and parameters (edit this first)
├── R/
│   ├── 01_import.R              # Load only the Excel sheets actually used
│   ├── 02_cleaning.R            # Cleaning / harmonization + explicit column renames
│   ├── 03_effectiveness.R       # DALYs-averted/patient lookup (Tufts, with Uganda fallback)
│   ├── 04_costs.R               # Unit cost lookup (OHT, with manual override)
│   ├── 05_league_table.R        # The "machine": funnel + league table computation
│   ├── 06_indicators.R          # Utilities reading the ORIGINAL workbook's own results
│   └── 07_export.R              # Export tables and figures
├── data/
│   ├── raw/                     # Source .xlsx workbook(s) go here (git-ignored)
│   └── processed/               # Cleaned intermediate data (git-ignored)
└── output/
    ├── tables/                   # Exported tables (git-ignored)
    └── figures/                  # Exported figures (git-ignored)
```

Only `.gitkeep` placeholders are versioned in `data/` and `output/`; actual data and results stay local and are excluded via `.gitignore`.

## How it works

1. Place your Excel workbook(s) in `data/raw/` (or point `config_local.R` at a copy elsewhere on your machine, see below).
2. Run the pipeline from the project root:

```r
source("main.R")
```

or from the terminal:

```bash
Rscript main.R
```

This produces, in `output/tables/`:
- `league_table_rebuilt.csv` — the rebuilt league table (see below).
- `intervention_funnel_log.csv` — all 389 interventions, with the stage and reason any excluded one was excluded.
- `original_top_20_cost_effective.csv` / `original_burden_by_category.csv` — read directly from the original workbook's own `LeagueTable_Final` / `GBD_TIER3`, kept as a comparison baseline.

### Using a workbook stored outside the project

If your Excel workbook lives elsewhere on your machine, don't edit `raw_data_path` in `config.R` (that path is shared with everyone via git). Instead, create a `config_local.R` file at the project root — it is git-ignored and automatically sourced if present:

```r
config_local <- list(
  raw_data_path = "C:/Users/you/Documents/hbp_senegal_data.xlsx"
)
```

Only the fields you list are overridden; everything else keeps its default. Each collaborator can point to their own local copy without touching version-controlled files.

## Requirements

```r
install.packages(c("readxl", "dplyr", "janitor", "ggplot2"))
```

## The league table machine

`R/03_effectiveness.R`, `R/04_costs.R` and `R/05_league_table.R` rebuild `LeagueTable_Final` from first principles in R, instead of just re-reading the workbook's own pre-computed table. This was reverse-engineered from the workbook's actual Excel formulas (not guessed from the output), and validated cell-by-cell against the workbook's own cached results before being trusted (see *Validation* below).

### The funnel

```
389 interventions (OHT Case data, the master list)
  -> Stage 1: linked to a Top-20-DALY-burden GBD cause
              (present in "Senegal HBP Tool - Top20 Causes")            [141 pass]
  -> Stage 2: has an effectiveness figure (DALYs averted per patient)
  -> Stage 3: has a unit cost
  -> Stage 4: has demand data (a positive case volume)
  -> included in the league table, with cost/DALYs/ICER computed and ranked
```

`intervention_funnel_log.csv` has one row per one of the 389 interventions, with `stage_excluded` (`0_included` or the stage it fell out at) and a human-readable `reason_excluded` — so nothing is ever silently dropped, and you can see exactly what is missing for any given intervention.

### Effectiveness (DALYs averted per patient) — `R/03_effectiveness.R`

1. `id_Ratio` records the analyst's chosen Tufts article ID + ratio number for an intervention. **This is the manual input point**: to change which article/ratio an intervention uses, edit `id_Ratio` in the workbook and re-run the pipeline.
2. That pair is looked up in `Tufts_Ratios`. A ratio whose absolute value exceeds `config$tufts_ratio_plausibility_bound` (default 6) is discarded as implausible.
3. If no usable Tufts ratio exists — including when the intervention has no `id_Ratio` entry at all — fall back to `Uganda HBP Tool`'s own DALYs-averted-per-patient figure, matched via the name bridge in `OHT Int name mapping recent-old`.
4. Otherwise: `effectiveness_status = "Missing"`.

### Unit cost — `R/04_costs.R`

1. `Senegal HBP Tool - Top20 Causes`'s own "Cost if manual input" column. **This is the manual input point** for cost: fill it in for an intervention (in the workbook) to override or supply a cost, and re-run.
2. Otherwise, the unit cost already resolved in `OHT Drug supply costs` ("Final unit cost in $ per case in Senegal OHT").
3. Otherwise: `cost_status = "Missing"`.

### League table computation — `R/05_league_table.R`

For interventions with complete data: `total_cost_full = unit_cost × cases_full`, `total_dalys_full = dalys_per_patient × cases_full` (and the equivalent `_realistic` versions using the realistic-scale-up case volume), `icer_usd = total_cost_full / total_dalys_full`, and a net-benefit figure using `config$cet_usd_per_daly` (435, sourced from the workbook's `'Data sources'!B15` — update it in `config.R`, not by re-adding that sheet).

**Assumption to confirm**: the workbook's own "Ranking of NHP" column (kept here as `rank_nhp`) ranks by *ascending* net DALYs averted (full implementation) — the worst first, which is unusual for a priority list. `league_table_rebuilt.csv` is sorted by `icer_rank` instead (ascending ICER = best value for money first), the conventional league-table order; `rank_nhp` is kept for cross-checking against the original workbook. Confirm which ordering you actually want presented.

### Re-running after a change (the point of rebuilding this in R)

Because every run re-reads the workbook, any manual edit — a new `id_Ratio` selection, a filled-in "Cost if manual input", an added or removed intervention, a change to CET — takes effect on the next `Rscript main.R`, with no code change needed. Add or remove an intervention row and the funnel counts adjust automatically.

### Validation

Before being trusted, the rebuilt logic was checked cell-by-cell against the workbook's own cached formula results for `Senegal HBP Tool - Top20 Causes` (unit cost, cases, DALYs/patient, total cost, total DALYs, ICER): it matched exactly on 139 of 141 interventions. The 2 exceptions were a genuine finding, not a bug in the rebuild:

- **`PrEP`** and **`Management of other pregnancy complications`** both have a target population set in `OHT Case data` (9.2M and 686K respectively) but coverage/case volume currently at 0. The workbook's own `Senegal HBP Tool - Top20 Causes` still showed a large, non-zero cached total for these two — a stale formula result left over from before the case data was zeroed out (Excel had not been fully recalculated and saved since). The R pipeline always recomputes from the current source values, so it correctly excludes these two at Stage 4 rather than repeating the stale number. 43 interventions across the full 389 share this exact pattern (target population set, case data still pending) — `intervention_funnel_log.csv` flags each one individually with the reason *"Target population is set... but coverage/case volume is still 0"*, distinct from interventions that have no target at all.

- A first version of `R/03_effectiveness.R` started its lookup from `id_Ratio`'s own rows, which meant an intervention with no `id_Ratio` entry never got a chance at the Uganda fallback — even though the workbook's own formula (`BF4`) tries that fallback independently of `id_Ratio`. 18 Top-20 interventions have no `id_Ratio` entry, including TB and adult malaria treatment; 2 of them (`XDR-TB treatment`, `TPT for HIV-negative child household contacts`) do have a usable Uganda fallback and were being silently lost. Fixed: the lookup now starts from the full intervention list.

## Header rows

Most sheets have column names on row 1. A few have a title or a merged group-header row above the real header — `config$sheet_header_row` lists how many rows to skip for those (`Senegal HBP Tool - Top20 Causes`, `Initial draft Sen`, `IHME_DATA_ALL_AGE_FINAL`, `OHT Case data`, `OHT Intervention overview`, `OHT Drug supply costs`, `Uganda HBP Tool`). The three OHT sheets in that list have a two-row header (a merged group label over several sub-columns, e.g. "Coverage" over "Baseline (2023)"/"Target (2028)"); only the second row is kept as the column name.

## Column renames

Several sheets repeat the same header text across year blocks (e.g. "2023" appears at 5 different columns in `OHT Case data`), or have ambiguous auto-generated names (`ICER (in $)`, `DALYs`). `R/02_cleaning.R` renames the columns the pipeline reads by name — **by column position** (the exact Excel column letter, documented inline), not by the original header text, so a duplicate or awkward header never causes a silent mismatch. Add a rename function there, following the existing pattern, for any other sheet/column you start referencing by name.

## Source data: `hbp_senegal_data.xlsx`

The source workbook has 34 sheets. `config$sheets_to_load` loads 18: the ones used by the league table machine above, plus broader HBP Senegal context kept available for other analyses (burden of disease, insurance cartography, demographic/macroeconomic parameters). Left out: pure bibliographic reference (`Tufts_Methods`, `Methods - description`, `Ratios - description`, `References`), other-country comparator data not used by the machine (`Epi data - ML`, `Intervention costs` in Malawian Kwacha), irregular multi-country reference blocks (`Exchange rate`, `population data PPP other`), a small key-value summary (`Results - OHT & GBD`), a project-tracking note (`Data sources` — its one needed value, CET, is in `config.R` instead), the superseded `0_cartographie_raw`, and single-cell section-divider sheets. To load any of these anyway, add the sheet name to `config$sheets_to_load`.

## Adding a new pipeline step

1. Create a new file in `R/` (e.g. `08_new_step.R`).
2. Define functions only — no top-level code that runs on `source()`.
3. `source()` the new file from `main.R`, in the right order, and call its functions in the pipeline section.

## Design principles

- Each script in `R/` only defines functions; running the pipeline is `main.R`'s job.
- `config.R` is the single place for paths and parameters — no hard-coded paths elsewhere.
- Only the Excel sheets actually used are loaded, to keep imports fast and explicit.
- Every intervention is accounted for: nothing is filtered out without a logged, human-readable reason.
