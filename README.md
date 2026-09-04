# HBP Senegal – R Analysis Pipeline

Modular R pipeline for the analysis of Senegal's Health Benefits Package (HBP). Instead of one large script, the pipeline is split into small, single-purpose R scripts under `R/` that are all sourced from a single entry point (`main.R`). This keeps each stage (import, cleaning, indicators, export) isolated, testable, and easy to extend.

## Project structure

```
HBP_Senegal_R/
├── main.R                  # Entry point: sources all R/ scripts and runs the pipeline
├── config.R                 # Paths and parameters (edit this first)
├── R/
│   ├── 01_import.R          # Load only the Excel sheets actually used
│   ├── 02_cleaning.R        # Cleaning / harmonization functions
│   ├── 03_indicators.R      # HBP indicator calculations
│   └── 04_export.R          # Export tables and figures
├── data/
│   ├── raw/                 # Source .xlsx workbook(s) go here (git-ignored)
│   └── processed/           # Cleaned intermediate data (git-ignored)
└── output/
    ├── tables/               # Exported tables (git-ignored)
    └── figures/              # Exported figures (git-ignored)
```

Only `.gitkeep` placeholders are versioned in `data/` and `output/`; actual data and results stay local and are excluded via `.gitignore`.

## How it works

1. Place your Excel workbook(s) in `data/raw/`.
2. Edit `config.R`:
   - `raw_data_path`: path to the workbook.
   - `sheets_to_load`: list **only** the sheet names actually used by the analysis. `01_import.R` reads exactly those sheets, not the entire workbook.
3. Run the pipeline from the project root:

```r
source("main.R")
```

or from the terminal:

```bash
Rscript main.R
```

## Requirements

```r
install.packages(c("readxl", "dplyr", "janitor", "ggplot2"))
```

## Adding a new pipeline step

1. Create a new file in `R/` (e.g. `05_new_step.R`).
2. Define functions only — no top-level code that runs on `source()`.
3. `source()` the new file from `main.R`, in the right order, and call its functions in the pipeline section.

## Design principles

- Each script in `R/` only defines functions; running the pipeline is `main.R`'s job.
- `config.R` is the single place for paths and parameters — no hard-coded paths elsewhere.
- Only the Excel sheets actually used are loaded, to keep imports fast and explicit.
