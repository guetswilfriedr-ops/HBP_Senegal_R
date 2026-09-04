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

## Source data: `hbp_senegal_data.xlsx`

The source workbook has 34 sheets. `config$sheets_to_load` (in `config.R`) loads only the 16 that are structured, Senegal-specific tables directly useful to the pipeline, grouped as:

| Group | Sheets |
|---|---|
| HBP intervention list & prioritization | `SEN_Cartography_insur_scheme`, `Senegal HBP Tool - Top20 Causes`, `Initial draft Sen`, `LeagueTable_Final`, `id_Ratio`, `OHT - GBD`, `OHT Int name mapping recent-old` |
| Burden of disease (GBD/IHME) | `GBD_TIER3`, `IHME_DATA_ALL_AGE_FINAL` |
| Costing engine (OHT, 2023–2028) | `OHT Case data`, `OHT Intervention overview`, `OHT Avg medical personnel minut`, `OHT Delivery channels`, `OHT Drug supply costs` |
| Cost-effectiveness reference (DCP3) | `DCP3 - GBD` |
| Demographic & macroeconomic parameters | `Population`, `PPP` |

The remaining 18 sheets are left out of the default pipeline:

- **Superseded**: `0_cartographie_raw` (raw draft of `SEN_Cartography_insur_scheme`).
- **Bibliographic reference, not needed downstream**: `Tufts_Ratios`, `Tufts_Methods`, `Methods - description`, `Ratios - description`, `References` — the cost-effectiveness ratios they document are already resolved into `LeagueTable_Final` / `id_Ratio`.
- **Other-country comparator data** (Uganda/Zambia, Malawi — not Senegal): `Uganda HBP Tool`, `Epi data - ML`, `Intervention costs` (in Malawian Kwacha).
- **Irregular / multi-country reference blocks**: `Exchange rate`, `population data PPP other` (mixed World Bank tables, not Senegal-specific single tables).
- **Small key-value summary, not a data table**: `Results - OHT & GBD`.
- **Project tracking note, not analytical data**: `Data sources`.
- **Section dividers** (single-cell sheets used as tab separators): `IHME - GBD ->`, `OHT ->`, `Map intervention with GBD ->`, `Tufts Database ->`, `ML-UG extracts ->`, `World Bank data ->`.

To load any of these anyway, add the sheet name to `config$sheets_to_load`.

### Header rows

Most sheets have column names on row 1. A few have a title or a merged group-header row above the real header — `config$sheet_header_row` lists how many rows to skip for those (`Senegal HBP Tool - Top20 Causes`, `Initial draft Sen`, `IHME_DATA_ALL_AGE_FINAL`, `OHT Case data`, `OHT Intervention overview`, `OHT Drug supply costs`). The three OHT sheets in that list have a two-row header (a merged group label over several sub-columns, e.g. "Coverage" over "Baseline (2023)"/"Target (2028)"); only the second row is kept as the column name, so those columns may need a manual rename in `R/02_cleaning.R` once you inspect them — flagged there as a starting point, not a finished mapping.

### Column renames

`R/02_cleaning.R` renames a few columns with ambiguous auto-generated names (e.g. `ICER (in $)`, `DALYs`) to stable, predictable names right after import, for `LeagueTable_Final` and `GBD_TIER3` — the two sheets `R/03_indicators.R` reads by column name. Add a rename function there for any other sheet you start referencing by column name.

## Adding a new pipeline step

1. Create a new file in `R/` (e.g. `05_new_step.R`).
2. Define functions only — no top-level code that runs on `source()`.
3. `source()` the new file from `main.R`, in the right order, and call its functions in the pipeline section.

## Design principles

- Each script in `R/` only defines functions; running the pipeline is `main.R`'s job.
- `config.R` is the single place for paths and parameters — no hard-coded paths elsewhere.
- Only the Excel sheets actually used are loaded, to keep imports fast and explicit.
