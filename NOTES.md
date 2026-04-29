# Sydney Lockout Laws — Project Notes

## Project Overview

Estimates the causal effect of Sydney's lockout laws (introduced February 2014)
on suburb-level economic outcomes using synthetic control methods, followed by a
spatial equilibrium welfare analysis. Primary identification event is the February
2014 imposition. The 2020–2021 repeal is used as narrative corroboration only due
to COVID-19 contamination.

---

## R Setup

- **IDE:** RStudio Desktop
- **Pipeline:** `targets` package (`tar_make()` to run, `tar_read()` to inspect)
- **Project root:** `sydney-lockout-laws/` (contains `.Rproj` and `_targets.R`)
- **Key packages:** targets, tarchetypes, tidyverse, readxl, here, fs, Synth,
  augsynth, scales, ggrepel, kableExtra, R.utils

### File structure

```
sydney-lockout-laws/
├── _targets.R                  # Pipeline definition — edit here to add new targets
├── _targets/                   # targets cache — do not edit manually
├── data/
│   ├── raw/                    # Original source files, never modified
│   ├── processed/              # Outputs written by pipeline targets (.rds)
│   └── external/               # Shapefiles, concordance tables
├── R/
│   ├── utils.R                 # Shared helpers (state labels, treatment indicators, readers)
│   ├── ingest.R                # Employment ingestion (Excel + AURIN CSVs) and population
│   ├── ingest_census.R         # Census housing ingestion (rent, mortgage)
│   ├── eda.R                   # Indexed employment EDA charts
│   ├── sc_analysis.R           # Synthetic control estimation and figures (~1600 lines)
│   ├── housing_analysis.R      # Census rent/mortgage gap vs. synthetic control
│   └── att_weighted_summary.R  # Aggregate ATT table with base-period levels
├── outputs/
│   ├── figures/                # EDA charts (PDFs)
│   ├── sc/                     # SC estimation CSVs, figures/, tables/
│   ├── housing/                # Housing cost effect CSVs
│   └── att_summary/            # Aggregate ATT detail + summary CSVs
└── paper/
```

### Function → file map

| File | Functions |
|---|---|
| `R/utils.R` | `add_state_labels()`, `add_treatment_indicators()`, `read_abs_sheet()`, `read_aurin_industry_csv()`, `read_aurin_all_jobs_csv()` |
| `R/ingest.R` | `ingest_abs_employment()` (Excel), `ingest_abs_employment_csv()` (AURIN CSVs), `read_aurin_regional_pop()` |
| `R/ingest_census.R` | `ingest_abs_census_housing()` |
| `R/eda.R` | `plot_indexed_employment()`, `build_index_data()`, `draw_index_chart()` |
| `R/sc_analysis.R` | `build_sc_panel()`, `build_donor_pools()`, `run_synth_one()`, `run_placebo_tests()`, `run_sc_estimates()`, `run_sc_figures()`, `sc_run_estimates_target()`, `sc_run_figures_target()` |
| `R/housing_analysis.R` | `compute_housing_cost_effect()` |
| `R/att_weighted_summary.R` | `compute_att_weighted_summary()` |

### How to run

```r
targets::tar_make()          # Run pipeline (skips up-to-date targets)
targets::tar_read(target)    # Load a target into session
targets::tar_visnetwork()    # Visualise pipeline dependency graph
```

---

## Pipeline Targets

| Target | Function | Inputs | Output | Status |
|---|---|---|---|---|
| `raw_abs_employment` | `tar_file()` | `data/raw/61600_DO003_260221.xlsx` | file path | ✅ Done |
| `raw_sa2_delineation` | `tar_file()` | `data/raw/sa2_deliniation_file.csv` | file path | ✅ Done |
| `raw_regional_pop` | `tar_file()` | `data/raw/au-govt-abs-...-sa2-2016.csv` | file path | ✅ Done |
| `raw_census_housing` | `tar_file()` | `data/raw/au-govt-abs-...-family-...-sa2-2016.csv` | file path | ✅ Done |
| `raw_all_jobs_csv` | `tar_file()` | `data/raw/au-govt-abs-...-all-jobs-...-sa2-2016.csv` | file path | ✅ Done |
| `raw_industry_csv_2011`–`2018` | `tar_file()` | 8 per-year AURIN CSVs | file paths | ✅ Done |
| `emp_sa2_tidy` | `ingest_abs_employment()` | `raw_abs_employment` | tidy tibble (7 FYs) | ✅ Done |
| `emp_sa2_csv_tidy` | `ingest_abs_employment_csv()` | 8 `raw_industry_csv_*` files | tidy tibble (8 FYs) | ✅ Done |
| `all_jobs_sa2_tidy` | `read_aurin_all_jobs_csv()` | `raw_all_jobs_csv` | tidy tibble (by sex) | ✅ Done |
| `pop_sa2_tidy` | `read_aurin_regional_pop()` | `raw_regional_pop` | long ERP panel | ✅ Done |
| `census_housing_tidy` | `ingest_abs_census_housing()` | `raw_census_housing` | long rent/mortgage panel | ✅ Done |
| `eda_indexed_employment` | `plot_indexed_employment()` | `emp_sa2_csv_tidy` | PDF files | ✅ Done |
| `sc_estimates` | `sc_run_estimates_target()` | `emp_sa2_csv_tidy`, `pop_sa2_tidy`, delineation | CSVs in `outputs/sc/` | ✅ Done |
| `sc_figures` | `sc_run_figures_target()` | `sc_estimates` (upstream dep) | PDFs + tables in `outputs/sc/` | ✅ Done |
| `housing_cost_effect` | `compute_housing_cost_effect()` | `census_housing_tidy`, `emp_sa2_csv_tidy`, `pop_sa2_tidy`, SC weights | CSVs in `outputs/housing/` | ✅ Done |
| `att_weighted_summary_files` | `compute_att_weighted_summary()` | `sc_att_numeric.csv`, `emp`, `pop`, `census_housing` | CSVs in `outputs/att_summary/` | ✅ Done |

### Architecture note: sc_estimates / sc_figures split

Estimation and figure production are intentionally separate targets. SC estimation
is expensive (~30 min with placebos enabled). Figure production reads the CSVs
from `outputs/sc/` and is cheap. This means a charting bug or aesthetic change
never triggers re-estimation.

During development, set `run_placebos = FALSE` in the `sc_estimates` target
definition in `_targets.R` to skip the permutation step. Set to `TRUE` for the
final analysis run.

---

## Data Sources

### Two employment data paths

The pipeline ingests employment data from two independent sources that cover
overlapping but different time spans:

1. **ABS Excel workbook** (`61600_DO003_260221.xlsx`) → target `emp_sa2_tidy`
   - 7 fiscal years: 2011-12 to 2017-18
   - Read via `ingest_abs_employment()` using `readxl`
   - Uses 2011 ASGS SA2 boundaries

2. **AURIN per-year CSVs** (8 files, one per FY) → target `emp_sa2_csv_tidy`
   - 8 fiscal years: 2011-12 to **2018-19** (one extra year)
   - Read via `ingest_abs_employment_csv()` using `read.csv` by column position
   - Uses 2016 ASGS SA2 boundaries

**`emp_sa2_csv_tidy` is the primary source used by all downstream analysis**
(EDA, SC estimation, housing, ATT summary). The Excel-based `emp_sa2_tidy` was
the original prototype and is retained for cross-validation. Both produce the
same tidy schema.

### Raw files in `data/raw/`

| File | Source | Target | Contents |
|---|---|---|---|
| `61600_DO003_260221.xlsx` | ABS Jobs in Australia, Cat. 6160.0 | `emp_sa2_tidy` | Tables 3.8–3.14: jobs ('000) and median income by industry × SA2, 2011-12 to 2017-18 |
| 8 × `au-govt-abs-...-by-industry-sa2-YYYY-YY-...csv` | AURIN/ABS Jobs in Australia | `emp_sa2_csv_tidy` | Same content as Excel, one file per FY, 2011-12 to 2018-19 |
| `au-govt-abs-...-all-jobs-...-sa2-2016.csv` | AURIN/ABS Jobs in Australia | `all_jobs_sa2_tidy` | Total jobs and median income by sex × SA2, wide across years |
| `au-govt-abs-...-regional-population-...-sa2-2016.csv` | ABS Regional Population | `pop_sa2_tidy` | Estimated Resident Population by SA2, 2001–2021 |
| `au-govt-abs-...-family-and-community-...-sa2-2016.csv` | ABS Census 2011, 2016 | `census_housing_tidy` | Average monthly rent and mortgage payments by SA2 |
| `sa2_deliniation_file.csv` | ABS ASGS 2016 | `raw_sa2_delineation` | SA2 → SA3 → SA4 → GCCSA → State concordance (2310 SA2s) |

### Processed target schemas

**`emp_sa2_tidy` / `emp_sa2_csv_tidy`** — one row per SA2 × fiscal year × industry

Columns: `sa2_code`, `sa2_name`, `fiscal_year`, `industry`, `jobs_000`,
`median_income_dollars`, `year_start`, `state_code`, `state_name`,
`treated_full`, `treated_partial`, `treated`

- 20 industry categories including Total (exclude Total when summing across industries)
- `industry` is a factor ordered by ABS industry sequence
- `year_start` is integer (e.g. 2017 for "2017-18") for sorting/filtering
- `jobs_000` is in thousands (0.5 = 500 workers)
- State/territory label rows stripped; only rows with numeric SA2 codes retained

**`all_jobs_sa2_tidy`** — one row per SA2 × fiscal year × sex

Columns: `sa2_code`, `sa2_name`, `fiscal_year`, `year_start`, `sex`,
`jobs_000`, `median_income_dollars`, `state_code`, `state_name`,
`treated_full`, `treated_partial`, `treated`

- `sex` is a factor: persons, males, females

**`pop_sa2_tidy`** — one row per SA2 × calendar year

Columns: `sa2_code`, `sa2_name`, `state_code`, `state_name`, `gccsa_code`,
`year`, `population`, `treated_full`, `treated_partial`, `treated`

- `year` is calendar year (integer), 2001–2021
- `population` is raw count (Estimated Resident Population)

**`census_housing_tidy`** — one row per SA2 × payment_type × Census year

Columns: `sa2_code`, `sa2_name`, `year`, `payment_type`, `avg_monthly_payment`,
`state_code`, `state_name`, `treated_full`, `treated_partial`, `treated`

- `year` is 2011 or 2016 only (Census years with rent/mortgage data)
- `payment_type` is "Rent" or "Mortgage"
- `avg_monthly_payment` is in AUD

### Data still to acquire

- CoreLogic / PropTrack — monthly dwelling values by suburb
- NSW FACS Rent & Sales — median weekly rent (annual, SA2)
- ABS Business Register (ABSBR) — business counts by industry (SA2/SA3, annual)
- NSW Liquor & Gaming Authority — licensed venue register (annual, postcode/address)
- City of Sydney Council — pedestrian sensor data (hourly, street level)
- NSW BOCSAR — assault data by suburb (for external validation)

---

## Identification Strategy

- **Treatment:** February 2014 imposition of lockout laws
- **Treated area:** CBD entertainment precinct — Kings Cross, Darlinghurst, Cockle
  Bay, The Rocks, Haymarket, parts of Surry Hills (SA2 boundary crosswalk needed)
- **Analysis window:** 2011–2012 pre-treatment / 2013–2018 post-treatment
  (year_start values; 2013 is the first partially treated FY)
- **COVID period (2020–2022):** excluded from causal analysis
- **Repeal (2020–2021):** narrative corroboration only

### Treated SA2s

| SA2 code | SA2 name | Treatment status |
|---|---|---|
| 117031337 | Sydney - Haymarket - The Rocks | Full |
| 117031329 | Darlinghurst | Full |
| 117031333 | Potts Point - Woolloomooloo | Full |
| 117031336 | Surry Hills | Partial |
| 117031334 | Pyrmont - Ultimo | Partial |

Defined in `utils.R` → `add_treatment_indicators()`. The same codes work for
both 2011 and 2016 ASGS boundaries (unchanged for these SA2s).

### Spillover SA2s

Untreated Sydney suburbs expected to absorb displaced late-night patronage.
Defined in `sc_analysis.R` → `SPILLOVER_SA2S` and mirrored in `eda.R` →
`spillover_eda`. **These must stay in sync manually.**

| SA2 code | SA2 name | SA4 |
|---|---|---|
| 117031332 | Newtown - Camperdown - Darlington | 117 City and Inner South |
| 117031331 | Glebe - Forest Lodge | 117 |
| 117031335 | Redfern - Chippendale | 117 |
| 117031338 | Waterloo - Beaconsfield | 117 |
| 117031330 | Erskineville - Alexandria | 117 |
| 120021388 | Leichhardt - Annandale | 120 Inner West |
| 120021387 | Balmain | 120 |
| 120021389 | Lilyfield - Rozelle | 120 |
| 118011345 | Paddington - Moore Park | 118 Eastern Suburbs |
| 118011341 | Bondi Junction - Waverly | 118 |

### Donor pools

Three pools are run for every treated/spillover unit × outcome combination:

| Pool ID | Label | Description |
|---|---|---|
| P1 | All Australia (exc. Greater Sydney) | Broadest pool; all SA2s outside Greater Sydney GCCSA (1GSYD) |
| P2 | All Australia exc. NSW | Eliminates all NSW SA2s to remove within-state spillover risk |
| P3 | Capital cities only (exc. Greater Sydney) | Restricts to 7 non-Sydney capital city GCCSAs (Melbourne, Brisbane, Adelaide, Perth, Hobart, Darwin, ACT) |

Pool membership is determined using the SA2 delineation file's `GCCSA_CODE_2016`
field, with a fallback heuristic based on SA4 codes for any SA2 missing from the
lookup. Fortitude Valley (305011106) is NA'd from 2016 onward due to QLD lockout
laws.

### Key methodological decisions

- Primary estimator: ADH Synthetic Control (Abadie, Diamond, Hainmueller 2010)
  via the `Synth` package
- Augmented SC (Ben-Michael et al. 2021) via `augsynth` available but not
  currently used as the primary estimator
- Inference: placebo-in-space permutation tests; exclude placebos with
  MSPE ratio > 5× the treated unit's pre-MSPE
- Base year for indexing: 2013 (year_start), i.e. 2012-13 FY (last full
  pre-treatment year; index = 100)
- Pre-treatment matching years: 2011, 2012 (year_start)
- Post-treatment years: 2013–2018 (year_start); 2013 is partially treated
- Minimum employment filter: 500 workers (0.5 in jobs_000) in 2012-13 for
  treated SA2 × industry combinations; below-threshold combos are skipped
- Median income SC estimated for Total, Retail trade, Accommodation and food
  services, Arts and recreation services (indexed as "Income: <industry>")
- Population indexed as a pseudo-industry ("Population") using ERP data
- Per-SC-run timeout: 120 seconds
- The Star casino exempt from lockout laws — exclude its catchment from donor
  pool; run separate robustness test
- SA2 boundaries do not align with regulatory precinct — manual classification
  of fully vs. partially treated suburbs; sensitivity tests excluding partial cases

### Industry coverage by unit role

- **Treated SA2s:** all 20 ABS industry divisions + Total + Population + 4
  income outcomes (subject to the 500-worker filter)
- **Spillover SA2s:** TARGET_INDUSTRIES only (Total, Accommodation and food
  services, Arts and recreation services, Retail trade, Population) + 4 income
  outcomes
- **Donor SA2s:** TARGET_INDUSTRIES only (keeps the panel lean)

---

## Output Files

### `outputs/sc/`

| File pattern | Contents |
|---|---|
| `sc_weights_P{1,2,3}.csv` | Donor SA2 weights per treated/spillover unit × industry |
| `sc_gaps_P{1,2,3}.csv` | Year-by-year treated vs. synthetic index and gap series |
| `sc_placebo_P{1,2,3}.csv` | Placebo gap series (only if `run_placebos = TRUE`) |
| `sc_att_summary.csv` | ATT estimates across all unit × industry × pool combos |
| `tables/sc_att_numeric.csv` | Numeric ATT table (pool_id, treated_code, industry, att, pre_mspe, n_donors) |
| `tables/sc_att_summary_formatted.csv` | Human-readable ATT table with significance stars |
| `figures/sc_gap_*.pdf` | Gap charts per treated/spillover SA2 |
| `figures/sc_placebo_*.pdf` | Placebo fan charts |
| `figures/sc_weights_*.pdf` | Lollipop charts of top-10 donor weights |

### `outputs/figures/`

| File | Contents |
|---|---|
| `eda_index_all_industries_combined.pdf` | Indexed employment chart: all industries |
| `eda_index_accommodation_and_food_services.pdf` | Indexed employment chart: accommodation/food |
| `eda_index_arts_and_recreation_services.pdf` | Indexed employment chart: arts/recreation |
| `eda_index_retail_trade.pdf` | Indexed employment chart: retail |

### `outputs/housing/`

| File | Contents |
|---|---|
| `housing_effect_by_sa2.csv` | Rent/mortgage % change gap per SA2 × pool |
| `housing_effect_weighted_summary.csv` | Weighted average gaps (by employment and population weights) |

### `outputs/att_summary/`

| File | Contents |
|---|---|
| `att_detail_by_sa2.csv` | Per-SA2 ATT with base-period levels, % change, raw change (employment, income, population, rent) |
| `att_aggregate_summary.csv` | Weighted aggregate ATT by pool × unit_role × outcome × industry |

---

## Welfare Analysis Framework

Spatial equilibrium model (Roback 1982, Moretti 2011, Kline & Moretti 2014).
Welfare decomposed into four components:

1. Consumer surplus loss (late-night amenity access)
2. Producer surplus loss (venue closures, revenue decline)
3. Labour market effects (employment and wages in hospitality/arts)
4. Housing market capitalisation (rents and dwelling values)

Netted against welfare gain from violence reduction, monetised using BOCSAR (2015)
assault estimates (26% reduction in lockout area, 32% in Kings Cross) and health
economics unit cost per assault (~AUD 85,000).

The `att_weighted_summary` target computes the aggregate ATT in levels
(base-period employment × SC-estimated % change = jobs lost/gained) for input
into the welfare decomposition. Additive quantities (employment, population) are
summed; non-additive quantities (median income, rent) are weighted-averaged
using base-period employment or population as weights.

---

## Robustness Tests Planned

- Placebo in time (fake treatment 2011–2012)
- Placebo in space (each control suburb as treated) — **implemented** via
  `run_placebo_tests()`, controlled by `run_placebos` flag
- Leave-one-out donor pool sensitivity
- Restrict to fully-treated suburbs only
- Non-Sydney NSW suburbs as intermediate donor pool
- Exclude The Star catchment
- Spatial RD at precinct boundary
- Cross-validate SC economic estimates against BOCSAR assault data

---

## Technical Notes

- `read_abs_sheet()` takes four arguments: `file_path`, `sheet_name`,
  `fiscal_year`, `col_names` — all passed explicitly from `ingest_abs_employment()`
- `pmap()` call uses `\(sheet, fiscal_year)` anonymous function syntax to avoid
  `..1`/`..2` context warnings
- OneDrive Files On-Demand should be disabled or project moved outside OneDrive
  to avoid interference with `_targets/` cache
- `read_aurin_industry_csv()` selects columns by position (3–44) because income
  column names are inconsistently abbreviated across years. It expects exactly
  45 columns per file; a warning fires if the count differs.
- `build_sc_panel()` recomputes "Total" employment by summing all non-Total
  industries rather than using the ABS Total column directly. This avoids
  double-counting issues.
- Population is appended to the SC panel as a pseudo-industry named "Population",
  converted to jobs_000 units (population / 1000) for index compatibility.
- Median income is appended as pseudo-industries named "Income: <industry>",
  converted to jobs_000 units (income / 1000) for index compatibility.
- The `census_housing_tidy` target depends on a CSV with extremely long column
  names (e.g. `rnt_mrtgge_pymnts_cnss_avrge_mnthly_hshld_rntl_pymnt`). These
  are renamed to `avg_monthly_rent` and `avg_monthly_mortgage` at ingestion.
- SC estimation uses a 120-second timeout per unit via `R.utils::withTimeout()`.
- Placebo tests are capped at 100 (configurable via `max_placebos`); if the donor
  pool exceeds this, a random sample is drawn with `set.seed(42)`.
- SA2 codes that must stay in sync across files:
  - Treated SA2s: `utils.R` (`add_treatment_indicators`) ↔ `sc_analysis.R`
    (`FULL_TREATED_SA2S`, `PARTIAL_TREATED_SA2S`)
  - Spillover SA2s: `eda.R` (`spillover_eda`) ↔ `sc_analysis.R` (`SPILLOVER_SA2S`)
  - Anecdotal controls: `eda.R` (`anecdotal_controls`) — EDA only, not used in SC

### Development tips

- `tar_make()` runs the full pipeline. Use `tar_make(names = c(target_name))`
  to run a single target and its upstream dependencies.
- Set `run_placebos = FALSE` in the `sc_estimates` target in `_targets.R` to
  skip permutation tests during development (~30 min savings).
- `tar_read(emp_sa2_csv_tidy)` loads the primary employment tibble into session
  for interactive exploration.
- `tar_visnetwork()` draws the dependency graph — useful after adding targets.

---

## Session Log

| Date | What was done |
|---|---|
| 2026-03-30 | Project setup: targets pipeline, folder structure, R/utils.R, R/ingest.R, _targets.R. emp_sa2_tidy target running successfully. |
| 2026-03–04 | Added AURIN per-year CSV ingestion (`ingest_abs_employment_csv`), AURIN all-jobs CSV reader, regional population reader. Added SA2 delineation file for GCCSA-based donor pool construction. |
| 2026-03–04 | Built EDA charts: indexed employment for all industries, accommodation/food, arts/recreation, retail. Added spillover SA2 series to charts. |
| 2026-03–04 | Full SC estimation pipeline: `build_sc_panel()`, `build_donor_pools()`, `run_synth_one()`, `run_placebo_tests()`, three donor pools (P1/P2/P3), treated + spillover SA2 loops, per-pool CSV outputs. Split into `sc_estimates` and `sc_figures` targets. |
| 2026-03–04 | Added median income and population as SC outcomes. Added 500-worker minimum filter for treated SA2 × industry combinations. |
| 2026-03–04 | Census housing ingestion (`ingest_abs_census_housing`). Housing cost effect analysis comparing rent/mortgage % change (2011→2016) between treated/spillover units and their synthetic controls. |
| 2026-03–04 | Aggregate ATT summary (`att_weighted_summary`): base-period levels, % change, raw change for employment, income, population, rent. Additive vs. non-additive aggregation logic. |
