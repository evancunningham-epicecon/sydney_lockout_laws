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
- **Key packages:** targets, tarchetypes, tidyverse, readxl, here, fs

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
│   ├── utils.R                 # Shared helper functions (read_abs_sheet)
│   └── ingest.R                # ingest_abs_employment() function
├── scripts/                    # One-off or exploratory scripts (not part of pipeline)
├── outputs/
│   ├── figures/
│   ├── tables/
│   └── robustness/
└── paper/
```

### How to run

```r
targets::tar_make()          # Run pipeline (skips up-to-date targets)
targets::tar_read(target)    # Load a target into session
targets::tar_visnetwork()    # Visualise pipeline dependency graph
```

---

## Pipeline Status

| Target | Function | Input | Output | Status |
|---|---|---|---|---|
| `raw_abs_employment` | `tar_file()` | `data/raw/61600_DO003_260221.xlsx` | file path | ✅ Done |
| `emp_sa2_tidy` | `ingest_abs_employment()` | `raw_abs_employment` | tidy tibble | ✅ Done |
| `sa2_panel` | TBD | `emp_sa2_tidy` + other sources | analysis panel | ⬜ Next |
| `sc_results` | TBD | `sa2_panel` | SC estimates | ⬜ Pending |
| `welfare_estimates` | TBD | `sc_results` | welfare decomposition | ⬜ Pending |

---

## Data

### Raw files in `data/raw/`

| File | Source | Contents |
|---|---|---|
| `61600_DO003_260221.xlsx` | ABS Jobs in Australia, Cat. 6160.0, released 26 Feb 2021 | Tables 3.8–3.14: employee jobs ('000) and median employee income per job ($) by industry division and SA2, 2011-12 to 2017-18 |

### Processed targets

**`emp_sa2_tidy`** — one row per SA2 × fiscal year × industry division

Columns: `sa2_code`, `sa2_name`, `fiscal_year`, `industry`, `jobs_000`,
`median_income_dollars`, `year_start`

- 20 industry categories including Total (exclude Total when summing across industries)
- `industry` is a factor ordered by ABS industry sequence
- `year_start` is integer (e.g. 2017 for "2017-18") for sorting/filtering
- Fiscal years covered: 2011-12 to 2017-18 (7 years)
- State/territory label rows stripped; only rows with numeric SA2 codes retained

### Data still to acquire

- ABS Census 2011, 2016, 2021 — employment, wages, rents, dwelling counts (SA2)
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
- **Analysis window:** 2010–2013 pre-treatment / 2014–2019 post-treatment
- **COVID period (2020–2022):** excluded from causal analysis
- **Repeal (2020–2021):** narrative corroboration only

### Donor pool

Interstate inner-urban, entertainment-oriented SA2s only. Sydney suburbs excluded
due to demand spillovers, business/labour relocation, and general equilibrium
effects. Sydney suburbs used in spillover analysis only.

| City | Candidate suburbs |
|---|---|
| Melbourne | Fitzroy, Collingwood, Richmond, St Kilda |
| Brisbane | Fortitude Valley*, West End, New Farm |
| Adelaide | Hindmarsh, Bowden, Norwood |
| Perth | Northbridge, Leederville, Mount Lawley |

*Fortitude Valley excluded from donor pool post-2016 (Queensland lockout laws)

### Key methodological decisions

- Primary estimator: Augmented Synthetic Control (Ben-Michael et al. 2021)
- Benchmark: standard ADH (Abadie, Diamond, Hainmueller 2010)
- Inference: permutation/placebo tests; exclude controls with MSPE ratio > 5×
- The Star casino exempt from lockout laws — exclude its catchment from donor pool;
  run separate robustness test
- SA2 boundaries do not align with regulatory precinct — manual classification of
  fully vs. partially treated suburbs; sensitivity tests excluding partial cases
- NSW GSP growth and unemployment rate included as SC matching covariates to absorb
  state-level macroeconomic noise

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

---

## Robustness Tests Planned

- Placebo in time (fake treatment 2011–2012)
- Placebo in space (each control suburb as treated)
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

---

## Session Log

| Date | What was done |
|---|---|
| 2026-03-30 | Project setup: targets pipeline, folder structure, R/utils.R, R/ingest.R, _targets.R. emp_sa2_tidy target running successfully. |
