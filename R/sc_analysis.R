# =============================================================================
# R/sc_analysis.R
# Synthetic Control Analysis — Sydney Lockout Laws
#
# Estimates the causal effect of the February 2014 lockout laws on indexed
# employment (total and by key industry) for each treated SA2 individually.
#
# Three donor pools are run for every treated unit × outcome combination:
#   P1  All Australia         (exc. all Greater Sydney SA4s listed below)
#   P2  All Australia exc. NSW
#   P3  Capital cities only   (GCCSA delineation from sa2_deliniation.csv,
#                              exc. Greater Sydney)
#
# Sydney SA4s excluded from ALL pools
# ------------------------------------
# The exclusion covers the full Greater Sydney GCCSA (SA4 codes 101–127 within
# NSW), PLUS the following eight additional SA4s which were named explicitly
# for exclusion due to demand-spillover risk:
#   117  Sydney - City and Inner South
#   118  Sydney - Eastern Suburbs
#   119  Sydney - Inner South West
#   120  Sydney - Inner West
#   121  Sydney - North Sydney and Hornsby
#   122  Sydney - Northern Beaches
#   125  Sydney - Parramatta
#   126  Sydney - Ryde
# These eight codes all fall within the existing Greater Sydney SA4 range
# (101–127) and are therefore already captured by the broad exclusion.
# They are listed explicitly here for documentation clarity.
#
# Capital city GCCSA codes (from sa2_deliniation.csv GCCSA_CODE_2016 field)
# -------------------------------------------------------------------------
#   1GSYD  Greater Sydney     — EXCLUDED (treated region)
#   2GMEL  Greater Melbourne
#   3GBRI  Greater Brisbane
#   4GADE  Greater Adelaide
#   5GPER  Greater Perth
#   6GHOB  Greater Hobart
#   7GDAR  Greater Darwin
#   8ACTE  Australian Capital Territory
#
# Placebo-in-space permutation tests produce empirical p-values and
# "margin of error" bands (5th–95th percentile of placebo gaps).
#
# Outputs written to outputs/sc/:
#   sc_weights_<pool>.csv           — donor weights per treated unit × outcome
#   sc_gaps_<pool>.csv              — treated vs synthetic gap series
#   sc_placebo_<pool>.csv           — placebo gap distribution
#   sc_att_summary.csv              — ATT table across all combinations
#   figures/sc_gap_<...>.pdf        — gap charts per treated SA2
#   figures/sc_placebo_<...>.pdf    — placebo fan charts
#   figures/sc_weights_<pool>.pdf   — weight tables
#   tables/sc_att_*.csv             — formatted and numeric ATT tables
#
# Dependencies: tidyverse, Synth (ADH 2010), augsynth (Ben-Michael et al. 2021)
#   Install if needed:
#     install.packages("Synth")
#     install.packages("augsynth")   # or: remotes::install_github("ebenmichael/augsynth")
#
# Targets pipeline separation
# ---------------------------
# Estimation and figure production are intentionally split into two functions
# and two pipeline targets so that a charting error does not require re-running
# the computationally expensive SC estimation:
#
#   tar_target(sc_estimates, sc_run_estimates_target(...), format = "file")
#   tar_target(sc_figures,   sc_run_figures_target(...),  format = "file")
#
# See Section 7 and _targets.R for the full snippet.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(Synth)
  library(augsynth)    # Ben-Michael et al. 2021 ASCM
  library(scales)
  library(ggrepel)
  library(kableExtra)
  library(fs)
})

# =============================================================================
# 0. CONFIGURATION
# =============================================================================

# Pre-treatment base year for indexing (last full pre-treatment FY)
BASE_YEAR       <- 2013L

# Treatment onset: 2013-14 FY is the first (partially) treated year
TREATMENT_YEAR  <- 2013L   # year_start value for 2013-14 FY

# Pre-treatment years used for SC matching (fully pre-treatment)
PRE_YEARS       <- 2011L:2012L

# All analysis years
ALL_YEARS       <- 2011L:2018L

# MSPE ratio threshold for excluding controls from placebo inference
MSPE_THRESHOLD  <- 5

# Minimum number of observations a donor SA2 must have across all years
MIN_OBS         <- 6L

# NSW state code
NSW_STATE_CODE <- 1L

# Greater Sydney GCCSA code in the delineation file
GREATER_SYDNEY_GCCSA <- "1GSYD"

# Capital city GCCSA codes (exc. Greater Sydney, which is always excluded)
CAPITAL_CITY_GCCSA_CODES <- c(
  "2GMEL",  # Greater Melbourne
  "3GBRI",  # Greater Brisbane
  "4GADE",  # Greater Adelaide
  "5GPER",  # Greater Perth
  "6GHOB",  # Greater Hobart
  "7GDAR",  # Greater Darwin
  "8ACTE"   # Australian Capital Territory
)

# Treated SA2 definitions (from utils.R)
FULL_TREATED_SA2S <- c(
  "117031337",  # Sydney - Haymarket - The Rocks
  "117031329",  # Darlinghurst
  "117031333"   # Potts Point - Woolloomooloo
)

PARTIAL_TREATED_SA2S <- c(
  "117031336",  # Surry Hills
  "117031334"   # Pyrmont - Ultimo
)

ALL_TREATED_SA2S <- c(FULL_TREATED_SA2S, PARTIAL_TREATED_SA2S)

# Core industries — used for spillover SA2s and as the reporting focus for
# treated SA2s. Also used as the industry set for donor pool panel construction
# (keeps the donor panel lean).
TARGET_INDUSTRIES <- c(
  "Total",
  "Accommodation and food services",
  "Arts and recreation services",
  "Retail trade",
  "Population"
)

# All 20 ABS industry divisions — used for treated SA2s only (sanity check
# across the full industry distribution).
ALL_ABS_INDUSTRIES <- c(
  "Total",
  "Population",
  "Agriculture, forestry and fishing",
  "Mining",
  "Manufacturing",
  "Electricity, gas, water and waste services",
  "Construction",
  "Wholesale trade",
  "Retail trade",
  "Accommodation and food services",
  "Transport, postal and warehousing",
  "Information media and telecommunications",
  "Finance and insurance services",
  "Rental, hiring and real estate services",
  "Professional, scientific and technical services",
  "Administrative and support services",
  "Public administration and safety",
  "Education and training",
  "Health care and social assistance",
  "Arts and recreation services",
  "Other services"
)

# Minimum employment threshold (jobs_000 units) applied to treated SA2s in
# 2012-13 (year_start == 2012, last fully pre-treatment year). Any treated
# SA2 × industry combination below this threshold is skipped: the SC
# estimator is unreliable when baseline employment is negligible, and the
# economic significance of any gap would be trivial.
# 500 workers = 0.5 in jobs_000 units.
MIN_JOBS_2012 <- 0.5

# Industries for which median income SC is estimated.
INCOME_INDUSTRIES <- c(
  "Total",
  "Retail trade",
  "Accommodation and food services",
  "Arts and recreation services"
)

# Spillover SA2s — untreated Sydney suburbs expected to absorb displaced
# late-night patronage from the lockout precinct. Selected from SA4s 117
# (City and Inner South) and 120 (Inner West) as the highest-priority
# spillover zones, plus two from SA4 118 (Eastern Suburbs / Oxford St
# corridor). These are run against TARGET_INDUSTRIES only.
#
# SA4 117 — City and Inner South (direct adjacency):
#   117031332  Newtown - Camperdown - Darlington   (most cited spillover destination)
#   117031331  Glebe - Forest Lodge                (adjacent to Pyrmont/Ultimo)
#   117031335  Redfern - Chippendale               (adjacent to Surry Hills)
#   117031338  Waterloo - Beaconsfield             (adjacent to Surry Hills)
#   117031330  Erskineville - Alexandria           (inner south corridor)
#
# SA4 120 — Inner West (easy-to-reach, high NTE activity):
#   120021388  Leichhardt - Annandale
#   120021387  Balmain
#   120021389  Lilyfield - Rozelle
#
# SA4 118 — Eastern Suburbs (Oxford St / Paddington corridor):
#   118011345  Paddington - Moore Park             (Oxford St borders precinct)
#   118011341  Bondi Junction - Waverly
SPILLOVER_SA2S <- c(
  "117031332",  # Newtown - Camperdown - Darlington
  "117031331",  # Glebe - Forest Lodge
  "117031335",  # Redfern - Chippendale
  "117031338",  # Waterloo - Beaconsfield
  "117031330",  # Erskineville - Alexandria
  "120021388",  # Leichhardt - Annandale
  "120021387",  # Balmain
  "120021389",  # Lilyfield - Rozelle
  "118011345",  # Paddington - Moore Park
  "118011341"   # Bondi Junction - Waverly
)

POOL_LABELS <- c(
  P1 = "All Australia (exc. Greater Sydney)",
  P2 = "All Australia exc. NSW",
  P3 = "Capital cities only (exc. Greater Sydney)"
)

OUT_DIR <- "outputs/sc"

# =============================================================================
# 1. DELINEATION FILE LOADER
# =============================================================================

#' load_sa2_gccsa_lookup
#' Reads the SA2 delineation CSV and returns a named character vector mapping
#' sa2_code (as character) → gccsa_code.
#'
#' The delineation file path is relative to the project root.
#' Pass delineation_path explicitly when calling from a non-standard working dir.
#'
#' @param delineation_path  path to sa2_deliniation.csv (2016 ASGS boundaries)
#' @return named character vector: names = sa2_code (9-digit, zero-padded),
#'         values = GCCSA_CODE_2016 (e.g. "1GSYD", "2GMEL")
load_sa2_gccsa_lookup <- function(
    delineation_path = "data/raw/sa2_deliniation_file.csv") {
  
  delineation <- read_csv(
    delineation_path,
    col_types = cols(
      SA2_MAINCODE_2016 = col_character(),
      GCCSA_CODE_2016   = col_character(),
      .default          = col_character()
    ),
    show_col_types = FALSE
  )
  
  lookup <- delineation$GCCSA_CODE_2016
  names(lookup) <- delineation$SA2_MAINCODE_2016
  lookup
}

# =============================================================================
# 2. DONOR POOL CONSTRUCTION
# =============================================================================

#' is_greater_sydney
#' Returns TRUE for SA2s in the Greater Sydney GCCSA.
#' Uses the GCCSA lookup derived from the delineation file when available;
#' falls back to the SA4-range heuristic if an SA2 code is absent from the
#' lookup (e.g. data vintage mismatch between employment data and delineation).
#'
#' The following Sydney SA4 codes are explicitly excluded:
#'   117 City and Inner South  118 Eastern Suburbs     119 Inner South West
#'   120 Inner West            121 North Sydney/Hornsby 122 Northern Beaches
#'   125 Parramatta            126 Ryde
#' All eight fall within the existing Greater Sydney SA4 range (101–127) and
#' are therefore captured by the broad heuristic. They are listed here for
#' documentation clarity.
#'
#' @param sa2_code   character vector of 9-digit SA2 codes
#' @param state_code integer vector of state codes (1 = NSW)
#' @param gccsa_lookup named character vector from load_sa2_gccsa_lookup()
#'                     (NULL to use SA4-range heuristic only)
#' @return logical vector
is_greater_sydney <- function(sa2_code, state_code, gccsa_lookup = NULL) {
  
  # SA4-range heuristic: Greater Sydney SA4s are 101–127 within NSW.
  # SA2 codes are 9 digits structured as S|AA|BB|CCCC where S = state,
  # AA = SA4-within-state (zero-padded to 2 digits), etc.
  # Digits 2–3 of the SA2 code give the SA4 number within the state.
  sa4_within_state <- as.integer(str_sub(sa2_code, 2, 3))
  heuristic_match  <- state_code == NSW_STATE_CODE & sa4_within_state %in% 1L:27L
  
  if (is.null(gccsa_lookup)) {
    return(heuristic_match)
  }
  
  # GCCSA-lookup match (preferred when the code is in the lookup)
  gccsa_val       <- gccsa_lookup[sa2_code]
  in_lookup       <- !is.na(gccsa_val)
  lookup_match    <- in_lookup & gccsa_val == GREATER_SYDNEY_GCCSA
  
  # Use lookup result where available; fall back to heuristic where not
  if_else(in_lookup, lookup_match, heuristic_match)
}

#' is_capital_city_gccsa
#' Returns TRUE for SA2s in any Australian capital city GCCSA
#' (excluding Greater Sydney, which is always excluded upstream).
#' Uses the GCCSA lookup from the delineation file.
#'
#' @param sa2_code     character vector
#' @param gccsa_lookup named character vector from load_sa2_gccsa_lookup()
#' @return logical vector
is_capital_city_gccsa <- function(sa2_code, gccsa_lookup) {
  gccsa_val <- gccsa_lookup[sa2_code]
  !is.na(gccsa_val) & gccsa_val %in% CAPITAL_CITY_GCCSA_CODES
}

#' build_donor_pools
#' Takes the tidy employment panel and returns a named list of three donor
#' pools, each a character vector of eligible sa2_codes.
#'
#' All pools exclude:
#'   - Treated SA2s (full and partial)
#'   - Greater Sydney metro SA2s (demand spillover contamination), identified
#'     via the GCCSA delineation file with SA4-range fallback.
#'     Explicitly excluded Sydney SA4s: 117, 118, 119, 120, 121, 122, 125, 126.
#'   - SA2s with insufficient data across the analysis window (< MIN_OBS years)
#'   - Fortitude Valley (Brisbane, SA2 305011106) from 2016 onward due to QLD
#'     lockout laws (handled via NA imputation in build_sc_panel())
#'
#' @param panel           tibble: output of build_sc_panel()
#' @param gccsa_lookup    named character vector from load_sa2_gccsa_lookup()
#' @return named list with elements P1, P2, P3
build_donor_pools <- function(panel, gccsa_lookup) {
  
  # Use "Total" if present, otherwise fall back to first available industry
  completeness_industry <- if ("Total" %in% unique(panel$industry)) "Total" else unique(panel$industry)[1]
  
  complete_sa2s <- panel |>
    filter(industry == completeness_industry) |>
    group_by(sa2_code) |>
    summarise(n_obs = sum(!is.na(index)), .groups = "drop") |>
    filter(n_obs >= MIN_OBS) |>
    pull(sa2_code)
  
  # Exclude treated SA2s and all Greater Sydney SA2s
  base_eligible <- panel |>
    filter(industry == "Total") |>
    distinct(sa2_code, sa2_name, state_code) |>
    filter(
      sa2_code %in% complete_sa2s,
      !sa2_code %in% ALL_TREATED_SA2S,
      !is_greater_sydney(sa2_code, state_code, gccsa_lookup)
    )
  
  # P1: All Australia (exc. Greater Sydney)
  P1 <- base_eligible$sa2_code
  
  # P2: All Australia exc. NSW entirely
  P2 <- base_eligible |>
    filter(state_code != NSW_STATE_CODE) |>
    pull(sa2_code)
  
  # P3: Capital cities only — SA2s whose GCCSA is one of the seven non-Sydney
  #     capital city GCCSAs, as delineated in the ABS 2016 ASGS file.
  #     Only SA2s present in the delineation lookup are eligible; any SA2 with
  #     an unknown GCCSA code is excluded conservatively.
  P3 <- base_eligible |>
    filter(is_capital_city_gccsa(sa2_code, gccsa_lookup)) |>
    pull(sa2_code)
  
  list(P1 = P1, P2 = P2, P3 = P3)
}

# =============================================================================
# 3. PANEL PREPARATION
# =============================================================================

#' build_sc_panel
#' From emp_sa2_csv_tidy, constructs a long employment panel indexed to
#' BASE_YEAR = 100, suitable for the Synth estimator.
#'
#' Industry coverage differs by SA2 role to keep the panel lean:
#'   - Treated SA2s (ALL_TREATED_SA2S) and spillover SA2s (SPILLOVER_SA2S)
#'     receive ALL_ABS_INDUSTRIES (20 divisions + Total).
#'   - All other SA2s (donor pool candidates) receive TARGET_INDUSTRIES only
#'     (Total + the three key industries). Donor SA2s are never asked to
#'     serve as the "treated" unit in run_synth_one(), so they only need the
#'     industries used in run_sc_estimates() donor matching.
#'
#' The Fortitude Valley exclusion is applied post-2015:
#'   - For year_start > 2015 (i.e. 2016-17 FY onward), Fortitude Valley
#'     (305011106) jobs are set to NA, preventing it from serving as a donor
#'     in years contaminated by Queensland's own lockout laws.
#'
#' @param emp  tibble: emp_sa2_csv_tidy from the targets pipeline
#' @return long tibble with columns:
#'   sa2_code, sa2_name, state_code, state_name,
#'   treated_full, treated_partial, treated,
#'   year_start, industry, index, jobs_000
build_sc_panel <- function(emp, pop = NULL) {
  
  FORTITUDE_VALLEY  <- "305011106"
  wide_coverage_sa2s <- c(ALL_TREATED_SA2S, SPILLOVER_SA2S)
  
  # Employment-only industry lists — never include "Population"
  emp_all_industries    <- setdiff(ALL_ABS_INDUSTRIES, "Population")
  emp_target_industries <- setdiff(TARGET_INDUSTRIES,  "Population")
  
  # ---------------------------------------------------------------------------
  # Helper: build long panel for a given set of SA2s and industries
  # ---------------------------------------------------------------------------
  make_long <- function(emp_subset, industries) {
    
    non_total_inds <- setdiff(industries, "Total")
    
    total_long <- emp_subset |>
      filter(year_start %in% ALL_YEARS, industry != "Total") |>
      group_by(sa2_code, sa2_name, state_code, state_name,
               treated_full, treated_partial, treated,
               year_start) |>
      summarise(jobs_000 = sum(jobs_000, na.rm = TRUE), .groups = "drop") |>
      mutate(industry = "Total")
    
    ind_long <- emp_subset |>
      filter(
        year_start %in% ALL_YEARS,
        as.character(industry) %in% non_total_inds
      ) |>
      select(sa2_code, sa2_name, state_code, state_name,
             treated_full, treated_partial, treated,
             year_start, industry, jobs_000) |>
      mutate(industry = as.character(industry))
    
    bind_rows(total_long, ind_long)
  }
  
  # Wide-coverage panel: treated + spillover SA2s get all employment industries
  wide_long <- emp |>
    filter(sa2_code %in% wide_coverage_sa2s) |>
    make_long(industries = emp_all_industries)
  
  # Narrow-coverage panel: donor candidates get target employment industries
  narrow_long <- emp |>
    filter(!sa2_code %in% wide_coverage_sa2s) |>
    make_long(industries = emp_target_industries)
  
  panel_long <- bind_rows(wide_long, narrow_long)
  
  # Nullify Fortitude Valley post-2015
  panel_long <- panel_long |>
    mutate(
      jobs_000 = if_else(
        sa2_code == FORTITUDE_VALLEY & year_start > 2015L,
        NA_real_,
        jobs_000
      )
    )
  
  # Index to BASE_YEAR per SA2 × industry
  base_vals <- panel_long |>
    filter(year_start == BASE_YEAR, !is.na(jobs_000), jobs_000 > 0) |>
    select(sa2_code, industry, base_jobs = jobs_000)
  
  panel_long <- panel_long |>
    left_join(base_vals, by = c("sa2_code", "industry")) |>
    mutate(
      index = if_else(!is.na(base_jobs) & base_jobs > 0,
                      100 * jobs_000 / base_jobs,
                      NA_real_)
    ) |>
    select(-base_jobs)
  
  # ------------------------------------------------------------------
  # Optionally append population as a pseudo-industry
  # Population is only added for treated and spillover SA2s.
  # Donor SA2s use their employment-based index as the counterfactual.
  # ------------------------------------------------------------------
  if (!is.null(pop)) {
    
    pop_filtered <- pop |>
      filter(year %in% ALL_YEARS) |>
      mutate(
        jobs_000 = population / 1000,
        industry = "Population",
        year_start = year
      ) |>
      select(sa2_code, sa2_name, state_code, state_name,
             treated_full, treated_partial, treated,
             year_start, industry, jobs_000)
    
    # Index population to BASE_YEAR
    pop_base <- pop_filtered |>
      filter(year_start == BASE_YEAR, !is.na(jobs_000), jobs_000 > 0) |>
      select(sa2_code, base_jobs = jobs_000)
    
    pop_indexed <- pop_filtered |>
      left_join(pop_base, by = "sa2_code") |>
      mutate(
        index = if_else(!is.na(base_jobs) & base_jobs > 0,
                        100 * jobs_000 / base_jobs,
                        NA_real_)
      ) |>
      select(-base_jobs)
    
    panel_long <- bind_rows(panel_long, pop_indexed)
  }
  
  # Append median income as pseudo-industries ("Income: <industry>")
  # Indexed to BASE_YEAR = 100. Included for ALL SA2s (donors need income
  # data to serve as synthetic controls for income outcomes).
  income_industries <- c("Total", "Retail trade",
                         "Accommodation and food services",
                         "Arts and recreation services")
  
  income_long <- emp |>
    filter(year_start %in% ALL_YEARS,
           as.character(industry) %in% income_industries,
           !is.na(median_income_dollars), median_income_dollars > 0) |>
    mutate(
      jobs_000  = median_income_dollars / 1000,
      industry  = paste0("Income: ", as.character(industry)),
      sa2_code  = as.character(sa2_code)
    ) |>
    select(sa2_code, sa2_name, state_code, state_name,
           treated_full, treated_partial, treated,
           year_start, industry, jobs_000)
  
  income_base <- income_long |>
    filter(year_start == BASE_YEAR, !is.na(jobs_000), jobs_000 > 0) |>
    select(sa2_code, industry, base_jobs = jobs_000)
  
  income_indexed <- income_long |>
    left_join(income_base, by = c("sa2_code", "industry")) |>
    mutate(index = if_else(!is.na(base_jobs) & base_jobs > 0,
                           100 * jobs_000 / base_jobs, NA_real_)) |>
    select(-base_jobs)
  
  panel_long <- bind_rows(panel_long, income_indexed)
  
  panel_long
}
# =============================================================================
# 4. SYNTHETIC CONTROL ESTIMATOR (ADH via Synth package)
# =============================================================================

#' run_synth_one
#' Runs the Abadie–Diamond–Hainmueller (2010) synthetic control estimator for
#' a single treated SA2 × outcome × donor pool combination.
#'
#' Returns a list with:
#'   $weights   tibble — donor SA2s and their SC weights (weight > 1e-4 only)
#'   $gap       tibble — year_start, treated_index, synth_index, gap
#'   $pre_mspe  scalar — pre-treatment mean squared prediction error
#'   $att       scalar — average post-treatment gap (ATT estimate)
#'
#' Returns NULL (with a warning) if Synth fails or has insufficient donors.
#'
#' @param panel         long panel tibble from build_sc_panel()
#' @param treated_code  sa2_code of the treated unit (character)
#' @param donor_codes   character vector of eligible donor sa2_codes
#' @param ind           industry string, e.g. "Total"
#' @param pre_years     integer vector of pre-treatment year_start values
#' @param post_years    integer vector of post-treatment year_start values
run_synth_one <- function(panel,
                          treated_code,
                          donor_codes,
                          ind,
                          pre_years  = PRE_YEARS,
                          post_years = TREATMENT_YEAR:max(ALL_YEARS)) {
  
  # Subset to this industry, including only treated + donor SA2s
  df <- panel |>
    filter(
      industry == ind,
      sa2_code %in% c(treated_code, donor_codes),
      year_start %in% ALL_YEARS,
      !is.na(index)
    )
  
  # Keep only donors complete across the entire analysis window
  complete_donors <- df |>
    filter(sa2_code != treated_code) |>
    group_by(sa2_code) |>
    filter(all(ALL_YEARS %in% year_start)) |>
    summarise(n = n(), .groups = "drop") |>
    filter(n == length(ALL_YEARS)) |>
    pull(sa2_code)
  
  if (length(complete_donors) < 2) {
    warning(sprintf(
      "Fewer than 2 complete donors for %s / %s — skipping", treated_code, ind
    ))
    return(NULL)
  }
  
  df <- df |> filter(sa2_code %in% c(treated_code, complete_donors))
  
  # Build numeric unit-ID map required by Synth
  all_codes   <- unique(df$sa2_code)
  code_to_id  <- setNames(seq_along(all_codes), all_codes)
  treated_id  <- code_to_id[[treated_code]]
  control_ids <- unname(code_to_id[complete_donors])
  
  df <- df |> mutate(unit_id = code_to_id[sa2_code])
  
  time_periods <- sort(unique(df$year_start))
  pre_period   <- intersect(pre_years, time_periods)
  post_period  <- intersect(post_years, time_periods)
  
  if (length(pre_period) < 1) {
    warning(sprintf("No pre-treatment periods for %s / %s", treated_code, ind))
    return(NULL)
  }
  
  tryCatch({
    dp <- dataprep(
      foo                   = as.data.frame(df),
      predictors            = "index",
      predictors.op         = "mean",
      dependent             = "index",
      unit.variable         = "unit_id",
      time.variable         = "year_start",
      treatment.identifier  = treated_id,
      controls.identifier   = control_ids,
      time.predictors.prior = pre_period,
      time.optimize.ssr     = pre_period,
      time.plot             = time_periods,
      unit.names.variable   = "sa2_code"
    )
    
    sp <- synth(dp, Sigf.ipop = 5, quadopt = "ipop")
    
    # Extract donor weights
    w_raw    <- sp$solution.w
    w_vec    <- as.numeric(w_raw)
    w_ids    <- as.integer(rownames(w_raw))
    id_to_code <- setNames(names(code_to_id), code_to_id)
    
    weights_tbl <- tibble(
      sa2_code = id_to_code[as.character(w_ids)],
      weight   = w_vec
    ) |>
      filter(weight > 1e-4) |>
      arrange(desc(weight)) |>
      left_join(
        df |> distinct(sa2_code, sa2_name, state_name),
        by = "sa2_code"
      )
    
    # Build gap series
    synth_index   <- as.numeric(dp$Y0plot %*% sp$solution.w)
    treated_index <- as.numeric(dp$Y1plot)
    years_plot    <- as.integer(rownames(dp$Y1plot))
    
    gap_tbl <- tibble(
      year_start    = years_plot,
      treated_index = treated_index,
      synth_index   = synth_index,
      gap           = treated_index - synth_index
    )
    
    # Pre-treatment MSPE
    pre_mspe <- gap_tbl |>
      filter(year_start %in% pre_period) |>
      summarise(mspe = mean(gap^2)) |>
      pull(mspe)
    
    # ATT: average gap over fully post-treatment years
    att <- gap_tbl |>
      filter(year_start %in% post_period) |>
      summarise(att = mean(gap)) |>
      pull(att)
    
    list(weights = weights_tbl, gap = gap_tbl, pre_mspe = pre_mspe, att = att)
    
  }, error = function(e) {
    warning(sprintf(
      "Synth failed for %s / %s: %s", treated_code, ind, conditionMessage(e)
    ))
    return(NULL)
  })
}

# =============================================================================
# 5. PLACEBO-IN-SPACE INFERENCE
# =============================================================================

#' run_placebo_tests
#' Iterates over donor SA2s, treating each as if it were the treated unit,
#' and collects gap series for the permutation distribution.
#'
#' @param panel          full panel tibble
#' @param donor_codes    character vector of donor sa2_codes for this pool
#' @param treated_code   sa2_code of the true treated unit
#' @param ind            industry string
#' @param true_pre_mspe  pre-MSPE of the true treated unit (for MSPE ratio filter)
#' @param max_placebos   cap on number of placebos to run (performance guard)
#' @return tibble of placebo gap series with mspe_ratio column
run_placebo_tests <- function(panel,
                              donor_codes,
                              treated_code,
                              ind,
                              true_pre_mspe,
                              max_placebos = 150L) {
  
  placebo_units <- donor_codes
  if (length(placebo_units) > max_placebos) {
    set.seed(42)
    placebo_units <- sample(placebo_units, max_placebos)
  }
  
  placebo_results <- map(placebo_units, function(placebo_code) {
    # Pool for this placebo: all donors + treated, minus the placebo itself
    placebo_donors <- setdiff(c(donor_codes, treated_code), placebo_code)
    
    res <- run_synth_one(
      panel        = panel,
      treated_code = placebo_code,
      donor_codes  = placebo_donors,
      ind          = ind
    )
    
    if (is.null(res)) return(NULL)
    
    mspe_ratio <- if (true_pre_mspe > 0) res$pre_mspe / true_pre_mspe else NA_real_
    
    res$gap |>
      mutate(
        placebo_code = placebo_code,
        pre_mspe     = res$pre_mspe,
        mspe_ratio   = mspe_ratio
      )
  })
  
  bind_rows(placebo_results)
}

#' compute_pvalue
#' Empirical p-value: share of placebos (after MSPE ratio filter) whose
#' post-treatment average |gap| ≥ |true ATT|.
compute_pvalue <- function(placebo_gaps, true_att,
                           mspe_threshold = MSPE_THRESHOLD) {
  eligible <- placebo_gaps |>
    filter(is.na(mspe_ratio) | mspe_ratio <= mspe_threshold)
  
  if (nrow(eligible) == 0) return(NA_real_)
  
  post_atts <- eligible |>
    filter(year_start >= TREATMENT_YEAR) |>
    group_by(placebo_code) |>
    summarise(placebo_att = mean(gap), .groups = "drop")
  
  mean(abs(post_atts$placebo_att) >= abs(true_att))
}

# =============================================================================
# 6. ESTIMATION — run_sc_estimates()
# =============================================================================
# This function runs all synthetic control estimation and writes the raw
# numeric outputs (CSVs) to disk. It does NOT produce any charts or tables.
# Run this target first; it is the expensive step.
#
# Separating estimation from figure production means that a charting error
# or cosmetic revision never requires re-running Synth.

#' run_sc_estimates
#' Runs SC for all treated/spillover SA2s × industries × pools and writes
#' CSV outputs. Returns a character vector of output file paths (for targets
#' format = "file").
#'
#' Industry scope by unit role:
#'   treated   — ALL_ABS_INDUSTRIES (20 divisions), subject to the 500-worker
#'               filter in 2012-13: any treated SA2 × industry combination
#'               where jobs_000 < MIN_JOBS_2012 in year_start == 2012 is
#'               silently skipped.
#'   spillover — TARGET_INDUSTRIES only (Total + 3 key industries).
#'
#' A `unit_role` column ("treated" / "spillover") is stamped on all output
#' tibbles (weights, gaps, att summary) for downstream filtering.
#'
#' @param emp              tibble: emp_sa2_csv_tidy from the pipeline
#' @param delineation_path path to sa2_deliniation_file.csv
#' @param out_dir          root output directory
#' @param run_placebos     logical: run permutation tests? (slow — ~30 min)
#' @param max_placebos     cap on placebo units per run
run_sc_estimates <- function(emp,
                             pop              = NULL,
                             delineation_path = "data/raw/sa2_deliniation_file.csv",
                             out_dir          = OUT_DIR,
                             run_placebos     = TRUE,
                             max_placebos     = 100L) {
  
  dir_create(out_dir)
  
  # ------------------------------------------------------------------
  # Load GCCSA lookup and build panel + pools
  # ------------------------------------------------------------------
  message("=== Loading SA2 GCCSA delineation ===")
  gccsa_lookup <- load_sa2_gccsa_lookup(delineation_path)
  
  message("=== Building SC panel ===")
  panel <- build_sc_panel(emp, pop = pop)
  
  message("=== Building donor pools ===")
  pools <- build_donor_pools(panel, gccsa_lookup)
  
  message(sprintf(
    "Pool sizes — P1: %d  P2: %d  P3: %d",
    length(pools$P1), length(pools$P2), length(pools$P3)
  ))
  
  # ------------------------------------------------------------------
  # Pre-compute 500-worker filter for treated SA2s
  # Jobs in 2012-13 (year_start == 2012) by SA2 × industry.
  # Any combination below MIN_JOBS_2012 is excluded from the treated loop.
  # ------------------------------------------------------------------
  jobs_2012 <- panel |>
    filter(year_start == 2012L, sa2_code %in% ALL_TREATED_SA2S) |>
    select(sa2_code, industry, jobs_000)
  
  passes_500_filter <- function(sa2, ind) {
    j <- jobs_2012 |>
      filter(sa2_code == sa2, industry == ind) |>
      pull(jobs_000)
    length(j) > 0 && !is.na(j) && j >= MIN_JOBS_2012
  }
  
  # ------------------------------------------------------------------
  # SA2 metadata helpers
  # ------------------------------------------------------------------
  # Labels for treated SA2s
  treated_labels <- panel |>
    filter(sa2_code %in% ALL_TREATED_SA2S) |>
    distinct(sa2_code, sa2_name, treated_full, treated_partial) |>
    mutate(treatment_status = if_else(treated_full == 1L, "Full", "Partial"))
  
  # Labels for spillover SA2s — treatment_status = "Spillover" for clarity
  spillover_labels <- panel |>
    filter(sa2_code %in% SPILLOVER_SA2S) |>
    distinct(sa2_code, sa2_name) |>
    mutate(treatment_status = "Spillover")
  
  # ------------------------------------------------------------------
  # Inner helper: record one SC result into the accumulator lists
  # ------------------------------------------------------------------
  record_result <- function(acc, pool_id, unit_code, unit_name,
                            treatment_status, unit_role, ind, res,
                            run_placebos, donor_codes, panel, max_placebos) {
    
    key <- paste(pool_id, unit_code, ind, sep = "|")
    
    acc$weights[[key]] <- res$weights |>
      mutate(
        pool_id          = pool_id,
        pool_label       = POOL_LABELS[pool_id],
        treated_code     = unit_code,
        treated_name     = unit_name,
        treatment_status = treatment_status,
        unit_role        = unit_role,
        industry         = ind
      )
    
    acc$gaps[[key]] <- res$gap |>
      mutate(
        pool_id          = pool_id,
        pool_label       = POOL_LABELS[pool_id],
        treated_code     = unit_code,
        treated_name     = unit_name,
        treatment_status = treatment_status,
        unit_role        = unit_role,
        industry         = ind,
        pre_mspe         = res$pre_mspe
      )
    
    att_row <- tibble(
      pool_id          = pool_id,
      pool_label       = POOL_LABELS[pool_id],
      treated_code     = unit_code,
      treated_name     = unit_name,
      treatment_status = treatment_status,
      unit_role        = unit_role,
      industry         = ind,
      att              = res$att,
      pre_mspe         = res$pre_mspe,
      n_donors         = nrow(res$weights)
    )
    
    if (run_placebos) {
      message(sprintf("      Running placebo tests ..."))
      plac <- run_placebo_tests(
        panel         = panel,
        donor_codes   = donor_codes,
        treated_code  = unit_code,
        ind           = ind,
        true_pre_mspe = res$pre_mspe,
        max_placebos  = max_placebos
      )
      if (!is.null(plac) && nrow(plac) > 0) {
        pval <- compute_pvalue(plac, res$att)
        att_row <- att_row |>
          mutate(p_value = pval, n_placebos = n_distinct(plac$placebo_code))
        acc$placebos[[key]] <- plac |>
          mutate(pool_id = pool_id, treated_code = unit_code, industry = ind)
      }
    }
    
    acc$att[[key]] <- att_row
    acc
  }
  
  # ------------------------------------------------------------------
  # Main loop: pool × (treated SA2s + spillover SA2s) × industry
  # ------------------------------------------------------------------
  all_weights  <- list()
  all_gaps     <- list()
  all_placebos <- list()
  all_att      <- list()
  
  for (pool_id in names(pools)) {
    
    message(sprintf("\n>>> Pool: %s (%s)", pool_id, POOL_LABELS[[pool_id]]))
    donor_codes <- pools[[pool_id]]
    
    acc <- list(weights = list(), gaps = list(), placebos = list(), att = list())
    
    # ---- Treated SA2s: all 20 ABS industry divisions, with 500-job filter ----
    message("  --- Treated SA2s (all ABS industries) ---")
    
    for (unit_code in ALL_TREATED_SA2S) {
      
      unit_name <- treated_labels |>
        filter(sa2_code == unit_code) |> pull(sa2_name)
      tstatus <- treated_labels |>
        filter(sa2_code == unit_code) |> pull(treatment_status)
      
      message(sprintf("  Treated: %s (%s) [%s]", unit_name, unit_code, tstatus))
      
      for (ind in ALL_ABS_INDUSTRIES) {
        
        # 500-worker filter
        if (!passes_500_filter(unit_code, ind)) {
          message(sprintf("    [skip] %s — below 500-worker threshold in 2012-13", ind))
          next
        }
        
        message(sprintf("    Industry: %s", ind))
        
        res <- tryCatch(
          withTimeout(
            run_synth_one(panel = panel, treated_code = unit_code,
                          donor_codes = donor_codes, ind = ind),
            timeout = 120
          ),
          TimeoutException = function(e) {
            message(sprintf("    [timeout] %s — %s", unit_name, ind))
            NULL
          },
          error = function(e) {
            message(sprintf("    [error] %s — %s: %s", unit_name, ind, e$message))
            NULL
          }
        )
        if (is.null(res)) next
        
        acc <- record_result(acc, pool_id, unit_code, unit_name,
                             tstatus, "treated", ind, res,
                             run_placebos, donor_codes, panel, max_placebos)
      }
      
      # ---- Median income outcomes for treated SA2s ----
      for (ind in paste0("Income: ", INCOME_INDUSTRIES)) {
        emp_ind <- str_remove(ind, "^Income: ")
        if (!passes_500_filter(unit_code, emp_ind)) {
          message(sprintf("    [skip] %s — corresponding employment below threshold", ind))
          next
        }
        message(sprintf("    Income outcome: %s", ind))
        res <- tryCatch(
          withTimeout(
            run_synth_one(panel = panel, treated_code = unit_code,
                          donor_codes = donor_codes, ind = ind),
            timeout = 120
          ),
          TimeoutException = function(e) {
            message(sprintf("    [timeout] %s — %s", unit_name, ind))
            NULL
          },
          error = function(e) {
            message(sprintf("    [error] %s — %s: %s", unit_name, ind, e$message))
            NULL
          }
        )
        if (is.null(res)) next
        acc <- record_result(acc, pool_id, unit_code, unit_name,
                             tstatus, "treated", ind, res,
                             run_placebos, donor_codes, panel, max_placebos)
      }
    }
    
    # ---- Spillover SA2s: TARGET_INDUSTRIES only ----
    message("  --- Spillover SA2s (core industries only) ---")
    
    for (unit_code in SPILLOVER_SA2S) {
      
      meta <- spillover_labels |> filter(sa2_code == unit_code)
      if (nrow(meta) == 0) {
        warning(sprintf("Spillover SA2 %s not found in panel — skipping", unit_code))
        next
      }
      unit_name <- meta$sa2_name
      
      message(sprintf("  Spillover: %s (%s)", unit_name, unit_code))
      
      # Donor pool for spillover: same interstate pool, but also exclude all
      # other Sydney spillover SA2s and treated SA2s to avoid contamination.
      spillover_donors <- setdiff(donor_codes, c(ALL_TREATED_SA2S, SPILLOVER_SA2S))
      
      for (ind in TARGET_INDUSTRIES) {
        
        message(sprintf("    Industry: %s", ind))
        
        res <- tryCatch(
          withTimeout(
            run_synth_one(panel = panel, treated_code = unit_code,
                          donor_codes = spillover_donors, ind = ind),
            timeout = 120
          ),
          TimeoutException = function(e) {
            message(sprintf("    [timeout] %s — %s", unit_name, ind))
            NULL
          },
          error = function(e) {
            message(sprintf("    [error] %s — %s: %s", unit_name, ind, e$message))
            NULL
          }
        )
        if (is.null(res)) next
        
        acc <- record_result(acc, pool_id, unit_code, unit_name,
                             "Spillover", "spillover", ind, res,
                             run_placebos, spillover_donors, panel, max_placebos)
      }
      
      # ---- Median income outcomes for spillover SA2s ----
      for (ind in paste0("Income: ", INCOME_INDUSTRIES)) {
        message(sprintf("    Income outcome: %s", ind))
        res <- tryCatch(
          withTimeout(
            run_synth_one(panel = panel, treated_code = unit_code,
                          donor_codes = spillover_donors, ind = ind),
            timeout = 120
          ),
          TimeoutException = function(e) {
            message(sprintf("    [timeout] %s — %s", unit_name, ind))
            NULL
          },
          error = function(e) {
            message(sprintf("    [error] %s — %s: %s", unit_name, ind, e$message))
            NULL
          }
        )
        if (is.null(res)) next
        acc <- record_result(acc, pool_id, unit_code, unit_name,
                             "Spillover", "spillover", ind, res,
                             run_placebos, spillover_donors, panel, max_placebos)
      }
      
    }
    
    # Collect pool results
    all_weights[[pool_id]]  <- bind_rows(acc$weights)
    all_gaps[[pool_id]]     <- bind_rows(acc$gaps)
    all_att[[pool_id]]      <- bind_rows(acc$att)
    if (run_placebos && length(acc$placebos) > 0)
      all_placebos[[pool_id]] <- bind_rows(acc$placebos)
    
    # Write per-pool CSVs immediately so partial results survive a later failure
    write_csv(all_weights[[pool_id]],
              path(out_dir, sprintf("sc_weights_%s.csv", pool_id)))
    write_csv(all_gaps[[pool_id]],
              path(out_dir, sprintf("sc_gaps_%s.csv", pool_id)))
    if (run_placebos && length(acc$placebos) > 0)
      write_csv(all_placebos[[pool_id]],
                path(out_dir, sprintf("sc_placebo_%s.csv", pool_id)))
    
  } # pool loop
  
  # Write combined summary
  all_att_df <- bind_rows(all_att)
  write_csv(all_att_df, path(out_dir, "sc_att_summary.csv"))
  
  message("\n=== SC estimation complete ===")
  message(sprintf("CSVs written to: %s", out_dir))
  
  invisible(all_att_df)
}

# =============================================================================
# 7. FIGURES & TABLES — run_sc_figures()
# =============================================================================
# This function reads the CSVs written by run_sc_estimates() and produces all
# charts and formatted tables. It has no dependency on the Synth package and
# can be re-run cheaply whenever chart aesthetics need updating.

#' run_sc_figures
#' Reads estimation CSVs from out_dir and writes all PDF charts and tables.
#' Returns a character vector of output file paths (for targets format = "file").
#'
#' @param out_dir  root output directory (must contain estimation CSVs)
run_sc_figures <- function(out_dir = OUT_DIR) {
  
  dir_create(path(out_dir, "figures"))
  dir_create(path(out_dir, "tables"))
  
  # ------------------------------------------------------------------
  # Load estimation outputs
  # ------------------------------------------------------------------
  message("=== Loading estimation outputs ===")
  
  # Read per-pool CSVs and bind
  pool_ids <- names(POOL_LABELS)
  
  all_gaps_df <- map(pool_ids, function(pid) {
    f <- path(out_dir, sprintf("sc_gaps_%s.csv", pid))
    if (file_exists(f)) read_csv(f, show_col_types = FALSE) else NULL
  }) |> bind_rows() |> mutate(treated_code = as.character(treated_code))
  
  all_weights_df <- map(pool_ids, function(pid) {
    f <- path(out_dir, sprintf("sc_weights_%s.csv", pid))
    if (file_exists(f)) read_csv(f, show_col_types = FALSE) else NULL
  }) |> bind_rows() |> mutate(treated_code = as.character(treated_code))
  
  all_att_df <- {
    f <- path(out_dir, "sc_att_summary.csv")
    if (file_exists(f)) {
      read_csv(f, show_col_types = FALSE) |>
        mutate(treated_code = as.character(treated_code))
    } else {
      stop("sc_att_summary.csv not found — run sc_estimates target first.")
    }
  }
  
  all_placebos_df <- map(pool_ids, function(pid) {
    f <- path(out_dir, sprintf("sc_placebo_%s.csv", pid))
    if (file_exists(f)) read_csv(f, show_col_types = FALSE) else NULL
  }) |> bind_rows()
  if (nrow(all_placebos_df) == 0 || ncol(all_placebos_df) == 0) {
    all_placebos_df <- NULL
  } else {
    all_placebos_df <- all_placebos_df |>
      mutate(
        treated_code = as.character(treated_code),
        placebo_code = as.character(placebo_code)
      )
  }
  
  # ------------------------------------------------------------------
  # Produce charts and tables
  # ------------------------------------------------------------------
  message("=== Producing charts ===")
  produce_gap_charts(all_gaps_df, all_placebos_df, POOL_LABELS,
                     path(out_dir, "figures"))
  produce_placebo_charts(all_gaps_df, all_placebos_df, POOL_LABELS,
                         path(out_dir, "figures"))
  produce_weight_tables(all_weights_df, POOL_LABELS,
                        path(out_dir, "figures"))
  produce_att_table(all_att_df, path(out_dir, "tables"))
  
  message("=== Figure production complete ===")
  message(sprintf("Outputs written to: %s", out_dir))
  
  # Return all output file paths for targets tracking
  dir_ls(path(out_dir, "figures"), recurse = FALSE, type = "file") |>
    c(dir_ls(path(out_dir, "tables"), recurse = FALSE, type = "file")) |>
    as.character()
}

# =============================================================================
# 8. CHART FUNCTIONS
# =============================================================================

# Shared colour palette for treated SA2s
TREATED_COLOURS <- c(
  "117031337" = "#D62728",   # Sydney - Haymarket - The Rocks
  "117031329" = "#E88A1A",   # Darlinghurst
  "117031333" = "#8C1A6A",   # Potts Point - Woolloomooloo
  "117031336" = "#888888",   # Surry Hills (partial)
  "117031334" = "#BBBBBB"    # Pyrmont - Ultimo (partial)
)

# Colour palette for spillover SA2s (cool greens/teals to distinguish from treated)
SPILLOVER_COLOURS <- c(
  "117031332" = "#1B7837",   # Newtown - Camperdown - Darlington
  "117031331" = "#4DAC26",   # Glebe - Forest Lodge
  "117031335" = "#A6DBA0",   # Redfern - Chippendale
  "117031338" = "#7FBF7B",   # Waterloo - Beaconsfield
  "117031330" = "#2166AC",   # Erskineville - Alexandria
  "120021388" = "#4393C3",   # Leichhardt - Annandale
  "120021387" = "#92C5DE",   # Balmain
  "120021389" = "#D1E5F0",   # Lilyfield - Rozelle
  "118011345" = "#8073AC",   # Paddington - Moore Park
  "118011341" = "#B2ABD2"    # Bondi Junction - Waverly
)

ALL_UNIT_COLOURS <- c(TREATED_COLOURS, SPILLOVER_COLOURS)

#' produce_gap_charts
#' For each pool × unit_role × industry, writes two PDFs:
#'   sc_actual_vs_synth_<role>_<pool>_<industry>.pdf
#'   sc_gap_<role>_<pool>_<industry>.pdf
#'
#' Treated SA2s are faceted across all industries present in the data.
#' Spillover SA2s are faceted across TARGET_INDUSTRIES only.
#' The two roles are written to separate PDFs so the treated-only charts
#' (which may have up to 20 industries) don't crowd the spillover charts.
produce_gap_charts <- function(gaps_df, placebos_df, POOL_LABELS, fig_dir) {
  
  roles <- unique(gaps_df$unit_role)
  
  for (role in roles) {
    
    gaps_role <- gaps_df |> filter(unit_role == role)
    inds_to_plot <- unique(gaps_role$industry)
    
    for (pool_id in unique(gaps_role$pool_id)) {
      for (ind in inds_to_plot) {
        
        g <- gaps_role |>
          filter(pool_id == !!pool_id, industry == !!ind)
        
        if (nrow(g) == 0) next
        
        # Placebo fan (only generated when placebos were run)
        fan <- NULL
        if (!is.null(placebos_df) && nrow(placebos_df) > 0) {
          plac_sub <- placebos_df |>
            filter(pool_id == !!pool_id, industry == !!ind,
                   treated_code %in% unique(g$treated_code))
          if (nrow(plac_sub) > 0) {
            fan <- plac_sub |>
              filter(is.na(mspe_ratio) | mspe_ratio <= MSPE_THRESHOLD) |>
              group_by(year_start) |>
              summarise(
                lo = quantile(gap, 0.05, na.rm = TRUE),
                hi = quantile(gap, 0.95, na.rm = TRUE),
                .groups = "drop"
              )
          }
        }
        
        role_label <- if (role == "treated") "Treated" else "Spillover"
        
        # Chart 1: actual vs synthetic index
        p1 <- ggplot(g, aes(x = year_start)) +
          geom_vline(xintercept = TREATMENT_YEAR + 0.5,
                     linetype = "dashed", colour = "grey40", linewidth = 0.5) +
          geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.3) +
          geom_line(aes(y = synth_index, colour = "Synthetic"),
                    linewidth = 0.9, linetype = "dashed") +
          geom_line(aes(y = treated_index, colour = "Actual"), linewidth = 1.0) +
          geom_point(aes(y = treated_index, colour = "Actual"), size = 2) +
          facet_wrap(~ paste0(treated_name, "\n[", treatment_status, "]"),
                     scales = "free_y", ncol = 3) +
          scale_colour_manual(
            values = c("Actual" = "#D62728", "Synthetic" = "#1F77B4"),
            name = NULL
          ) +
          scale_x_continuous(breaks = ALL_YEARS) +
          labs(
            title    = sprintf("Actual vs Synthetic [%s] — %s", role_label, ind),
            subtitle = sprintf(
              "%s  |  Index: 100 = %d–%02d FY  |  Vertical: Feb 2014 onset",
              POOL_LABELS[pool_id], BASE_YEAR, (BASE_YEAR + 1L) %% 100L
            ),
            x = "Fiscal year start", y = "Employment index (100 = base year)",
            caption = "Synthetic: ADH (2010) Synth estimator  |  Dashed vertical: treatment onset"
          ) +
          theme_minimal(base_size = 10) +
          theme(
            legend.position  = "bottom",
            strip.text       = element_text(face = "bold", size = 8),
            panel.grid.minor = element_blank(),
            axis.text.x      = element_text(angle = 45, hjust = 1)
          )
        
        # Chart 2: gap with placebo fan
        p2_base <- ggplot(g, aes(x = year_start)) +
          geom_vline(xintercept = TREATMENT_YEAR + 0.5,
                     linetype = "dashed", colour = "grey40", linewidth = 0.5) +
          geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5)
        
        if (!is.null(fan)) {
          p2_base <- p2_base +
            geom_ribbon(
              data = fan,
              aes(x = year_start, ymin = lo, ymax = hi),
              inherit.aes = FALSE,
              fill = "steelblue", alpha = 0.15
            )
        }
        
        p2 <- p2_base +
          geom_line(aes(y = gap, colour = treated_code), linewidth = 1.0) +
          geom_point(aes(y = gap, colour = treated_code), size = 2) +
          facet_wrap(~ paste0(treated_name, "\n[", treatment_status, "]"),
                     scales = "free_y", ncol = 3) +
          scale_colour_manual(values = ALL_UNIT_COLOURS, guide = "none") +
          scale_x_continuous(breaks = ALL_YEARS) +
          labs(
            title    = sprintf("Gap (Actual − Synthetic) [%s] — %s", role_label, ind),
            subtitle = sprintf(
              "%s  |  Shaded band: 5th–95th pctile of placebo distribution (MSPE ratio ≤ %d×)",
              POOL_LABELS[pool_id], MSPE_THRESHOLD
            ),
            x = "Fiscal year start",
            y = "Gap in employment index (index points)",
            caption = if (role == "treated") {
              "Negative gap = lockout laws associated with lower employment than counterfactual"
            } else {
              "Positive gap = spillover SA2 gained employment relative to counterfactual"
            }
          ) +
          theme_minimal(base_size = 10) +
          theme(
            strip.text       = element_text(face = "bold", size = 8),
            panel.grid.minor = element_blank(),
            axis.text.x      = element_text(angle = 45, hjust = 1)
          )
        
        ind_slug <- ind |>
          tolower() |>
          str_replace_all("[^a-z0-9]+", "_") |>
          str_remove("_$")
        
        ggsave(path(fig_dir, sprintf("sc_actual_vs_synth_%s_%s_%s.pdf",
                                     role, pool_id, ind_slug)),
               p1, width = 14, height = 8)
        ggsave(path(fig_dir, sprintf("sc_gap_%s_%s_%s.pdf",
                                     role, pool_id, ind_slug)),
               p2, width = 14, height = 8)
      }
    }
  }
}

#' produce_placebo_charts
#' For each pool × unit (treated + spillover): a spaghetti chart of placebo
#' gap series (grey) with the true gap overlaid in colour. One PDF per unit,
#' faceted by industry. Treated SA2s show all industries run; spillover SA2s
#' show TARGET_INDUSTRIES only.
produce_placebo_charts <- function(gaps_df, placebos_df, POOL_LABELS, fig_dir) {
  
  if (is.null(placebos_df) || nrow(placebos_df) == 0) return(invisible(NULL))
  
  all_units <- c(ALL_TREATED_SA2S, SPILLOVER_SA2S)
  
  for (pool_id in unique(gaps_df$pool_id)) {
    for (unit_code in all_units) {
      
      g_true <- gaps_df |>
        filter(pool_id == !!pool_id, treated_code == !!unit_code)
      
      if (nrow(g_true) == 0) next
      
      unit_name <- g_true |> slice(1) |> pull(treated_name)
      role      <- g_true |> slice(1) |> pull(unit_role)
      role_label <- if (role == "treated") "Treated" else "Spillover"
      
      g_plac <- placebos_df |>
        filter(pool_id == !!pool_id, treated_code == !!unit_code,
               is.na(mspe_ratio) | mspe_ratio <= MSPE_THRESHOLD)
      
      if (nrow(g_plac) == 0) next
      
      unit_colour <- ALL_UNIT_COLOURS[[unit_code]]
      if (is.null(unit_colour)) unit_colour <- "#333333"
      
      p <- ggplot() +
        geom_vline(xintercept = TREATMENT_YEAR + 0.5,
                   linetype = "dashed", colour = "grey30", linewidth = 0.5) +
        geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
        geom_line(
          data = g_plac,
          aes(x = year_start, y = gap, group = placebo_code),
          colour = "grey70", linewidth = 0.3, alpha = 0.6
        ) +
        geom_line(
          data = g_true,
          aes(x = year_start, y = gap),
          colour = unit_colour, linewidth = 1.2
        ) +
        geom_point(
          data = g_true,
          aes(x = year_start, y = gap),
          colour = unit_colour, size = 2.5
        ) +
        facet_wrap(~ industry, scales = "free_y", ncol = 2) +
        scale_x_continuous(breaks = ALL_YEARS) +
        labs(
          title    = sprintf("Placebo test [%s] — %s", role_label, unit_name),
          subtitle = sprintf(
            "%s  |  Grey: placebo gaps (MSPE ratio ≤ %d×)  |  Coloured: true gap",
            POOL_LABELS[pool_id], MSPE_THRESHOLD
          ),
          x = "Fiscal year start",
          y = "Gap in employment index (index points)",
          caption = paste0(
            "Empirical p-value = share of placebos with |post-ATT| ≥ |true ATT|.\n",
            "Only placebos with pre-MSPE ≤ ", MSPE_THRESHOLD,
            "× unit's pre-MSPE are shown."
          )
        ) +
        theme_minimal(base_size = 10) +
        theme(
          strip.text       = element_text(face = "bold", size = 8),
          panel.grid.minor = element_blank(),
          axis.text.x      = element_text(angle = 45, hjust = 1)
        )
      
      # ncol depends on number of industries faceted
      n_inds <- n_distinct(g_true$industry)
      fig_h  <- max(8, ceiling(n_inds / 2) * 4)
      
      code_slug <- str_sub(unit_code, -4, -1)
      ggsave(
        path(fig_dir, sprintf("sc_placebo_%s_%s_%s.pdf",
                              role, pool_id, code_slug)),
        p, width = 12, height = fig_h
      )
    }
  }
}

#' produce_weight_tables
#' Lollipop charts of SC weights.
#' One PDF per pool × unit (treated SA2 or spillover SA2), faceted by
#' industry. Splitting by unit keeps each file to a manageable page count
#' and avoids ggsave's 50-inch height limit.
produce_weight_tables <- function(weights_df, POOL_LABELS, fig_dir) {
  
  all_units <- unique(weights_df$treated_code)
  
  for (pool_id in unique(weights_df$pool_id)) {
    for (unit_code in all_units) {
      
      w <- weights_df |>
        filter(pool_id == !!pool_id, treated_code == !!unit_code) |>
        mutate(
          donor_label = sprintf("%s\n(%s)", sa2_name, state_name),
          facet_label = industry
        ) |>
        group_by(industry) |>
        slice_max(weight, n = 10) |>
        ungroup() |>
        mutate(donor_label = fct_reorder(donor_label, weight, .fun = sum))
      
      if (nrow(w) == 0) next
      
      unit_name <- w$treated_name[[1]]
      role      <- if ("unit_role" %in% names(w)) w$unit_role[[1]] else "treated"
      n_inds    <- n_distinct(w$facet_label)
      # 3 rows per industry panel, min 8 inches, hard cap at 48 inches
      fig_h     <- min(48, max(8, n_inds * 3))
      
      p <- ggplot(w, aes(x = weight, y = donor_label)) +
        geom_segment(
          aes(x = 0, xend = weight, y = donor_label, yend = donor_label),
          colour = "grey60", linewidth = 0.6
        ) +
        geom_point(colour = "#1F77B4", size = 2.5) +
        geom_text(aes(label = sprintf("%.3f", weight)),
                  hjust = -0.2, size = 2.5) +
        facet_wrap(~ facet_label, scales = "free_y", ncol = 2) +
        scale_x_continuous(limits = c(0, 1.05),
                           labels = percent_format(accuracy = 1)) +
        labs(
          title    = sprintf("SC weights [%s] — %s", role, unit_name),
          subtitle = sprintf(
            "%s  |  Top 10 donors per industry (weights sum to 1)",
            POOL_LABELS[pool_id]
          ),
          x = "SC weight", y = NULL,
          caption = "ADH (2010) Synth estimator; weights optimised to minimise pre-treatment MSPE"
        ) +
        theme_minimal(base_size = 9) +
        theme(
          strip.text       = element_text(face = "bold", size = 8),
          panel.grid.minor = element_blank(),
          axis.text.y      = element_text(size = 7)
        )
      
      code_slug <- str_sub(unit_code, -4, -1)
      ggsave(
        path(fig_dir, sprintf("sc_weights_%s_%s_%s.pdf", role, pool_id, code_slug)),
        p, width = 14, height = fig_h
      )
    }
  }
}

#' produce_att_table
#' Writes a formatted summary table of ATT estimates and p-values.
#' The `unit_role` column ("treated" / "spillover") is preserved so readers
#' can filter the table by analysis type.
produce_att_table <- function(att_df, tables_dir) {
  
  dir_create(tables_dir)
  
  has_pval <- "p_value" %in% names(att_df)
  
  tbl <- att_df |>
    mutate(
      att_pct    = sprintf("%+.1f pp", att),
      pre_mspe_f = sprintf("%.2f", pre_mspe),
      pval_f     = if (has_pval) sprintf("%.3f", p_value) else "—",
      sig        = if (has_pval) {
        case_when(
          p_value <= 0.05 ~ "***",
          p_value <= 0.10 ~ "**",
          p_value <= 0.20 ~ "*",
          TRUE            ~ ""
        )
      } else ""
    ) |>
    select(
      `Role`            = unit_role,
      `Pool`            = pool_label,
      `SA2`             = treated_name,
      `Status`          = treatment_status,
      `Industry`        = industry,
      `ATT (index pts)` = att_pct,
      `Pre-MSPE`        = pre_mspe_f,
      `p-value`         = pval_f,
      `Sig`             = sig,
      `N donors`        = n_donors
    )
  
  write_csv(tbl,    path(tables_dir, "sc_att_summary_formatted.csv"))
  write_csv(att_df, path(tables_dir, "sc_att_numeric.csv"))
  
  message(sprintf("ATT table written to %s", path(tables_dir, "sc_att_summary_formatted.csv")))
  
  invisible(tbl)
}

# =============================================================================
# 9. CONVENIENCE WRAPPER — run_sc_analysis()
# =============================================================================
# Calls run_sc_estimates() then run_sc_figures() in sequence.
# Useful for interactive use or single-step pipelines; in targets, prefer the
# two separate targets so that figure reruns don't trigger re-estimation.

#' run_sc_analysis
#' End-to-end wrapper: estimation + figures.
run_sc_analysis <- function(emp,
                            delineation_path = "data/raw/sa2_deliniation_file.csv",
                            out_dir          = OUT_DIR,
                            run_placebos     = TRUE,
                            max_placebos     = 100L) {
  run_sc_estimates(emp,
                   delineation_path = delineation_path,
                   out_dir          = out_dir,
                   run_placebos     = run_placebos,
                   max_placebos     = max_placebos)
  run_sc_figures(out_dir = out_dir)
  invisible(NULL)
}

# =============================================================================
# 10. TARGETS INTEGRATION HELPERS
# =============================================================================

#' sc_run_estimates_target
#' Thin wrapper for tar_target(format = "file"). Runs estimation only and
#' returns a character vector of written CSV file paths.
sc_run_estimates_target <- function(emp,
                                    pop              = NULL,
                                    delineation_path = "data/raw/sa2_deliniation_file.csv",
                                    out_dir          = OUT_DIR,
                                    run_placebos     = TRUE,
                                    max_placebos     = 100L) {
  run_sc_estimates(emp,
                   pop              = pop,
                   delineation_path = delineation_path,
                   out_dir          = out_dir,
                   run_placebos     = run_placebos,
                   max_placebos     = max_placebos)
  # Return all CSV paths for targets to track
  dir_ls(out_dir, recurse = FALSE, type = "file",
         regexp = "\\.csv$") |> as.character()
}

#' sc_run_figures_target
#' Thin wrapper for tar_target(format = "file"). Reads CSVs produced by
#' sc_run_estimates_target() and writes charts/tables. The sc_estimates
#' target must be listed as an upstream dependency in _targets.R so that
#' this target is invalidated whenever new estimates are written.
#'
#' Pass sc_estimates_files (the output of sc_run_estimates_target) as an
#' argument to declare the file dependency explicitly.
sc_run_figures_target <- function(sc_estimates_files,   # dependency declaration
                                  out_dir = OUT_DIR) {
  # sc_estimates_files is used only to declare the dependency; the function
  # reads from out_dir directly via run_sc_figures().
  run_sc_figures(out_dir = out_dir)
}