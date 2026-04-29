# R/welfare_analysis.R
# Equilibrium-level welfare analysis of Sydney's lockout laws using a logit
# residential choice model with share-based amenity identification.
#
# Implements the specification in claude_lockout_welfare_instructions.md:
#   - Logit discrete-choice model: V[k,t] = y[k,t] - r[k,t] + a[k,t]
#   - Income per person: y_pp[k,t] = (E[k,t]/N[k,t]) * y_pw[k,t]
#     so that employment-rate changes (e.g. displaced jobs raising E in
#     Rest of Sydney) flow through to resident welfare via higher y_pp.
#   - Amenity recovery from observed population shares
#   - Citywide expected resident welfare via log-sum-exp
#   - Worker welfare: employment × income_per_worker (unaffected by
#     per-person conversion)
#   - Two geography versions: 3-location and 16-location
#
# Robustness checks:
#   - sigma_robustness: additional scale-parameter values (default: 5000, 15000)
#   - displacement_fractions: share of destroyed CBD + Inner-Ring jobs
#     re-created in Rest of Sydney (default: 0%, 50%, 100%)
#   Each combination of (pool × displacement_fraction × sigma) is a separate
#   scenario. Results are stacked in the output CSVs with identifying columns
#   `sigma`, `displacement_pct`, and `pool_id`.
#
# Inputs:
#   att_detail         — att_detail_by_sa2 tibble or CSV path
#   emp                — emp_sa2_csv_tidy tibble
#   pop                — pop_sa2_tidy tibble (must include gccsa_code)
#   census_housing_path— path to raw census CSV (dwelling counts & tenure)
#   delineation_path   — path to sa2_deliniation_file.csv
#
# Outputs (written to out_dir):
#   lockout_welfare_inputs_3loc.csv    — baselines, treatment effects, post values
#   lockout_welfare_inputs_16loc.csv
#   lockout_welfare_results_3loc.csv   — amenities, welfare objects
#   lockout_welfare_results_16loc.csv
#
# Returns character vector of file paths (for targets format = "file").

compute_welfare_analysis <- function(
    att_detail,
    emp,
    pop,
    census_housing_path,
    delineation_path,
    out_dir  = "outputs/welfare",
    sigma    = 10000,
    .att_dep = NULL,
    # --- Robustness parameters ---
    # Additional sigma values to test alongside the main specification.
    # Set to NULL to skip sigma robustness.
    sigma_robustness = c(5000, 15000),
    # Job-displacement fractions: proportion of destroyed CBD + Inner-Ring
    # jobs that are re-created in Rest of Sydney. The baseline assumption is
    # 0 (no replacement). Each value produces a separate scenario.
    # Set to NULL to skip displacement robustness.
    displacement_fractions = c(0, 0.5, 1.0)
) {

  library(tidyverse)

  # ===========================================================================
  # 0. SA2 definitions (must stay in sync with sc_analysis.R)
  # ===========================================================================

  CBD_SA2S <- c(
    "117031337",  # Sydney - Haymarket - The Rocks
    "117031329",  # Darlinghurst
    "117031333",  # Potts Point - Woolloomooloo
    "117031336",  # Surry Hills
    "117031334"   # Pyrmont - Ultimo
  )

  INNER_RING_SA2S <- c(
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

  GCCSA_SYDNEY <- "1GSYD"

  # ===========================================================================
  # 1. Load ATT detail
  # ===========================================================================
  if (is.character(att_detail) && length(att_detail) == 1 &&
      file.exists(att_detail)) {
    att_detail <- read_csv(att_detail, show_col_types = FALSE)
  }
  att_detail <- att_detail |>
    mutate(treated_code = as.character(treated_code))

  # ===========================================================================
  # 2. Identify all Greater Sydney SA2s
  # ===========================================================================
  delin <- read.csv(delineation_path, stringsAsFactors = FALSE) |>
    as_tibble() |>
    mutate(sa2_code = as.character(SA2_MAINCODE_2016))

  sydney_sa2s <- delin |>
    filter(GCCSA_CODE_2016 == GCCSA_SYDNEY) |>
    pull(sa2_code)

  # ===========================================================================
  # 3. Base-period levels (all Greater Sydney SA2s)
  # ===========================================================================
  BASE_EMP_YEAR <- 2012L
  BASE_POP_YEAR <- 2012L

  # --- Employment & income (FY 2012-13: year_start = 2012) ---
  # median_income_dollars is annual per-job income
  emp_base <- emp |>
    filter(year_start == BASE_EMP_YEAR, industry == "Total") |>
    mutate(
      sa2_code = as.character(sa2_code),
      E0       = suppressWarnings(as.numeric(jobs_000)) * 1000,
      y0       = suppressWarnings(as.numeric(median_income_dollars))
    ) |>
    filter(sa2_code %in% sydney_sa2s) |>
    select(sa2_code, sa2_name, E0, y0)

  # --- Population (calendar year 2012) ---
  pop_base <- pop |>
    filter(year == BASE_POP_YEAR) |>
    mutate(
      sa2_code = as.character(sa2_code),
      N0       = suppressWarnings(as.numeric(population))
    ) |>
    filter(sa2_code %in% sydney_sa2s) |>
    select(sa2_code, N0)

  # --- Census 2011: rent & dwelling counts ---
  # Rent is "average monthly household rental payment" (per-dwelling, monthly)
  census_raw <- read.csv(census_housing_path, check.names = FALSE,
                         stringsAsFactors = FALSE) |>
    as_tibble() |>
    mutate(sa2_code = as.character(sa2_maincode_2016)) |>
    filter(yr == 2011L, sa2_code %in% sydney_sa2s)

  rent_base <- census_raw |>
    mutate(R0_monthly_hh = suppressWarnings(as.numeric(
      rnt_mrtgge_pymnts_cnss_avrge_mnthly_hshld_rntl_pymnt
    ))) |>
    select(sa2_code, R0_monthly_hh) |>
    filter(!is.na(R0_monthly_hh))

  dwelling_base <- census_raw |>
    mutate(
      total_dwellings = suppressWarnings(as.numeric(
        dwelling_structure_private_dwellings_census_total_num
      )),
      rented_pc = suppressWarnings(as.numeric(
        tenure_type_occupied_private_dwellings_census_rented_pc
      )),
      H = total_dwellings #* rented_pc / 100
    ) |>
    select(sa2_code, total_dwellings, H) |>
    filter(!is.na(H))

  # ===========================================================================
  # 3a. Convert rent to annual dollars per person
  #
  # Rent data is per-household monthly. Convert using:
  #   avg_household_size = population / total_occupied_dwellings
  #   annual_rent_per_person = (monthly_hh_rent * 12) / avg_household_size
  #
  # Assumption: average household size is approximated by
  #   population / total_private_dwellings for each SA2.
  #   This is recorded as the occupancy assumption.
  # ===========================================================================
  base_sa2 <- tibble(sa2_code = sydney_sa2s) |>
    left_join(emp_base,      by = "sa2_code") |>
    left_join(pop_base,      by = "sa2_code") |>
    left_join(rent_base,     by = "sa2_code") |>
    left_join(dwelling_base, by = "sa2_code") |>
    mutate(
      avg_hh_size = if_else(
        !is.na(total_dwellings) & total_dwellings > 0,
        N0 / total_dwellings,
        NA_real_
      ),
      # Annual rent per person
      R0 = if_else(
        !is.na(avg_hh_size) & avg_hh_size > 0,
        (R0_monthly_hh * 12) / avg_hh_size,
        NA_real_
      )
    )

  message(sprintf(
    "welfare_analysis: median avg_hh_size = %.2f (range %.2f–%.2f)",
    median(base_sa2$avg_hh_size, na.rm = TRUE),
    min(base_sa2$avg_hh_size, na.rm = TRUE),
    max(base_sa2$avg_hh_size, na.rm = TRUE)
  ))

  # ===========================================================================
  # 4. Safe weighted mean helper
  # ===========================================================================
  wmean <- function(x, w) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    if (sum(ok) == 0) return(0)
    weighted.mean(x[ok], w[ok])
  }

  # ===========================================================================
  # 5. Numerically stable log-sum-exp
  # ===========================================================================
  log_sum_exp <- function(v) {
    m <- max(v)
    m + log(sum(exp(v - m)))
  }

  # ===========================================================================
  # 6. Core welfare computation for a given geography
  #    Returns list(inputs_df, results_df) for one pool × geography version
  # ===========================================================================
  compute_welfare_one <- function(loc_data, sigma, geo_label) {
    # loc_data must have columns:
    #   location, employment_pre, employment_te, population_pre, population_te,
    #   income_pre, income_te, rent_pre, rent_te, is_rest

    n_loc <- nrow(loc_data)
    ref_idx <- which(loc_data$is_rest)
    stopifnot(length(ref_idx) == 1)

    # ------ Post-treatment values ------
    loc_data <- loc_data |>
      mutate(
        employment_post = employment_pre + employment_te,
        income_pw_pre   = income_pre,               # per-worker income (preserve)
        income_pw_post  = income_pre + income_te,    # per-worker income (preserve)
        rent_post       = rent_pre + rent_te
      )

    # Population: CBD/Inner-Ring use level TE; Rest = balancing residual
    total_pop_pre <- sum(loc_data$population_pre)

    loc_data <- loc_data |>
      mutate(
        population_post = if_else(
          !is_rest,
          population_pre + population_te,
          NA_real_
        )
      )
    # Set Rest as residual
    non_rest_pop_post <- sum(loc_data$population_post[!loc_data$is_rest], na.rm = TRUE)
    loc_data$population_post[ref_idx] <- total_pop_pre - non_rest_pop_post

    # Population shares
    loc_data <- loc_data |>
      mutate(
        pop_share_pre  = population_pre / total_pop_pre,
        pop_share_post = population_post / total_pop_pre
      )

    # ------ Convert income to per-person (employment-rate adjusted) ------
    # The resident choice model requires y[k,t] = annual income per person.
    # Data provides median income per worker. Per-person income is:
    #   y_pp[k,t] = (E[k,t] / N[k,t]) * y_pw[k,t]
    # This ensures that when displaced jobs move to Rest of Sydney (raising
    # its employment) while population stays constant, per-person income
    # rises — the channel the displacement robustness check is designed
    # to test.
    loc_data <- loc_data |>
      mutate(
        emp_rate_pre  = employment_pre / population_pre,
        emp_rate_post = employment_post / population_post,
        income_pre    = emp_rate_pre  * income_pw_pre,
        income_post   = emp_rate_post * income_pw_post,
        income_te     = income_post - income_pre
      )

    # ------ Validation checks ------
    checks_passed <- TRUE
    issues <- character()

    if (any(loc_data$employment_post < 0, na.rm = TRUE)) {
      issues <- c(issues, "employment_post < 0 for some locations")
      checks_passed <- FALSE
    }
    if (any(loc_data$population_post <= 0, na.rm = TRUE)) {
      issues <- c(issues, "population_post <= 0 for some locations")
      checks_passed <- FALSE
    }
    if (any(is.na(loc_data$income_post))) {
      issues <- c(issues, "income_post is NA for some locations")
      checks_passed <- FALSE
    }
    if (any(is.na(loc_data$rent_post))) {
      issues <- c(issues, "rent_post is NA for some locations")
      checks_passed <- FALSE
    }
    if (any(loc_data$pop_share_pre <= 0, na.rm = TRUE)) {
      issues <- c(issues, "pop_share_pre <= 0")
      checks_passed <- FALSE
    }
    if (any(loc_data$pop_share_post <= 0, na.rm = TRUE)) {
      issues <- c(issues, "pop_share_post <= 0")
      checks_passed <- FALSE
    }
    tol <- 1e-6
    if (abs(sum(loc_data$population_post) - total_pop_pre) > tol) {
      issues <- c(issues, "population_post does not sum to pre-treatment total")
      checks_passed <- FALSE
    }
    if (abs(sum(loc_data$pop_share_pre) - 1) > tol) {
      issues <- c(issues, "pop_share_pre does not sum to 1")
      checks_passed <- FALSE
    }
    if (abs(sum(loc_data$pop_share_post) - 1) > tol) {
      issues <- c(issues, "pop_share_post does not sum to 1")
      checks_passed <- FALSE
    }
    if (!checks_passed) {
      warning(paste0(
        "welfare_analysis [", geo_label, "]: validation issues:\n  ",
        paste(issues, collapse = "\n  ")
      ))
    }

    # ------ Treatment effects for shares ------
    loc_data <- loc_data |>
      mutate(
        pop_share_te = pop_share_post - pop_share_pre
      )

    # ------ Percentage treatment effects ------
    safe_pct <- function(te, base) {
      if_else(abs(base) < 1e-10, NA_real_, 100 * te / base)
    }

    loc_data <- loc_data |>
      mutate(
        employment_te_pct = safe_pct(employment_te, employment_pre),
        income_te_pct     = safe_pct(income_te, income_pre),
        rent_te_pct       = safe_pct(rent_te, rent_pre),
        population_te_pct = if_else(
          is_rest,
          NA_real_,
          safe_pct(population_post - population_pre, population_pre)
        ),
        pop_share_te_pct  = safe_pct(pop_share_te, pop_share_pre)
      )

    # For Rest, population_te is the residual change, not an SC estimate
    loc_data <- loc_data |>
      mutate(
        population_te_display = if_else(is_rest, NA_real_, population_te)
      )

    # ------ Recover amenities (Sections 6–7) ------
    # Reference location: a[ref] = 0 in both periods
    # For non-ref k:
    #   a_pre[k] = sigma*(log(s_pre[k]) - log(s_pre[ref]))
    #              - ((y_pre[k] - r_pre[k]) - (y_pre[ref] - r_pre[ref]))

    ref_log_share_pre  <- log(loc_data$pop_share_pre[ref_idx])
    ref_log_share_post <- log(loc_data$pop_share_post[ref_idx])
    ref_netinc_pre     <- loc_data$income_pre[ref_idx] - loc_data$rent_pre[ref_idx]
    ref_netinc_post    <- loc_data$income_post[ref_idx] - loc_data$rent_post[ref_idx]

    loc_data <- loc_data |>
      mutate(
        amenity_pre = if_else(
          is_rest,
          0,
          sigma * (log(pop_share_pre) - ref_log_share_pre) -
            ((income_pre - rent_pre) - ref_netinc_pre)
        ),
        amenity_post = if_else(
          is_rest,
          0,
          sigma * (log(pop_share_post) - ref_log_share_post) -
            ((income_post - rent_post) - ref_netinc_post)
        ),
        delta_amenity = amenity_post - amenity_pre
      )

    # ------ Deterministic resident welfare (Section 8) ------
    loc_data <- loc_data |>
      mutate(
        V_pre  = income_pre - rent_pre + amenity_pre,
        V_post = income_post - rent_post + amenity_post,
        delta_V = V_post - V_pre
      )

    # ------ Citywide expected resident welfare (Section 9) ------
    # EU = sigma * log(sum_k exp(V[k] / sigma))  [numerically stable]
    EU_pre  <- sigma * log_sum_exp(loc_data$V_pre / sigma)
    EU_post <- sigma * log_sum_exp(loc_data$V_post / sigma)
    delta_EU <- EU_post - EU_pre

    # ------ Worker welfare (Section 10) ------
    # Worker welfare uses per-worker income (not per-person), since it
    # represents the total wage bill: E[k] * y_pw[k].
    loc_data <- loc_data |>
      mutate(
        worker_welfare_pre  = employment_pre * income_pw_pre,
        worker_welfare_post = employment_post * income_pw_post,
        delta_worker_welfare = worker_welfare_post - worker_welfare_pre
      )

    worker_welfare_pre_total  <- sum(loc_data$worker_welfare_pre)
    worker_welfare_post_total <- sum(loc_data$worker_welfare_post)
    delta_worker_welfare_total <- worker_welfare_post_total - worker_welfare_pre_total

    # ------ Total welfare (Section 11) ------
    total_pop_weight <- total_pop_pre

    resident_welfare_pre_total  <- EU_pre * total_pop_weight
    resident_welfare_post_total <- EU_post * total_pop_weight
    delta_resident_welfare_total <- resident_welfare_post_total - resident_welfare_pre_total

    total_welfare_pre  <- worker_welfare_pre_total + resident_welfare_pre_total
    total_welfare_post <- worker_welfare_post_total + resident_welfare_post_total
    delta_total_welfare <- total_welfare_post - total_welfare_pre

    # ------ Build inputs output (File 1) ------
    inputs_df <- loc_data |>
      transmute(
        location = location,
        employment_pre,
        employment_te,
        employment_te_pct,
        employment_post,
        population_pre,
        population_te = population_te_display,
        population_te_pct,
        population_post,
        pop_share_pre,
        pop_share_te,
        pop_share_te_pct,
        pop_share_post,
        income_pre,
        income_te,
        income_te_pct,
        income_post,
        rent_pre,
        rent_te,
        rent_te_pct,
        rent_post
      )

    # ------ Build results output (File 2) ------
    results_loc <- loc_data |>
      transmute(
        location = location,
        sigma = sigma,
        amenity_pre,
        amenity_post,
        delta_amenity,
        V_pre,
        V_post,
        delta_V,
        worker_welfare_pre,
        worker_welfare_post,
        delta_worker_welfare,
        # Citywide-only columns: leave NA for location rows
        EU_pre                    = NA_real_,
        EU_post                   = NA_real_,
        delta_EU                  = NA_real_,
        resident_welfare_pre_total  = NA_real_,
        resident_welfare_post_total = NA_real_,
        delta_resident_welfare_total = NA_real_,
        worker_welfare_pre_total    = NA_real_,
        worker_welfare_post_total   = NA_real_,
        delta_worker_welfare_total  = NA_real_,
        total_welfare_pre           = NA_real_,
        total_welfare_post          = NA_real_,
        delta_total_welfare         = NA_real_
      )

    # Citywide total row
    total_row <- tibble(
      location = "Sydney_Total",
      sigma    = sigma,
      amenity_pre  = NA_real_,
      amenity_post = NA_real_,
      delta_amenity = NA_real_,
      V_pre  = NA_real_,
      V_post = NA_real_,
      delta_V = NA_real_,
      worker_welfare_pre  = NA_real_,
      worker_welfare_post = NA_real_,
      delta_worker_welfare = NA_real_,
      EU_pre  = EU_pre,
      EU_post = EU_post,
      delta_EU = delta_EU,
      resident_welfare_pre_total  = resident_welfare_pre_total,
      resident_welfare_post_total = resident_welfare_post_total,
      delta_resident_welfare_total = delta_resident_welfare_total,
      worker_welfare_pre_total    = worker_welfare_pre_total,
      worker_welfare_post_total   = worker_welfare_post_total,
      delta_worker_welfare_total  = delta_worker_welfare_total,
      total_welfare_pre           = total_welfare_pre,
      total_welfare_post          = total_welfare_post,
      delta_total_welfare         = delta_total_welfare
    )

    results_df <- bind_rows(results_loc, total_row)

    # ------ Validation printout (Section 16) ------
    message(sprintf("--- %s validation ---", geo_label))
    message(sprintf("  Sum pop_share_pre:  %.8f", sum(loc_data$pop_share_pre)))
    message(sprintf("  Sum pop_share_post: %.8f", sum(loc_data$pop_share_post)))
    message(sprintf("  Reference location: %s", loc_data$location[ref_idx]))
    message(sprintf("  sigma:              %d", sigma))
    message(sprintf("  Rest emp_rate_pre:  %.6f  emp_rate_post: %.6f",
                    loc_data$emp_rate_pre[ref_idx],
                    loc_data$emp_rate_post[ref_idx]))
    message(sprintf("  Rest income_pp_pre: %.2f  income_pp_post: %.2f",
                    loc_data$income_pre[ref_idx],
                    loc_data$income_post[ref_idx]))
    message(sprintf("  EU_pre:             %.2f", EU_pre))
    message(sprintf("  EU_post:            %.2f", EU_post))
    message(sprintf("  delta_EU:           %.2f", delta_EU))
    message(sprintf("  delta_worker_welfare_total: %.0f", delta_worker_welfare_total))
    message(sprintf("  delta_total_welfare:        %.0f", delta_total_welfare))

    list(inputs = inputs_df, results = results_df)
  }

  # ===========================================================================
  # 7. Build sigma grid and displacement-fraction grid for robustness
  # ===========================================================================
  sigma_values <- unique(c(sigma, sigma_robustness))
  if (is.null(displacement_fractions)) displacement_fractions <- 0

  # ===========================================================================
  # 8. Iterate over donor pools × displacement fractions × sigma values
  # ===========================================================================
  pools <- sort(unique(att_detail$pool_id))

  all_inputs_3loc   <- list()
  all_results_3loc  <- list()
  all_inputs_16loc  <- list()
  all_results_16loc <- list()

  run_idx <- 0L

  for (p in pools) {

    att_p <- att_detail |> filter(pool_id == p)

    # -----------------------------------------------------------------------
    # 8a. Extract per-SA2 treatment effects (same for all scenarios)
    # -----------------------------------------------------------------------
    delta_E_tbl <- att_p |>
      filter(outcome_type == "Employment", industry == "Total") |>
      mutate(delta_E = att_raw * 1000) |>
      select(sa2_code = treated_code, delta_E)

    # Income TE is already in annual dollars (median_income_dollars)
    delta_y_tbl <- att_p |>
      filter(outcome_type == "Median income", industry == "Total") |>
      select(sa2_code = treated_code, delta_y = att_raw)

    delta_N_tbl <- att_p |>
      filter(outcome_type == "Population", industry == "Population") |>
      select(sa2_code = treated_code, delta_N = att_raw)

    # Rent TE is in monthly per-household dollars; convert to annual per-person
    # using the same avg_hh_size as the baseline
    delta_R_tbl <- att_p |>
      filter(outcome_type == "Rent", industry == "Rent (monthly avg)") |>
      select(sa2_code = treated_code, delta_R_monthly_hh = att_raw)

    # Merge with base_sa2 to get avg_hh_size for rent conversion
    delta_R_tbl <- delta_R_tbl |>
      left_join(
        base_sa2 |> select(sa2_code, avg_hh_size),
        by = "sa2_code"
      ) |>
      mutate(
        delta_R = if_else(
          !is.na(avg_hh_size) & avg_hh_size > 0,
          (delta_R_monthly_hh * 12) / avg_hh_size,
          NA_real_
        )
      ) |>
      select(sa2_code, delta_R)

    # -----------------------------------------------------------------------
    # 8b. Build the 15 SA2 rows (CBD + Inner Ring) — shared across scenarios
    # -----------------------------------------------------------------------
    sa2_15 <- base_sa2 |>
      filter(sa2_code %in% c(CBD_SA2S, INNER_RING_SA2S)) |>
      left_join(delta_E_tbl, by = "sa2_code") |>
      left_join(delta_y_tbl, by = "sa2_code") |>
      left_join(delta_N_tbl, by = "sa2_code") |>
      left_join(delta_R_tbl, by = "sa2_code") |>
      mutate(
        across(c(delta_E, delta_y, delta_N, delta_R), ~ replace_na(.x, 0)),
        across(c(E0, N0, y0, R0), ~ replace_na(.x, 0))
      )

    # Net employment change in CBD + Inner Ring (typically negative)
    net_emp_te_treated <- sum(sa2_15$delta_E)

    # Rest of Greater Sydney baseline
    rest_codes <- setdiff(sydney_sa2s, c(CBD_SA2S, INNER_RING_SA2S))
    rest_data  <- base_sa2 |> filter(sa2_code %in% rest_codes)

    rest_E0 <- sum(rest_data$E0, na.rm = TRUE)
    rest_N0 <- sum(rest_data$N0, na.rm = TRUE)
    rest_y0 <- wmean(rest_data$y0, rest_data$E0)
    rest_R0 <- wmean(rest_data$R0, rest_data$N0)

    # Zone aggregation for 3-location version
    sa2_15_with_zone <- sa2_15 |>
      mutate(zone = if_else(sa2_code %in% CBD_SA2S, "CBD", "Inner_Ring"))

    # -----------------------------------------------------------------------
    # 8c. Loop over displacement fractions × sigma values
    # -----------------------------------------------------------------------
    for (disp_frac in displacement_fractions) {
      for (sig in sigma_values) {

        run_idx <- run_idx + 1L

        # Label for this scenario
        scenario_tag <- sprintf("disp%.0fpct_sigma%d", disp_frac * 100, sig)

        # Rest employment TE: displaced fraction of net CBD+Inner Ring
        # employment change. net_emp_te_treated is negative (net job loss),
        # so negating gives the positive number of jobs re-created in Rest.
        rest_employment_te <- -net_emp_te_treated * disp_frac

        message(sprintf(
          "\n=== Pool %s | displacement=%.0f%% | sigma=%d ===",
          p, disp_frac * 100, sig
        ))
        message(sprintf(
          "  Net emp TE in CBD+InnerRing: %.0f | Offset to Rest: %.0f",
          net_emp_te_treated, rest_employment_te
        ))

        # ----- 16-location version -----
        sa2_15_loc <- sa2_15 |>
          transmute(
            location       = sa2_name,
            employment_pre = E0,
            employment_te  = delta_E,
            population_pre = N0,
            population_te  = delta_N,
            income_pre     = y0,
            income_te      = delta_y,
            rent_pre       = R0,
            rent_te        = delta_R,
            is_rest        = FALSE
          )

        rest_row_16 <- tibble(
          location       = "Rest_of_Sydney",
          employment_pre = rest_E0,
          employment_te  = rest_employment_te,
          population_pre = rest_N0,
          population_te  = 0,
          income_pre     = rest_y0,
          income_te      = 0,
          rent_pre       = rest_R0,
          rent_te        = 0,
          is_rest        = TRUE
        )

        loc_16 <- bind_rows(sa2_15_loc, rest_row_16)

        welfare_16 <- compute_welfare_one(
          loc_16, sig,
          sprintf("16loc pool=%s %s", p, scenario_tag)
        )

        welfare_16$inputs$pool_id  <- p
        welfare_16$results$pool_id <- p
        welfare_16$inputs$displacement_pct  <- disp_frac * 100
        welfare_16$results$displacement_pct <- disp_frac * 100
        all_inputs_16loc[[run_idx]]  <- welfare_16$inputs
        all_results_16loc[[run_idx]] <- welfare_16$results

        # ----- 3-location version -----
        zone_agg <- sa2_15_with_zone |>
          group_by(zone) |>
          summarise(
            employment_pre = sum(E0),
            employment_te  = sum(delta_E),
            population_pre = sum(N0),
            population_te  = sum(delta_N),
            income_pre     = wmean(y0, E0),
            income_te      = wmean(delta_y, E0),
            rent_pre       = wmean(R0, N0),
            rent_te        = wmean(delta_R, N0),
            .groups        = "drop"
          ) |>
          mutate(
            location = zone,
            is_rest  = FALSE
          ) |>
          select(-zone)

        rest_row_3 <- tibble(
          location       = "Rest_of_Sydney",
          employment_pre = rest_E0,
          employment_te  = rest_employment_te,
          population_pre = rest_N0,
          population_te  = 0,
          income_pre     = rest_y0,
          income_te      = 0,
          rent_pre       = rest_R0,
          rent_te        = 0,
          is_rest        = TRUE
        )

        loc_3 <- bind_rows(zone_agg, rest_row_3)

        welfare_3 <- compute_welfare_one(
          loc_3, sig,
          sprintf("3loc pool=%s %s", p, scenario_tag)
        )

        welfare_3$inputs$pool_id  <- p
        welfare_3$results$pool_id <- p
        welfare_3$inputs$displacement_pct  <- disp_frac * 100
        welfare_3$results$displacement_pct <- disp_frac * 100
        all_inputs_3loc[[run_idx]]  <- welfare_3$inputs
        all_results_3loc[[run_idx]] <- welfare_3$results
      }
    }
  }

  # ===========================================================================
  # 9. Write outputs
  # ===========================================================================
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  out_inputs_3loc   <- bind_rows(all_inputs_3loc)
  out_results_3loc  <- bind_rows(all_results_3loc)
  out_inputs_16loc  <- bind_rows(all_inputs_16loc)
  out_results_16loc <- bind_rows(all_results_16loc)

  path_inputs_3loc   <- file.path(out_dir, "lockout_welfare_inputs_3loc.csv")
  path_results_3loc  <- file.path(out_dir, "lockout_welfare_results_3loc.csv")
  path_inputs_16loc  <- file.path(out_dir, "lockout_welfare_inputs_16loc.csv")
  path_results_16loc <- file.path(out_dir, "lockout_welfare_results_16loc.csv")

  write_csv(out_inputs_3loc,   path_inputs_3loc)
  write_csv(out_results_3loc,  path_results_3loc)
  write_csv(out_inputs_16loc,  path_inputs_16loc)
  write_csv(out_results_16loc, path_results_16loc)

  message(sprintf("welfare_analysis: wrote %s", path_inputs_3loc))
  message(sprintf("welfare_analysis: wrote %s", path_results_3loc))
  message(sprintf("welfare_analysis: wrote %s", path_inputs_16loc))
  message(sprintf("welfare_analysis: wrote %s", path_results_16loc))

  # --- Robustness summary ---
  n_sigma  <- length(sigma_values)
  n_disp   <- length(displacement_fractions)
  n_pools  <- length(pools)
  n_combos <- n_pools * n_disp * n_sigma
  message(sprintf(
    "\nwelfare_analysis: %d scenarios total (%d pools × %d displacement × %d sigma)",
    n_combos, n_pools, n_disp, n_sigma
  ))
  message(sprintf("  sigma values: %s", paste(sigma_values, collapse = ", ")))
  message(sprintf("  displacement fractions: %s",
                  paste(sprintf("%.0f%%", displacement_fractions * 100), collapse = ", ")))

  c(path_inputs_3loc, path_results_3loc, path_inputs_16loc, path_results_16loc)
}
