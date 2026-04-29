# R/att_weighted_summary.R
# Aggregate SC treatment effects for employment, median income, population,
# and rent — with base-period levels, percent change, and raw change.
#
# Produces two CSV outputs:
#   att_detail_by_sa2.csv       — one row per SA2 × pool × outcome
#   att_aggregate_summary.csv   — weighted averages across SA2s by unit_role
#
# Weighting:
#   Employment outcomes → weighted by base-period employment in the SAME industry
#   Income outcomes     → weighted by base-period employment in the SAME industry
#                          (workers earning that income)
#   Population          → weighted by base-period population
#   Rent                → weighted by base-period population (residents who pay)
#
# Aggregation:
#   For ADDITIVE quantities (employment, population):
#     total_att_raw = sum of per-SA2 raw changes (total aggregate impact)
#     total_base    = sum of per-SA2 base values (total in group)
#   For NON-ADDITIVE quantities (median income, rent):
#     total_att_raw = weighted average of per-SA2 raw changes (representative change)
#     total_base    = weighted average of per-SA2 base values (representative level)
#
# Returns a character vector of written file paths (for targets format = "file").

compute_att_weighted_summary <- function(
    sc_att,            # sc_att_numeric tibble (or path to CSV)
    emp,               # emp_sa2_csv_tidy tibble (has jobs_000 and median_income_dollars)
    pop,               # population tibble: sa2_code, year, population
    census_housing,    # census_housing_tidy tibble from ingest_abs_census_housing()
    sc_weights_dir = "outputs/sc",
    out_dir        = "outputs/att_summary",
    .sc_figures_dep = NULL
) {
  
  # ===========================================================================
  # 0. Load sc_att if supplied as a path
  # ===========================================================================
  if (is.character(sc_att) && length(sc_att) == 1 && file.exists(sc_att)) {
    sc_att <- read_csv(sc_att, show_col_types = FALSE)
  }
  sc_att <- sc_att |>
    mutate(treated_code = as.character(treated_code))
  
  # ===========================================================================
  # 1. Base-period levels
  #    Employment & income: 2012-13 (year_start = 2012), last pre-treatment FY
  #    Population: 2012 (calendar year, aligns with employment)
  #    Rent: 2011 (Census year, only available 2011 & 2016)
  # ===========================================================================
  BASE_EMP_YEAR <- 2012L
  BASE_POP_YEAR <- 2012L
  
  emp_base <- emp |>
    filter(year_start == BASE_EMP_YEAR) |>
    mutate(
      sa2_code = as.character(sa2_code),
      industry = as.character(industry)
    ) |>
    select(sa2_code, industry,
           base_emp    = jobs_000,
           base_income = median_income_dollars) |>
    mutate(
      base_emp    = suppressWarnings(as.numeric(base_emp)),
      base_income = suppressWarnings(as.numeric(base_income))
    )
  
  pop_base <- pop |>
    filter(year == BASE_POP_YEAR) |>
    mutate(sa2_code = as.character(sa2_code)) |>
    select(sa2_code, base_pop = population) |>
    mutate(base_pop = suppressWarnings(as.numeric(base_pop)))
  
  rent_base <- census_housing |>
    filter(year == 2011L, payment_type == "Rent") |>
    mutate(sa2_code = as.character(sa2_code)) |>
    select(sa2_code, base_rent = avg_monthly_payment) |>
    mutate(base_rent = suppressWarnings(as.numeric(base_rent)))
  
  # ===========================================================================
  # 2. Rent treatment effect — recompute from census + SC weights
  # ===========================================================================
  rent_pct_change <- census_housing |>
    filter(year %in% c(2011L, 2016L), payment_type == "Rent",
           !is.na(avg_monthly_payment)) |>
    arrange(sa2_code, year) |>
    group_by(sa2_code, sa2_name) |>
    filter(all(c(2011L, 2016L) %in% year)) |>
    summarise(
      val_2011   = avg_monthly_payment[year == 2011L],
      val_2016   = avg_monthly_payment[year == 2016L],
      pct_change = (val_2016 - val_2011) / val_2011 * 100,
      .groups    = "drop"
    ) |>
    mutate(sa2_code = as.character(sa2_code)) |>
    filter(!is.na(pct_change))
  
  weight_files <- list.files(sc_weights_dir,
                             pattern = "^sc_weights_P\\d+\\.csv$",
                             full.names = TRUE)
  
  rent_att_by_sa2 <- NULL
  if (length(weight_files) > 0) {
    sc_weights <- map(weight_files, ~ read.csv(.x, stringsAsFactors = FALSE)) |>
      list_rbind() |>
      as_tibble() |>
      mutate(
        treated_code = as.character(treated_code),
        sa2_code     = as.character(sa2_code)
      ) |>
      filter(industry == "Total")
    
    synth_rent_pct <- sc_weights |>
      left_join(rent_pct_change |> select(sa2_code, pct_change),
                by = "sa2_code") |>
      group_by(pool_id, treated_code, treated_name, unit_role) |>
      summarise(synth_pct_change = sum(weight * pct_change, na.rm = TRUE),
                .groups = "drop")
    
    actual_rent_pct <- rent_pct_change |>
      filter(sa2_code %in% sc_weights$treated_code) |>
      select(sa2_code, actual_pct_change = pct_change)
    
    rent_att_by_sa2 <- synth_rent_pct |>
      left_join(actual_rent_pct,
                by = c("treated_code" = "sa2_code")) |>
      mutate(att_pct_rent = actual_pct_change - synth_pct_change) |>
      select(pool_id, treated_code, treated_name, unit_role, att_pct_rent)
  }
  
  # ===========================================================================
  # 3. Build per-SA2 detail table
  # ===========================================================================
  POOL_LABELS <- c(
    P1 = "All Australia (exc. Greater Sydney)",
    P2 = "All Australia exc. NSW",
    P3 = "Capital cities only (exc. Greater Sydney)"
  )
  
  # --- 3a. Employment and income ATTs ---
  # ATT is in index points (base = 100).
  # att_pct = att / 100 (proportion change relative to base)
  # att_raw = base_value * att_pct
  
  detail_emp_income <- sc_att |>
    mutate(
      is_income    = str_starts(industry, "Income: "),
      emp_industry = if_else(is_income,
                             str_remove(industry, "^Income: "),
                             as.character(industry)),
      outcome_type = if_else(is_income, "Median income", "Employment")
    ) |>
    filter(emp_industry != "Population") |>
    left_join(emp_base, by = c("treated_code" = "sa2_code",
                               "emp_industry" = "industry")) |>
    mutate(
      base_value = if_else(outcome_type == "Median income", base_income, base_emp),
      att_pct    = att / 100,
      att_raw    = base_value * att_pct
    ) |>
    select(pool_id, pool_label, treated_code, treated_name,
           treatment_status, unit_role,
           outcome_type, industry = emp_industry,
           att_index_pts = att, pre_mspe, n_donors,
           base_value, att_pct, att_raw)
  
  # --- 3b. Population ATTs ---
  detail_pop <- sc_att |>
    filter(industry == "Population") |>
    left_join(pop_base, by = c("treated_code" = "sa2_code")) |>
    mutate(
      outcome_type = "Population",
      base_value   = base_pop,
      att_pct      = att / 100,
      att_raw      = base_value * att_pct
    ) |>
    select(pool_id, pool_label, treated_code, treated_name,
           treatment_status, unit_role,
           outcome_type, industry,
           att_index_pts = att, pre_mspe, n_donors,
           base_value, att_pct, att_raw)
  
  # --- 3c. Rent ATTs ---
  detail_rent <- NULL
  if (!is.null(rent_att_by_sa2) && nrow(rent_att_by_sa2) > 0) {
    # att_pct_rent is in percentage points (e.g., 5.0 = 5pp).
    # Convert to proportion for att_pct and att_raw.
    unit_meta <- sc_att |>
      distinct(treated_code, treatment_status)
    
    detail_rent <- rent_att_by_sa2 |>
      left_join(rent_base, by = c("treated_code" = "sa2_code")) |>
      left_join(unit_meta, by = "treated_code") |>
      mutate(
        pool_label    = POOL_LABELS[pool_id],
        outcome_type  = "Rent",
        industry      = "Rent (monthly avg)",
        att_index_pts = att_pct_rent,
        pre_mspe      = NA_real_,
        n_donors      = NA_integer_,
        base_value    = base_rent,
        att_pct       = att_pct_rent / 100,
        att_raw       = base_rent * att_pct
      ) |>
      select(pool_id, pool_label, treated_code, treated_name,
             treatment_status, unit_role,
             outcome_type, industry,
             att_index_pts, pre_mspe, n_donors,
             base_value, att_pct, att_raw)
  }
  
  detail <- bind_rows(detail_emp_income, detail_pop, detail_rent) |>
    arrange(pool_id, unit_role, treated_code, outcome_type, industry)
  
  # ===========================================================================
  # 4. Aggregate summary
  # ===========================================================================
  
  # For additive outcomes (employment, population): just sum raw changes and
  # base values. The aggregate percent change = total_att_raw / total_base.
  # No weighting required — these are counts.
  #
  # For non-additive outcomes (median income, rent): weighted average of
  # per-SA2 effects, weighted by base employment (income) or base population
  # (rent).
  
  detail_with_wt <- detail |>
    left_join(emp_base |> select(sa2_code, industry, base_emp),
              by = c("treated_code" = "sa2_code", "industry")) |>
    left_join(pop_base |> select(sa2_code, base_pop),
              by = c("treated_code" = "sa2_code")) |>
    mutate(
      is_additive = outcome_type %in% c("Employment", "Population"),
      agg_weight = case_when(
        outcome_type == "Median income" ~ base_emp,
        outcome_type == "Rent"          ~ base_pop,
        TRUE                            ~ NA_real_   # not used for additive
      )
    )
  
  agg_additive <- detail_with_wt |>
    filter(is_additive, !is.na(att_index_pts), !is.na(base_value)) |>
    group_by(pool_id, pool_label, unit_role, outcome_type, industry) |>
    summarise(
      n_units        = n(),
      agg_base_value = sum(base_value, na.rm = TRUE),
      agg_att_raw    = sum(att_raw, na.rm = TRUE),
      .groups        = "drop"
    ) |>
    mutate(
      weighted_att_pct = agg_att_raw / agg_base_value,
      weighted_att_idx = weighted_att_pct * 100
    )
  
  agg_nonadd <- detail_with_wt |>
    filter(!is_additive, !is.na(att_index_pts),
           !is.na(agg_weight), agg_weight > 0) |>
    group_by(pool_id, pool_label, unit_role, outcome_type, industry) |>
    summarise(
      n_units          = n(),
      weighted_att_idx = weighted.mean(att_index_pts, w = agg_weight, na.rm = TRUE),
      weighted_att_pct = weighted.mean(att_pct,       w = agg_weight, na.rm = TRUE),
      agg_att_raw      = weighted.mean(att_raw,       w = agg_weight, na.rm = TRUE),
      agg_base_value   = weighted.mean(base_value,    w = agg_weight, na.rm = TRUE),
      .groups          = "drop"
    )
  
  aggregate <- bind_rows(agg_additive, agg_nonadd) |>
    arrange(pool_id, unit_role, outcome_type, industry)
  
  
  # ===========================================================================
  # 5. Write outputs
  # ===========================================================================
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  path_detail    <- file.path(out_dir, "att_detail_by_sa2.csv")
  path_aggregate <- file.path(out_dir, "att_aggregate_summary.csv")
  
  write_csv(detail,    path_detail)
  write_csv(aggregate, path_aggregate)
  
  message(sprintf("att_weighted_summary: wrote %s", path_detail))
  message(sprintf("att_weighted_summary: wrote %s", path_aggregate))
  
  c(path_detail, path_aggregate)
}