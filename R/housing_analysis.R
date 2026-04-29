# R/housing_analysis.R
# Back-of-the-envelope weighted housing cost effect
#
# Computes the difference in % change in rent and mortgage payments (2011→2016)
# between treated/spillover units and their synthetic controls.
# Weights: total employment (2011) and population (2011).
#
# Writes two CSV files to out_dir:
#   housing_effect_by_sa2.csv        — gap per SA2 × pool × payment_type
#   housing_effect_weighted_summary.csv — weighted avg gap (emp & pop weights)
#
# Returns a character vector of written file paths (for targets format = "file").

compute_housing_cost_effect <- function(
    census_housing,
    emp,
    pop,
    sc_weights_dir,  # directory containing sc_weights_P1.csv, _P2.csv, _P3.csv
    out_dir = "outputs/housing"
) {
  
  # ---------------------------------------------------------------------------
  # 1. Percent change 2011 → 2016 for every SA2 × payment_type
  # ---------------------------------------------------------------------------
  pct_change <- census_housing |>
    filter(year %in% c(2011L, 2016L), !is.na(avg_monthly_payment)) |>
    arrange(sa2_code, payment_type, year) |>
    group_by(sa2_code, sa2_name, payment_type,
             treated_full, treated_partial, treated) |>
    filter(all(c(2011L, 2016L) %in% year)) |>
    summarise(
      val_2011 = avg_monthly_payment[year == 2011L],
      val_2016 = avg_monthly_payment[year == 2016L],
      pct_change = (val_2016 - val_2011) / val_2011 * 100,
      .groups = "drop"
    ) |>
    filter(!is.na(pct_change))
  
  # ---------------------------------------------------------------------------
  # 2. SC donor weights — read all three pool files and bind
  #    Each file schema: pool_id, treated_code, sa2_code (donor), weight, sa2_name, state_name, unit_role, industry
  # ---------------------------------------------------------------------------
  weight_files <- list.files(sc_weights_dir, pattern = "^sc_weights_P\\d+\\.csv$",
                             full.names = TRUE)
  if (length(weight_files) == 0) {
    stop("No sc_weights_P*.csv files found in: ", sc_weights_dir)
  }
  
  sc_weights <- map(weight_files, ~ read.csv(.x, stringsAsFactors = FALSE)) |>
    list_rbind() |>
    as_tibble() |>
    mutate(
      treated_code = as.character(treated_code),
      sa2_code     = as.character(sa2_code)       # donor SA2
    ) |>
    filter(industry == "Total")
  
  # ---------------------------------------------------------------------------
  # 3. Employment weight: Total jobs in 2011 for each SA2
  # ---------------------------------------------------------------------------
  emp_weights <- emp |>
    filter(year_start == 2011L, industry == "Total") |>
    select(sa2_code, emp_2011 = jobs_000) |>
    mutate(
      sa2_code = as.character(sa2_code),
      emp_2011 = suppressWarnings(as.numeric(emp_2011))
    ) |>
    filter(!is.na(emp_2011), emp_2011 > 0)
  
  # ---------------------------------------------------------------------------
  # 4. Population weight: ERP in 2011
  # ---------------------------------------------------------------------------
  pop_weights <- pop |>
    filter(year == 2011L) |>
    select(sa2_code, pop_2011 = population) |>
    mutate(
      sa2_code = as.character(sa2_code),
      pop_2011 = suppressWarnings(as.numeric(pop_2011))
    ) |>
    filter(!is.na(pop_2011), pop_2011 > 0)
  
  # ---------------------------------------------------------------------------
  # 5. Compute synthetic % change for each treated/spillover unit
  #    Synthetic % change = weighted average of donor % changes using SC weights
  # ---------------------------------------------------------------------------
  sc_weights_joined <- sc_weights |>
    left_join(
      pct_change |> select(sa2_code, payment_type, pct_change),
      by = "sa2_code"
    ) |>
    group_by(pool_id, treated_code, treated_name, unit_role, payment_type) |>
    summarise(
      synth_pct_change = sum(weight * pct_change, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------------------------------------------------------------------------
  # 6. Join actual % change for treated/spillover units with synthetic
  # ---------------------------------------------------------------------------
  treated_pct <- pct_change |>
    filter(sa2_code %in% sc_weights$treated_code) |>
    select(sa2_code, payment_type, actual_pct_change = pct_change)
  
  # Also include spillover units — join via sc_weights unit_role
  combined <- sc_weights_joined |>
    left_join(
      treated_pct,
      by = c("treated_code" = "sa2_code", "payment_type")
    ) |>
    mutate(
      gap_pct_change = actual_pct_change - synth_pct_change
    ) |>
    left_join(emp_weights, by = c("treated_code" = "sa2_code")) |>
    left_join(pop_weights, by = c("treated_code" = "sa2_code"))
  
  # ---------------------------------------------------------------------------
  # 7. Weighted average gap across units, by pool × unit_role × payment_type
  # ---------------------------------------------------------------------------
  weighted_summary <- bind_rows(
    # Employment-weighted
    combined |>
      group_by(pool_id, unit_role, payment_type) |>
      summarise(
        weighted_gap = weighted.mean(gap_pct_change, w = emp_2011, na.rm = TRUE),
        n_units      = n(),
        weight_type  = "Employment (2011 total jobs)",
        .groups = "drop"
      ),
    # Population-weighted
    combined |>
      group_by(pool_id, unit_role, payment_type) |>
      summarise(
        weighted_gap = weighted.mean(gap_pct_change, w = pop_2011, na.rm = TRUE),
        n_units      = n(),
        weight_type  = "Population (2011 ERP)",
        .groups = "drop"
      )
  ) |>
    arrange(pool_id, unit_role, payment_type, weight_type)
  
  # ---------------------------------------------------------------------------
  # 8. Write output files
  # ---------------------------------------------------------------------------
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  path_by_sa2  <- file.path(out_dir, "housing_effect_by_sa2.csv")
  path_summary <- file.path(out_dir, "housing_effect_weighted_summary.csv")
  
  # Per-SA2 detail: retain gap components and weights for welfare decomposition
  write_csv(
    combined |> select(pool_id, unit_role, treated_code, treated_name, payment_type,
                       synth_pct_change, actual_pct_change, gap_pct_change,
                       emp_2011, pop_2011),
    path_by_sa2
  )
  
  write_csv(weighted_summary, path_summary)
  
  message(sprintf("housing_analysis: wrote %s", path_by_sa2))
  message(sprintf("housing_analysis: wrote %s", path_summary))
  
  # Return file paths so targets can track them with format = "file"
  c(path_by_sa2, path_summary)
}