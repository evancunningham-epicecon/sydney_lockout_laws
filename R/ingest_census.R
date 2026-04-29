# R/ingest_census.R
# Ingests ABS Census 2011–2018 family/community data (SA2 level)
# Keeps only rent and mortgage payment variables (available 2011 & 2016 only)
# Returns a tidy long tibble: one row per SA2 × year × payment_type

ingest_abs_census_housing <- function(file_path) {
  
  raw <- read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE) |>
    as_tibble()
  
  raw |>
    select(
      sa2_code = sa2_maincode_2016,
      sa2_name = sa2_name_2016,
      year     = yr,
      avg_monthly_rent     = rnt_mrtgge_pymnts_cnss_avrge_mnthly_hshld_rntl_pymnt,
      avg_monthly_mortgage = rent_mortgage_payments_census_average_monthly_household_payment
    ) |>
    mutate(
      sa2_code             = as.character(sa2_code),
      year                 = as.integer(year),
      avg_monthly_rent     = suppressWarnings(as.numeric(avg_monthly_rent)),
      avg_monthly_mortgage = suppressWarnings(as.numeric(avg_monthly_mortgage))
    ) |>
    # Drop rows where both measures are NA (non-Census years in the panel)
    filter(!is.na(avg_monthly_rent) | !is.na(avg_monthly_mortgage)) |>
    # Keep only the two Census years where these variables exist
    filter(year %in% c(2011L, 2016L)) |>
    pivot_longer(
      cols      = c(avg_monthly_rent, avg_monthly_mortgage),
      names_to  = "payment_type",
      values_to = "avg_monthly_payment"
    ) |>
    mutate(
      payment_type = recode(payment_type,
                            avg_monthly_rent     = "Rent",
                            avg_monthly_mortgage = "Mortgage"
      )
    ) |>
    add_state_labels() |>
    add_treatment_indicators() |>
    arrange(sa2_code, payment_type, year)
}