# R/ingest.R
# Ingest function for ABS Jobs in Australia: Tables 3.8-3.14 (SA2 level)
# Called by _targets.R — not run directly

ingest_abs_employment <- function(file_path) {

  # ---------------------------------------------------------------------------
  # 1. Sheet and year metadata
  #    Tables 3.8 to 3.14 are in REVERSE order: 3.8 = 2017-18, 3.14 = 2011-12
  # ---------------------------------------------------------------------------

  sheet_meta <- tribble(
    ~sheet,      ~fiscal_year,
    "Table 3.8",  "2017-18",
    "Table 3.9",  "2016-17",
    "Table 3.10", "2015-16",
    "Table 3.11", "2014-15",
    "Table 3.12", "2013-14",
    "Table 3.13", "2012-13",
    "Table 3.14", "2011-12"
  )

  # ---------------------------------------------------------------------------
  # 2. Industry names (in ABS column order)
  # ---------------------------------------------------------------------------

  industries <- c(
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
    "Other services",
    "Total"
  )

  jobs_cols   <- paste0("jobs_", industries)
  income_cols <- paste0("income_", industries)
  col_names   <- c("sa2_code", "sa2_name", jobs_cols, income_cols)

  # ---------------------------------------------------------------------------
  # 3. Read and bind all seven sheets
  # ---------------------------------------------------------------------------

  emp_wide <- sheet_meta |>
    pmap(~ read_abs_sheet(file_path, ..1, ..2, col_names)) |>
    list_rbind()

  # ---------------------------------------------------------------------------
  # 4. Pivot jobs to long
  # ---------------------------------------------------------------------------

  jobs_long <- emp_wide |>
    select(sa2_code, sa2_name, fiscal_year, all_of(jobs_cols)) |>
    pivot_longer(
      cols      = all_of(jobs_cols),
      names_to  = "industry",
      values_to = "jobs_000"
    ) |>
    mutate(industry = str_remove(industry, "^jobs_"))

  # ---------------------------------------------------------------------------
  # 5. Pivot income to long
  # ---------------------------------------------------------------------------

  income_long <- emp_wide |>
    select(sa2_code, sa2_name, fiscal_year, all_of(income_cols)) |>
    pivot_longer(
      cols      = all_of(income_cols),
      names_to  = "industry",
      values_to = "median_income_dollars"
    ) |>
    mutate(industry = str_remove(industry, "^income_"))

  # ---------------------------------------------------------------------------
  # 6. Join, coerce types, and sort
  # ---------------------------------------------------------------------------

  jobs_long |>
    left_join(income_long, by = c("sa2_code", "sa2_name", "fiscal_year", "industry")) |>
    mutate(
      jobs_000              = suppressWarnings(as.numeric(jobs_000)),
      median_income_dollars = suppressWarnings(as.numeric(median_income_dollars)),
      year_start            = as.integer(str_extract(fiscal_year, "^\\d{4}")),
      industry              = factor(industry, levels = industries)
    ) |>
    arrange(sa2_code, year_start, industry)
}
