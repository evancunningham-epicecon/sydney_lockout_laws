# =============================================================================
# 3. Helper: read one sheet, strip header rows, attach year
# =============================================================================
 
read_abs_sheet <- function(file_path, sheet_name, fiscal_year, col_names) {
 
  raw <- read_excel(
    path      = file_path,
    sheet     = sheet_name,
    col_names = FALSE,
    # Rows 1–8 are header/metadata; data starts at row 9
    skip      = 8
  )
 
  # Drop fully-empty rows (section label rows in the ABS layout)
  raw <- raw |>
    filter(if_any(everything(), ~ !is.na(.x)))
 
  # Keep the first 42 columns (SA2 code, SA2 name, 20 job cols, 20 income cols)
  # and assign clean names
  raw <- raw |>
    select(1:42) |>
    set_names(col_names)
 
  # Remove rows that are state/territory label rows:
  # these have text in sa2_code but no numeric SA2 code
  raw <- raw |>
    filter(str_detect(sa2_code, "^\\d+$"))
 
  raw |>
    mutate(
      fiscal_year = fiscal_year,
      sa2_code    = as.character(sa2_code)
    )
}
 