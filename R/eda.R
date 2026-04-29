# R/eda.R
# EDA functions — called by _targets.R, not run directly

# SA2 codes for anecdotal interstate control suburbs shown in EDA charts.
# Northbridge WA has no standalone SA2 in this dataset; Perth City is used instead.
anecdotal_controls <- tibble::tribble(
  ~sa2_code,    ~control_label,
  "206071142",  "Fitzroy (Melb)",
  "206071141",  "Collingwood (Melb)",
  "206051133",  "St Kilda (Melb)",
  "305011106",  "Fortitude Valley (Bris)",
  "503021041",  "Perth City / Northbridge (Perth)"
)

# Spillover SA2s — untreated Sydney suburbs expected to absorb displaced
# late-night patronage. Shown in EDA charts as a third series type so
# pre-treatment trends for treated, spillover, and interstate controls can
# be visually compared before the SC analysis.
# These codes must stay in sync with SPILLOVER_SA2S in sc_analysis.R.
spillover_eda <- tibble::tribble(
  ~sa2_code,    ~spillover_label,
  "117031332",  "Newtown - C'down (Syd)",
  "117031331",  "Glebe (Syd)",
  "117031335",  "Redfern (Syd)",
  "117031338",  "Waterloo (Syd)",
  "117031330",  "Erskineville (Syd)",
  "120021388",  "Leichhardt (Syd)",
  "120021387",  "Balmain (Syd)",
  "120021389",  "Lilyfield (Syd)",
  "118011345",  "Paddington (Syd)",
  "118011341",  "Bondi Junction (Syd)"
)

# -----------------------------------------------------------------------------
# Helper: build indexed employment series
#   - Aggregates jobs across industries (or filters to one industry)
#   - Indexes to 100 at base_year (year_start == base_year)
#   - Returns treated SA2s + spillover SA2s + interstate controls in one tibble
#   - series_type: "Full treatment" | "Partial treatment" | "Spillover" | "Control"
# -----------------------------------------------------------------------------

build_index_data <- function(emp_sa2_tidy,
                             industry_filter = NULL,
                             base_year       = 2013L) {
  
  control_codes  <- anecdotal_controls$sa2_code
  spillover_codes <- spillover_eda$sa2_code
  
  # Subset to treated + spillover + anecdotal controls
  df <- emp_sa2_tidy |>
    filter(
      treated == 1 | sa2_code %in% control_codes | sa2_code %in% spillover_codes,
      industry != "Total"
    )
  
  # Apply industry filter if supplied; otherwise sum all industries
  if (!is.null(industry_filter)) {
    df <- df |> filter(as.character(industry) == industry_filter)
  }
  
  df <- df |>
    group_by(sa2_code, sa2_name, year_start, treated, treated_full, treated_partial) |>
    summarise(jobs_000 = sum(jobs_000, na.rm = TRUE), .groups = "drop")
  
  # Index to base year
  base <- df |>
    filter(year_start == base_year) |>
    select(sa2_code, base_jobs = jobs_000)
  
  df <- df |>
    left_join(base, by = "sa2_code") |>
    mutate(index = 100 * jobs_000 / base_jobs)
  
  # Attach readable labels — spillover join first so control_label stays NA
  # for spillover SA2s (they are not in anecdotal_controls)
  df |>
    left_join(anecdotal_controls, by = "sa2_code") |>
    left_join(spillover_eda,      by = "sa2_code") |>
    mutate(
      series_label = case_when(
        treated_full    == 1                    ~ paste0(sa2_name, " [treated]"),
        treated_partial == 1                    ~ paste0(sa2_name, " [partial]"),
        sa2_code %in% spillover_codes           ~ spillover_label,
        TRUE                                    ~ control_label
      ),
      series_type = case_when(
        treated_full    == 1                    ~ "Full treatment",
        treated_partial == 1                    ~ "Partial treatment",
        sa2_code %in% spillover_codes           ~ "Spillover",
        TRUE                                    ~ "Control"
      )
    )
}

# -----------------------------------------------------------------------------
# Helper: draw one indexed employment chart
# -----------------------------------------------------------------------------

draw_index_chart <- function(data, title_industry, base_year = 2013L) {
  
  treatment_year <- 2013L   # 2013-14 FY = onset year; vertical line between 2012 and 2013
  
  ggplot(data, aes(
    x        = year_start,
    y        = index,
    colour   = series_label,
    linetype = series_type,
    group    = series_label
  )) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 1.8) +
    geom_hline(yintercept = 100, colour = "grey70", linewidth = 0.4) +
    geom_vline(
      xintercept = treatment_year + 0.5,
      linetype   = "dashed",
      colour     = "grey30",
      linewidth  = 0.5
    ) +
    annotate(
      "text",
      x      = treatment_year + 0.55,
      y      = Inf,
      label  = "Feb 2014\n(2013-14 FY\npartially treated)",
      hjust  = 0,
      vjust  = 1.3,
      size   = 2.6,
      colour = "grey30"
    ) +
    scale_x_continuous(breaks = 2011:2018) +
    scale_linetype_manual(
      values = c(
        "Full treatment"    = "solid",
        "Partial treatment" = "dotdash",
        "Spillover"         = "twodash",
        "Control"           = "longdash"
      ),
      name = NULL
    ) +
    scale_colour_manual(
      values = c(
        # Treated — warm colours
        "Sydney - Haymarket - The Rocks [treated]" = "#D62728",
        "Darlinghurst [treated]"                   = "#E88A1A",
        "Potts Point - Woolloomooloo [treated]"    = "#8C1A6A",
        # Partial — muted greys
        "Surry Hills [partial]"                    = "#AAAAAA",
        "Pyrmont - Ultimo [partial]"               = "#CCCCCC",
        # Spillover — greens/teals (matches SPILLOVER_COLOURS in sc_analysis.R)
        "Newtown - C'down (Syd)"                   = "#1B7837",
        "Glebe (Syd)"                              = "#4DAC26",
        "Redfern (Syd)"                            = "#A6DBA0",
        "Waterloo (Syd)"                           = "#7FBF7B",
        "Erskineville (Syd)"                       = "#2166AC",
        "Leichhardt (Syd)"                         = "#4393C3",
        "Balmain (Syd)"                            = "#92C5DE",
        "Lilyfield (Syd)"                          = "#D1E5F0",
        "Paddington (Syd)"                         = "#8073AC",
        "Bondi Junction (Syd)"                     = "#B2ABD2",
        # Interstate controls — cool colours
        "Fitzroy (Melb)"                           = "#1F77B4",
        "Collingwood (Melb)"                       = "#17BECF",
        "St Kilda (Melb)"                          = "#2CA02C",
        "Fortitude Valley (Bris)"                  = "#9467BD",
        "Perth City / Northbridge (Perth)"         = "#5B8A3C"
      ),
      name = NULL
    ) +
    labs(
      title    = paste0("Indexed employee jobs — ", title_industry),
      subtitle = paste0(
        "Index: 100 = ", base_year, "-", substr(base_year + 1, 3, 4),
        " FY  |  ABS Jobs in Australia 2011–12 to 2018–19"
      ),
      x       = "Fiscal year start",
      y       = "Employment index (100 = base year)",
      caption = paste0(
        "Dashed vertical: lockout law onset (Feb 2014 falls in 2013-14 FY)\n",
        "Spillover: untreated Sydney suburbs expected to receive displaced patronage"
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "right",
      legend.text      = element_text(size = 7),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# -----------------------------------------------------------------------------
# Main EDA function: one PDF per industry
# -----------------------------------------------------------------------------

plot_indexed_employment <- function(emp_sa2_tidy, out_dir, base_year = 2013L) {
  
  fs::dir_create(out_dir)
  
  industries <- c(
    "All industries combined",
    "Accommodation and food services",
    "Arts and recreation services",
    "Retail trade"
  )
  
  paths <- purrr::map_chr(industries, function(ind) {
    
    ind_filter <- if (ind == "All industries combined") NULL else ind
    
    data <- build_index_data(emp_sa2_tidy, ind_filter, base_year)
    
    p <- draw_index_chart(data, ind, base_year)
    
    slug <- ind |>
      tolower() |>
      stringr::str_replace_all("[^a-z0-9]+", "_") |>
      stringr::str_remove("_$")
    
    path <- file.path(out_dir, paste0("eda_index_", slug, ".pdf"))
    ggsave(path, p, width = 11, height = 6)
    path
  })
  
  paths
}