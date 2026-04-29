# make_exec_summary_tables.R
# ------------------------------------------------------------------------------
# Generate LaTeX tables for the executive summary of
# "The Local Economic Impact of Sydney's Lockout Laws".
#
# Produces four self-contained, \input-able .tex files in outputs/paper/tables/:
#
#   tab_headline_att.tex      Aggregate ATTs, treated vs spillover, all outcomes
#   tab_treated_sa2.tex       Per-SA2 ATTs for the five directly-treated precincts
#   tab_spillover_sa2.tex     Per-SA2 ATTs for the key spillover precincts
#   tab_welfare_scenarios.tex Welfare decomposition across sigma and displacement
#
# All tables restrict to donor pool P1 (All Australia excluding Greater Sydney)
# per the executive summary's main specification.
#
# Tables use the booktabs + threeparttable packages already loaded in
# sydney_lockout_laws_latex.tex. They are designed as \input snippets --
# no \begin{document} wrapper.
#
# Run inside the project root (where the outputs/ tree lives). To wire into
# targets, add a tar_target(exec_summary_tables, make_exec_summary_tables(...),
# format = "file") to _targets.R; otherwise source() and call directly.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
})

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# Format a signed percent with one decimal place and an explicit sign, so that
# positive and negative effects align visually in columns.
fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "---",
         sprintf("%+.*f", digits, x))
}

# Format large integers (e.g. dollar levels, populations) with thousands
# separators and no decimal places.
fmt_int <- function(x) {
  ifelse(is.na(x), "---",
         formatC(round(x), format = "d", big.mark = ","))
}

# Format a dollar amount with $ prefix.
fmt_dollar <- function(x, digits = 0) {
  ifelse(is.na(x), "---",
         paste0("\\$", formatC(round(x, digits), format = "f",
                               digits = digits, big.mark = ",")))
}

# Format a signed dollar amount (for deltas).
fmt_dollar_signed <- function(x, digits = 0) {
  ifelse(is.na(x), "---",
         sprintf("%s\\$%s",
                 ifelse(x < 0, "-", "+"),
                 formatC(abs(round(x, digits)), format = "f",
                         digits = digits, big.mark = ",")))
}

# Wrap a numeric column so negatives render red and positives green/black in
# draft PDFs. Kept monochrome for the NBER-style output; easy to swap in later.
fmt_pct_bf <- function(x, digits = 1) {
  s <- fmt_pct(x, digits)
  # Bold the row total -- call externally.
  s
}

# ------------------------------------------------------------------------------
# Main entry point
# ------------------------------------------------------------------------------

make_exec_summary_tables <- function(
    att_aggregate_path   = "outputs/att_summary/att_aggregate_summary.csv",
    att_detail_path      = "outputs/att_summary/att_detail_by_sa2.csv",
    welfare_3loc_path    = "outputs/welfare/lockout_welfare_results_3loc.csv",
    welfare_inputs_path  = "outputs/welfare/lockout_welfare_inputs_3loc.csv",
    out_dir              = "outputs/paper/tables",
    pool                 = "P1",
    # Dependency arguments -- unused inside the function; their purpose is to
    # force {targets} to rebuild this target whenever the named upstream targets
    # change. Pass att_weighted_summary_files and welfare_files from _targets.R.
    .att_dep             = NULL,
    .welfare_dep         = NULL
) {
  dir_create(out_dir)
  
  agg   <- read_csv(att_aggregate_path,  show_col_types = FALSE) |>
    filter(pool_id == pool)
  det   <- read_csv(att_detail_path,     show_col_types = FALSE) |>
    filter(pool_id == pool)
  welf  <- read_csv(welfare_3loc_path,   show_col_types = FALSE) |>
    filter(pool_id == pool)
  winp  <- read_csv(welfare_inputs_path, show_col_types = FALSE) |>
    filter(pool_id == pool)
  
  out_files <- c(
    write_headline_att_table(agg, file.path(out_dir, "tab_headline_att.tex")),
    write_treated_sa2_table (det, file.path(out_dir, "tab_treated_sa2.tex")),
    write_spillover_sa2_table(det, file.path(out_dir, "tab_spillover_sa2.tex")),
    write_welfare_scenarios_table(welf, winp,
                                  file.path(out_dir, "tab_welfare_scenarios.tex"))
  )
  
  # Return the character vector of file paths written. {targets} uses this as
  # the target value when format = "file"; do NOT wrap in invisible(), which
  # would hide the paths from the targets framework.
  out_files
}

# ------------------------------------------------------------------------------
# Table 1: Headline ATT by outcome and treatment status
# ------------------------------------------------------------------------------
# Rows: Total employment, Acc. & food, Arts & rec, Retail, Median income,
#       Population, Rent.
# Columns: Treated (5 SA2s), Spillover (10 SA2s), each with % change and level.
# ------------------------------------------------------------------------------

write_headline_att_table <- function(agg, outfile) {
  
  # Row ordering: labour-market outcomes first (all employment-weighted),
  # then resident-market outcomes (population-weighted).
  row_spec <- tribble(
    ~outcome_type,    ~industry,                           ~row_label,                          ~panel,
    "Employment",     "Total",                             "Total employment (000s)",           "Labour",
    "Employment",     "Accommodation and food services",   "\\quad Accommodation \\& food",     "Labour",
    "Employment",     "Arts and recreation services",      "\\quad Arts \\& recreation",        "Labour",
    "Employment",     "Retail trade",                      "\\quad Retail trade",               "Labour",
    "Median income",  "Total",                             "Median annual income (\\$)",        "Labour",
    "Population",     "Population",                        "Resident population",               "Resident",
    "Rent",           "Rent (monthly avg)",                "Average monthly rent (\\$)",        "Resident"
  )
  
  # Pull treated and spillover side-by-side.
  wide <- agg |>
    select(unit_role, outcome_type, industry, agg_base_value,
           agg_att_raw, weighted_att_pct) |>
    mutate(weighted_att_pct = weighted_att_pct * 100) |>
    pivot_wider(
      id_cols     = c(outcome_type, industry),
      names_from  = unit_role,
      values_from = c(agg_base_value, agg_att_raw, weighted_att_pct),
      names_glue  = "{unit_role}_{.value}"
    )
  
  tab <- row_spec |>
    left_join(wide, by = c("outcome_type", "industry"))
  
  # Format a cell: "$+8.9$ ($+1{,}234$)" -- pct then raw effect in parens.
  # For rent and income the base-value is a level, which we show once on the
  # right to anchor the magnitude.
  fmt_row <- function(pct, raw, base, is_pct_row = TRUE) {
    pct_s <- fmt_pct(pct)
    if (is_pct_row) {
      sprintf("%s\\%%", pct_s)
    } else {
      pct_s
    }
  }
  
  # Build the LaTeX body one row at a time.
  lines <- character()
  current_panel <- ""
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    if (r$panel != current_panel) {
      if (current_panel != "") lines <- c(lines, "\\addlinespace")
      lines <- c(lines,
                 sprintf("\\multicolumn{4}{l}{\\textit{%s market}} \\\\",
                         r$panel))
      current_panel <- r$panel
    }
    
    # Treated cell
    t_pct <- fmt_pct(r$treated_weighted_att_pct)
    s_pct <- fmt_pct(r$spillover_weighted_att_pct)
    
    # Pre-treatment level, taken from the treated aggregate base value, scaled
    # to legible units.
    base_treated <- r$treated_agg_base_value
    base_spill   <- r$spillover_agg_base_value
    
    level_s <- if (r$outcome_type == "Employment") {
      # base_value is in thousands of jobs already.
      sprintf("%s / %s",
              formatC(base_treated, format = "f", digits = 1, big.mark = ","),
              formatC(base_spill,   format = "f", digits = 1, big.mark = ","))
    } else if (r$outcome_type == "Population") {
      sprintf("%s / %s",
              fmt_int(base_treated), fmt_int(base_spill))
    } else {
      # income or rent dollars
      sprintf("\\$%s / \\$%s",
              fmt_int(base_treated), fmt_int(base_spill))
    }
    
    lines <- c(lines,
               sprintf("%s & %s\\%% & %s\\%% & %s \\\\",
                       r$row_label, t_pct, s_pct, level_s))
  }
  
  # Assemble LaTeX.
  tex <- c(
    "% ----- Table 1: Headline ATTs by outcome, treated vs spillover -----------",
    "% Auto-generated by make_exec_summary_tables.R -- do not edit by hand.",
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\begin{threeparttable}",
    "\\caption{Estimated Effect of the Sydney Lockout Laws on Key Outcomes}",
    "\\label{tab:headline-att}",
    "\\begin{tabular}{l c c l}",
    "\\toprule",
    " & \\multicolumn{2}{c}{ATT, 2014--2018} & Pre-treatment \\\\",
    "\\cmidrule(lr){2-3}",
    "Outcome & Treated & Spillover & level (T / S) \\\\",
    "\\midrule",
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item \\textit{Notes:} Synthetic control estimates for 2014--2018, averaged",
    "across post-treatment years and then aggregated across units. Employment",
    "and median income outcomes are weighted by base-period industry employment;",
    "population and rent outcomes are weighted by base-period resident",
    "population. The ``Treated'' column covers the five SA2 regions directly",
    "subject to the lockout laws (Sydney--Haymarket--The Rocks, Surry Hills,",
    "Darlinghurst, Potts Point--Woolloomooloo, and Pyrmont--Ultimo). The",
    "``Spillover'' column covers the ten remaining inner-ring SA2s. Pre-treatment",
    "employment levels are in thousands of jobs. Arts \\& recreation is estimated",
    "only for the three treated SA2s with non-negligible baseline employment in",
    "the sector. Donor pool: all Australian SA2s excluding Greater Sydney.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  writeLines(tex, outfile)
  message("Wrote ", outfile)
  outfile
}

# ------------------------------------------------------------------------------
# Table 2: Per-SA2 ATTs for the five directly-treated precincts
# ------------------------------------------------------------------------------

write_treated_sa2_table <- function(det, outfile) {
  
  treated_order <- c(
    "Sydney - Haymarket - The Rocks",
    "Surry Hills",
    "Darlinghurst",
    "Potts Point - Woolloomooloo",
    "Pyrmont - Ultimo"
  )
  
  # Pull one row per (SA2, outcome of interest).
  pull_pct <- function(name, outcome_type, industry) {
    det |>
      filter(treated_name == name,
             outcome_type == !!outcome_type,
             industry     == !!industry) |>
      pull(att_pct) |>
      (\(x) if (length(x) == 0) NA_real_ else x[1] * 100)()
  }
  
  rows <- map_dfr(treated_order, function(nm) {
    tibble(
      sa2           = nm,
      emp_total     = pull_pct(nm, "Employment",    "Total"),
      emp_accfood   = pull_pct(nm, "Employment",    "Accommodation and food services"),
      income_total  = pull_pct(nm, "Median income", "Total"),
      pop           = pull_pct(nm, "Population",    "Population"),
      rent          = pull_pct(nm, "Rent",          "Rent (monthly avg)")
    )
  })
  
  body <- rows |>
    mutate(
      across(c(emp_total, emp_accfood, income_total, pop, rent),
             ~ paste0(fmt_pct(.x), "\\%"))
    ) |>
    mutate(line = sprintf("%s & %s & %s & %s & %s & %s \\\\",
                          sa2, emp_total, emp_accfood,
                          income_total, pop, rent)) |>
    pull(line)
  
  tex <- c(
    "% ----- Table 2: ATTs by directly-treated SA2 -----------------------------",
    "% Auto-generated by make_exec_summary_tables.R -- do not edit by hand.",
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\begin{threeparttable}",
    "\\caption{Heterogeneous Effects Across the Five Directly-Treated Precincts}",
    "\\label{tab:treated-sa2}",
    "\\begin{tabular}{l c c c c c}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Employment} & Median & & Monthly \\\\",
    "\\cmidrule(lr){2-3}",
    "SA2 & Total & Acc. \\& food & income & Population & rent \\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item \\textit{Notes:} Synthetic control ATTs expressed as percentage",
    "changes relative to each SA2's synthetic counterfactual, 2014--2018.",
    "Each cell is a separate SC estimate, run independently by SA2 $\\times$",
    "outcome. Donor pool: all Australian SA2s excluding Greater Sydney.",
    "Arts \\& recreation estimates are omitted here for the two precincts with",
    "negligible pre-treatment employment in that industry.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  writeLines(tex, outfile)
  message("Wrote ", outfile)
  outfile
}

# ------------------------------------------------------------------------------
# Table 3: Per-SA2 ATTs for the key spillover precincts
# ------------------------------------------------------------------------------
# Highlights the narrative of (a) the three outward-migration hotspots and
# (b) the two relocation destinations.
# ------------------------------------------------------------------------------

write_spillover_sa2_table <- function(det, outfile) {
  
  # Ordered to tell the story: displacement-losers first, then gainers.
  spillover_order <- c(
    "Redfern - Chippendale",
    "Waterloo - Beaconsfield",
    "Newtown - Camperdown - Darlington",
    "Bondi Junction - Waverly",
    "Paddington - Moore Park",
    "Glebe - Forest Lodge",
    "Erskineville - Alexandria",
    "Balmain",
    "Leichhardt - Annandale",
    "Lilyfield - Rozelle"
  )
  
  pull_pct <- function(name, outcome_type, industry) {
    det |>
      filter(treated_name == name,
             outcome_type == !!outcome_type,
             industry     == !!industry) |>
      pull(att_pct) |>
      (\(x) if (length(x) == 0) NA_real_ else x[1] * 100)()
  }
  
  rows <- map_dfr(spillover_order, function(nm) {
    tibble(
      sa2           = nm,
      emp_total     = pull_pct(nm, "Employment",    "Total"),
      emp_accfood   = pull_pct(nm, "Employment",    "Accommodation and food services"),
      emp_retail    = pull_pct(nm, "Employment",    "Retail trade"),
      pop           = pull_pct(nm, "Population",    "Population"),
      rent          = pull_pct(nm, "Rent",          "Rent (monthly avg)")
    )
  })
  
  body <- rows |>
    mutate(
      across(c(emp_total, emp_accfood, emp_retail, pop, rent),
             ~ paste0(fmt_pct(.x), "\\%"))
    ) |>
    mutate(line = sprintf("%s & %s & %s & %s & %s & %s \\\\",
                          sa2, emp_total, emp_accfood, emp_retail,
                          pop, rent)) |>
    pull(line)
  
  tex <- c(
    "% ----- Table 3: ATTs by spillover SA2 ------------------------------------",
    "% Auto-generated by make_exec_summary_tables.R -- do not edit by hand.",
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\begin{threeparttable}",
    "\\caption{Spillover Effects Across Inner-Ring Sydney SA2s}",
    "\\label{tab:spillover-sa2}",
    "\\begin{tabular}{l c c c c c}",
    "\\toprule",
    " & \\multicolumn{3}{c}{Employment} & & Monthly \\\\",
    "\\cmidrule(lr){2-4}",
    "SA2 & Total & Acc. \\& food & Retail & Population & rent \\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item \\textit{Notes:} Synthetic control ATTs for the ten inner-ring",
    "Sydney SA2s not directly subject to the lockout laws, expressed as",
    "percentage changes relative to each SA2's synthetic counterfactual,",
    "2014--2018. Each cell is a separate SC estimate. Donor pool: all",
    "Australian SA2s excluding Greater Sydney.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  writeLines(tex, outfile)
  message("Wrote ", outfile)
  outfile
}

# ------------------------------------------------------------------------------
# Table 4: Welfare decomposition across sigma and displacement scenarios
# ------------------------------------------------------------------------------
# Uses the 3-location aggregation (CBD / Inner_Ring / Rest_of_Sydney) so the
# table stays compact. Shows Delta EU per resident, total resident welfare
# change, total worker welfare change, and total welfare change, across the
# six (sigma, displacement) cells that bracket the headline $113--$264 range.
# ------------------------------------------------------------------------------

write_welfare_scenarios_table <- function(welf, winp, outfile) {
  
  # Pull city-level totals only.
  tot <- welf |>
    filter(location == "Sydney_Total") |>
    select(sigma, displacement_pct,
           EU_pre,
           delta_EU,
           delta_resident_welfare_total,
           delta_worker_welfare_total,
           delta_total_welfare) |>
    mutate(delta_EU_pct = delta_EU / EU_pre * 100)
  
  # Organize: sigma as column groups, displacement as rows.
  # Order: displacement 0 (no displacement; most aggressive), 50, 100 (full
  # displacement; most conservative).
  tot <- tot |>
    mutate(
      sigma_label = factor(
        sigma,
        levels = c(5000, 10000, 15000),
        labels = c("$\\sigma=5{,}000$", "$\\sigma=10{,}000$", "$\\sigma=15{,}000$")
      ),
      disp_label = factor(
        displacement_pct,
        levels = c(0, 50, 100),
        labels = c("0\\% (all jobs lost)",
                   "50\\% (half displaced)",
                   "100\\% (all displaced)")
      )
    ) |>
    arrange(displacement_pct, sigma)
  
  # Format: four values per (sigma, disp) cell, stacked vertically is too
  # much. Instead, make each row a sigma-level scenario, columns are the
  # four welfare components, and we stack three panels by displacement.
  
  rows <- character()
  disp_levels <- levels(tot$disp_label)
  for (d in disp_levels) {
    rows <- c(rows,
              sprintf("\\multicolumn{6}{l}{\\textit{Job displacement to Rest of Sydney: %s}} \\\\",
                      d))
    sub <- tot |> filter(disp_label == d) |> arrange(sigma)
    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      rows <- c(rows,
                sprintf("\\quad %s & %s & %s\\%% & %s & %s & %s \\\\",
                        as.character(r$sigma_label),
                        fmt_dollar_signed(r$delta_EU, 0),
                        fmt_pct(r$delta_EU_pct, 2),
                        fmt_dollar_signed(r$delta_resident_welfare_total / 1e9, 2),
                        fmt_dollar_signed(r$delta_worker_welfare_total   / 1e9, 2),
                        fmt_dollar_signed(r$delta_total_welfare           / 1e9, 2)))
    }
    if (d != tail(disp_levels, 1)) rows <- c(rows, "\\addlinespace")
  }
  
  tex <- c(
    "% ----- Table 4: Welfare decomposition across scenarios -------------------",
    "% Auto-generated by make_exec_summary_tables.R -- do not edit by hand.",
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    "\\begin{threeparttable}",
    "\\caption{Welfare Effects of the Sydney Lockout Laws, by Scenario}",
    "\\label{tab:welfare-scenarios}",
    "\\begin{tabular}{l c c c c c}",
    "\\toprule",
    " & \\multicolumn{2}{c}{$\\Delta EU$ per resident} & \\multicolumn{3}{c}{Aggregate welfare change (\\$B/yr)} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-6}",
    "Taste dispersion & \\$/yr & \\% of $EU_{\\text{pre}}$ & Residents & Workers & Total \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item \\textit{Notes:} Spatial equilibrium welfare estimates under the",
    "three-location aggregation (CBD, Inner Ring, Rest of Sydney). $\\sigma$ is",
    "the logit taste-dispersion parameter; higher values imply stronger",
    "idiosyncratic preferences over location and therefore a larger welfare",
    "weight on lost amenities. Displacement denotes the share of jobs lost in",
    "the treated and spillover regions that are assumed to relocate to the",
    "rest of Sydney rather than be destroyed. $\\Delta EU$ is the change in",
    "the logit inclusive value (expected resident welfare), expressed in",
    "dollars per resident per year. Donor pool: all Australian SA2s excluding",
    "Greater Sydney.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  writeLines(tex, outfile)
  message("Wrote ", outfile)
  outfile
}

# ------------------------------------------------------------------------------
# Script-mode entry point
# ------------------------------------------------------------------------------
# If sourced interactively, do nothing. If Rscript'd, run with defaults.

if (!interactive() && sys.nframe() == 0L) {
  make_exec_summary_tables()
}