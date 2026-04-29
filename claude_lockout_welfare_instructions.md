# Instructions for Claude: Equilibrium-Level Welfare Analysis of Sydney's Lockout Laws

These instructions implement the **equilibrium-level, share-based welfare approach** for the Sydney lockout laws project.

Use this method exactly as specified below. Do not substitute the earlier shortcut method that infers amenity from `delta_R - delta_y` alone.

The objective is to:

1. Compile **pre-treatment baseline values** for each location.
2. Construct **post-treatment values** by applying the synthetic control (SC) treatment effects to the pre-treatment baselines.
3. Calibrate **location-specific amenities** in the pre-treatment and post-treatment equilibria so that observed population shares are rationalized under a logit residential choice model.
4. Compute **deterministic resident welfare by location** in each equilibrium.
5. Compute **citywide expected resident welfare** in each equilibrium using the log-sum formula implied by the logit model.
6. Compute **worker welfare** separately using employment and income.
7. Output clean files containing the requested baseline, treatment-effect, post-treatment, and welfare objects.

## 1. Core modeling framework

Assume residents choose among locations within Sydney according to a logit discrete-choice model.

For location `k` in period `t`, deterministic utility is:

```text
V[k,t] = y[k,t] - r[k,t] + a[k,t]
```

where:

- `y[k,t]` = annual income per person
- `r[k,t]` = annual rent per person
- `a[k,t]` = amenity term

Population shares satisfy:

```text
s[k,t] = exp(V[k,t] / sigma) / sum_j exp(V[j,t] / sigma)
```

where `sigma` is the taste-dispersion scale parameter.

### Scale parameter assumption

Use:

```text
sigma = 10000
```

measured in annual dollars per person.

This is a reasonable calibration for a within-city residential location-choice problem with moderate mobility frictions and substantial idiosyncratic tastes. Treat it as the main specification. Also make it easy to change later for robustness.

## 2. Geography

Run the analysis in two versions.

### Version A: 3 locations

1. `CBD`
2. `Inner_Ring`
3. `Rest_of_Sydney`

### Version B: 16 locations

- 5 CBD SA2s
- 10 Inner-Ring SA2s
- 1 catch-all residual unit for the rest of Sydney

Index locations by `k`.

## 3. Required inputs

For each location `k`, compile the following **pre-treatment baseline values**:

- `employment_pre[k]` = total employment in the location
- `population_pre[k]` = total resident population in the location
- `income_pre[k]` = annual income per person
- `rent_pre[k]` = annual rent per person

Also compile the **SC treatment effects** for each location:

- `employment_te[k]` = treatment effect on total employment (level)
- `income_te[k]` = treatment effect on annual income per person (level)
- `rent_te[k]` = treatment effect on annual rent per person (level)
- `population_te[k]` = treatment effect on population (level) for CBD and Inner-Ring locations
- `population_share_te[k]` = treatment effect on the population share, if directly available

Use level population treatment effects for CBD and Inner-Ring locations whenever available. For the residual `Rest_of_Sydney` location, do not directly apply an SC population treatment effect. Instead, set post-treatment population as the balancing residual required to keep total Sydney population fixed at its pre-treatment level.

### Important unit requirement

All income and rent variables must be in **annual dollars per person**.

- If rent is weekly, convert to annual: `annual_rent = weekly_rent * 52`
- If rent is per dwelling or household rather than per person, convert to a per-person measure using an explicit occupancy assumption or observed average household size. Record the assumption.
- If income is not annual, convert it to annual.

### Important formula for post-treatment values

Construct post-treatment values by applying treatment effects to pre-treatment baselines:

```text
post_value = pre_value + treatment_effect
```

Examples:

- If pre-treatment employment is `10000` and the SC estimate is `-10`, post-treatment employment is `9990`.
- If pre-treatment employment is `1000` and the SC estimate is `-10`, post-treatment employment is `990`.

Use the formula exactly. Do not hard-code any example values.

## 4. Build pre-treatment and post-treatment datasets

For each location `k`, construct the following variables.

### Pre-treatment variables

```text
employment_pre[k]
population_pre[k]
income_pre[k]
rent_pre[k]
pop_share_pre[k] = population_pre[k] / sum_j population_pre[j]
```

### Post-treatment variables

```text
employment_post[k] = employment_pre[k] + employment_te[k]
income_post[k] = income_pre[k] + income_te[k]
rent_post[k] = rent_pre[k] + rent_te[k]
```

For population and population shares, use the following hierarchy.

#### Case 1: CBD and Inner-Ring locations with level population treatment effects

For all CBD and Inner-Ring locations, compute:

```text
population_post[k] = population_pre[k] + population_te[k]
```

#### Case 2: Rest of Sydney residual location

Do not apply an independent SC population treatment effect to `Rest_of_Sydney`. Instead, set its post-treatment population so that total Sydney population remains fixed at the pre-treatment total:

```text
total_population_pre = sum_j population_pre[j]
population_post[Rest_of_Sydney] = total_population_pre - sum_{k != Rest_of_Sydney} population_post[k]
```

Then compute post-treatment shares for all locations:

```text
pop_share_post[k] = population_post[k] / total_population_pre
```

Because total Sydney population is held fixed, the denominator is the pre-treatment total population.

#### Case 3: If only population-share treatment effects are available for a robustness exercise

Only use this case if level population treatment effects are unavailable. Then compute:

```text
pop_share_post[k] = pop_share_pre[k] + population_share_te[k]
```

Re-normalize the shares so that they sum to 1 exactly:

```text
pop_share_post[k] = pop_share_post[k] / sum_j pop_share_post[j]
```

Then recover post-treatment population counts by holding total Sydney population fixed:

```text
population_post[k] = pop_share_post[k] * total_population_pre
```

### Validation checks

Before proceeding, enforce these checks:

1. `employment_post[k] >= 0`
2. `population_post[k] > 0` for all locations
3. `income_post[k]` is non-missing
4. `rent_post[k]` is non-missing
5. `pop_share_pre[k] > 0`
6. `pop_share_post[k] > 0`
7. `sum_k population_post[k] = sum_k population_pre[k]` within numerical tolerance
8. `sum_k pop_share_pre[k] = 1` within numerical tolerance
9. `sum_k pop_share_post[k] = 1` within numerical tolerance

If any share is zero or negative, stop and flag the issue.

## 5. Normalization for amenities

Amenities are only identified up to a normalization.

Use the following normalization in both periods:

- Set the amenity in the reference location equal to zero.

### Reference location

For the 3-location model:

```text
reference location = Rest_of_Sydney
```

For the 16-location model:

```text
reference location = the catch-all Rest_of_Sydney residual unit
```

So impose:

```text
a[reference, pre] = 0
a[reference, post] = 0
```

## 6. Recover amenities in the pre-treatment equilibrium

For each non-reference location `k`, recover the pre-treatment amenity as:

```text
a_pre[k] = sigma * (log(pop_share_pre[k]) - log(pop_share_pre[ref]))
           - ((income_pre[k] - rent_pre[k]) - (income_pre[ref] - rent_pre[ref]))
```

For the reference location:

```text
a_pre[ref] = 0
```

## 7. Recover amenities in the post-treatment equilibrium

For each non-reference location `k`, recover the post-treatment amenity as:

```text
a_post[k] = sigma * (log(pop_share_post[k]) - log(pop_share_post[ref]))
            - ((income_post[k] - rent_post[k]) - (income_post[ref] - rent_post[ref]))
```

For the reference location:

```text
a_post[ref] = 0
```

## 8. Compute deterministic resident welfare by location

For each location `k`, compute deterministic utility in each period.

### Pre-treatment deterministic welfare

```text
V_pre[k] = income_pre[k] - rent_pre[k] + a_pre[k]
```

### Post-treatment deterministic welfare

```text
V_post[k] = income_post[k] - rent_post[k] + a_post[k]
```

### Change in deterministic welfare

```text
delta_V[k] = V_post[k] - V_pre[k]
```

These are the location-specific deterministic welfare values for residents living in each location.

## 9. Compute citywide expected resident welfare

Under the logit model, citywide expected resident welfare is the log-sum object.

### Pre-treatment expected resident welfare

```text
EU_pre = sigma * log(sum_k exp(V_pre[k] / sigma))
```

### Post-treatment expected resident welfare

```text
EU_post = sigma * log(sum_k exp(V_post[k] / sigma))
```

### Change in expected resident welfare

```text
delta_EU = EU_post - EU_pre
```

This is the correct citywide expected resident welfare change implied by the residential choice model.

## 10. Compute worker welfare separately

Compute worker welfare using employment and annual income per person.

### Pre-treatment worker welfare by location

```text
worker_welfare_pre[k] = employment_pre[k] * income_pre[k]
```

### Post-treatment worker welfare by location

```text
worker_welfare_post[k] = employment_post[k] * income_post[k]
```

### Change in worker welfare by location

```text
delta_worker_welfare[k] = worker_welfare_post[k] - worker_welfare_pre[k]
```

### Citywide worker welfare

```text
worker_welfare_pre_total = sum_k worker_welfare_pre[k]
worker_welfare_post_total = sum_k worker_welfare_post[k]
delta_worker_welfare_total = worker_welfare_post_total - worker_welfare_pre_total
```

## 11. Compute total welfare change

Define citywide total welfare change as:

```text
delta_total_welfare = delta_worker_welfare_total + delta_EU * total_population_weight
```

### Population weight for resident welfare

Use:

```text
total_population_weight = sum_k population_pre[k]
```

So the resident welfare component in dollars is:

```text
resident_welfare_pre_total = EU_pre * sum_k population_pre[k]
resident_welfare_post_total = EU_post * sum_k population_pre[k]
delta_resident_welfare_total = resident_welfare_post_total - resident_welfare_pre_total
```

Then:

```text
total_welfare_pre = worker_welfare_pre_total + resident_welfare_pre_total
total_welfare_post = worker_welfare_post_total + resident_welfare_post_total
delta_total_welfare = total_welfare_post - total_welfare_pre
```

Use **baseline total population** as the weight in both periods unless instructed otherwise.

## 12. Treatment effects in percentage terms

For each location and each variable, compute percentage treatment effects.

For any variable `x` with pre-treatment baseline `x_pre[k]` and treatment effect `x_te[k]`:

```text
x_te_pct[k] = 100 * x_te[k] / x_pre[k]
```

Do this for at least:

- employment
- income
- rent
- population, if available in levels
- population share, if using shares

If a baseline is zero or near zero, return missing and flag it rather than dividing by zero.

## 13. Output files

Produce exactly two main output files for each geography version.

### File 1: Baselines, treatment effects, and post-treatment values

Filename suggestion for 3-location version:

```text
lockout_welfare_inputs_3loc.csv
```

Filename suggestion for 16-location version:

```text
lockout_welfare_inputs_16loc.csv
```

Include one row per location.

Required columns:

```text
location
employment_pre
employment_te
employment_te_pct
employment_post

population_pre
population_te
population_te_pct
population_post

pop_share_pre
pop_share_te
pop_share_te_pct
pop_share_post

income_pre
income_te
income_te_pct
income_post

rent_pre
rent_te
rent_te_pct
rent_post
```

For the residual `Rest_of_Sydney` row, keep `population_te` and `population_te_pct` missing if post-treatment population is computed as the balancing residual rather than from an SC estimate. If some treatment-effect objects are unavailable because population is observed directly rather than estimated as a treatment effect, keep the column and fill with missing values as appropriate.

### File 2: Welfare objects

Filename suggestion for 3-location version:

```text
lockout_welfare_results_3loc.csv
```

Filename suggestion for 16-location version:

```text
lockout_welfare_results_16loc.csv
```

Include one row per location plus one citywide total row.

Required columns for each location row:

```text
location
sigma
amenity_pre
amenity_post
delta_amenity

V_pre
V_post
delta_V

worker_welfare_pre
worker_welfare_post
delta_worker_welfare
```

For the citywide total row, include:

```text
location = Sydney_Total
sigma
EU_pre
EU_post
delta_EU

resident_welfare_pre_total
resident_welfare_post_total
delta_resident_welfare_total

worker_welfare_pre_total
worker_welfare_post_total
delta_worker_welfare_total

total_welfare_pre
total_welfare_post
delta_total_welfare
```

It is fine for location-level rows to leave citywide-only columns empty.

## 14. Recommended implementation sequence

Follow this order exactly.

1. Load baseline data by location.
2. Load SC treatment effects by location.
3. Convert all income and rent variables to annual dollars per person.
4. Construct post-treatment values by adding treatment effects to baselines.
5. Construct post-treatment populations for CBD and Inner-Ring using level SC effects, then set `Rest_of_Sydney` as the balancing residual that keeps total Sydney population fixed.
6. Construct pre-treatment and post-treatment population shares.
7. Choose `sigma = 10000`.
8. Choose the reference location.
9. Recover `a_pre[k]`.
10. Recover `a_post[k]`.
11. Compute `V_pre[k]`, `V_post[k]`, and `delta_V[k]`.
12. Compute `EU_pre`, `EU_post`, and `delta_EU`.
13. Compute worker welfare by location and citywide.
14. Compute resident welfare totals using baseline total population.
15. Compute total welfare pre, post, and change.
16. Compute treatment effects in percentage terms.
17. Export the two output files.

## 15. Numerical stability

Use numerically stable code for the log-sum calculation.

Do not compute:

```text
log(sum(exp(V/sigma)))
```

naively if utilities are large.

Instead use a stable log-sum-exp routine.

## 16. Minimal validation output

At the end, print a compact validation summary showing:

1. Sum of pre-treatment population shares
2. Sum of post-treatment population shares
3. Reference location used
4. Value of `sigma`
5. Citywide `EU_pre`, `EU_post`, `delta_EU`
6. Citywide `delta_worker_welfare_total`
7. Citywide `delta_total_welfare`

Do not add narrative discussion. Only display the numerical validation summary.

## 17. Important methodological constraints

Apply the method exactly as specified here.

- Do not revert to the shortcut `delta_a = delta_r - delta_y` as the primary identification method.
- Do not add rent changes directly into total welfare.
- Do not replace the log-sum resident welfare object with a simple share-weighted average utility unless explicitly asked.
- Do not change the normalization unless explicitly asked.
- Do not change `sigma` unless explicitly asked; treat `10000` as the main calibration.

## 18. Deliverables

For each geography version, return:

1. The compiled input/output file with baselines, treatment effects, percentage effects, and post-treatment values.
2. The welfare-results file with amenities, deterministic welfare by location, and citywide expected welfare.
3. A minimal validation printout with the requested numerical checks only.

