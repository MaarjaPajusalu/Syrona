# syrona

Stratified prevalence comparison across OMOP CDM datasets.

Syrona extracts prevalence tables from OMOP CDM databases (conditions, procedures, drugs), computes log2 prevalence ratios between paired datasets, and synthesizes them via random-effects meta-analysis at multiple aggregation levels.

## Installation

```r
# install.packages("remotes")
remotes::install_github("your-org/syrona")
```

## Quick start

```r
library(syrona)

# 1. Connect to an OMOP CDM database
db <- syrona_connect("path/to/omop.duckdb")

# 2. Extract stratified prevalence tables
extract_all("Dataset_A", db = db)
syrona_disconnect(db)

# Repeat for a second database/cohort, then:

# 3. Compare
compare_all("Dataset_A", "Dataset_B")
```

## What it does

1. **Extract** (Phase 1) - query an OMOP CDM via CDMConnector + dplyr to produce prevalence by concept x year x sex x age group, concept metadata, chapter assignments, and SNOMED attributes. k-anonymity suppression applied automatically.

2. **Compare** (Phase 2) - pair two datasets, match strata, compute log2 prevalence ratios with confidence intervals.

3. **Meta-analyse** (Phase 3) - synthesize per-stratum estimates via random-effects meta-analysis (Paule-Mandel tau) across years, age groups, and sexes.

## Domains

- **Conditions** - SNOMED concepts, chapters via body system / disease category / ICD-10
- **Procedures** - SNOMED concepts, chapters by method / by site
- **Drugs** - rolled up to Ingredient level, ATC 1st level chapters

## OHDSI cohort support

Extract subpopulations using standard OHDSI cohort tables:

```r
# Create a care-site cohort
create_caresite_cohort(con, care_site_id = 101, cohort_id = 1,
                       cohort_schema = "results", cdm_schema = "cdm")

# Extract only that cohort
extract_all("Hospital_A", db = db, cohort_id = 1, cohort_schema = "results")
```

## Dependencies

- [CDMConnector](https://CRAN.R-project.org/package=CDMConnector) (>= 2.0.0)
- [omopgenerics](https://CRAN.R-project.org/package=omopgenerics) (>= 1.3.0)
- [meta](https://CRAN.R-project.org/package=meta) (for meta-analysis)
- [duckdb](https://CRAN.R-project.org/package=duckdb) (for local databases)
