## code to prepare `cellhealth_metrics` dataset goes here
## code to prepare `cellhealth` dataset goes here
library(dplyr)
library(readr)

## -----------------------------------------------------------------------------

url <-
  "https://github.com/broadinstitute/grit-benchmark/raw/main/1.calculate-metrics/cell-health/data/cell_health_merged_feature_select.csv.gz"

cellhealth <-
  read_csv(
    url,
    col_types = cols(
      .default = col_double(),
      Metadata_Plate = col_character(),
      Metadata_Well = col_character(),
      Metadata_WellRow = col_character(),
      Metadata_WellCol = col_character(),
      Metadata_cell_line = col_character(),
      Metadata_gene_name = col_character(),
      Metadata_pert_name = col_character()
    )
  )

## -----------------------------------------------------------------------------

sim_df <- matric::sim_calculate(cellhealth)

## -----------------------------------------------------------------------------
drop_group <-
  data.frame(Metadata_gene_name = "EMPTY")

reference <-
  data.frame(Metadata_gene_name = c("Chr2"))

all_same_cols_ref <-
  c(
    "Metadata_cell_line",
    "Metadata_Plate"
  )

all_same_cols_rep <-
  c(
    "Metadata_cell_line",
    "Metadata_gene_name",
    "Metadata_pert_name"
  )

all_same_cols_rep_ref <-
  c(
    "Metadata_cell_line",
    "Metadata_gene_name",
    "Metadata_pert_name",
    "Metadata_Plate"
  )

any_different_cols_non_rep <-
  c(
    "Metadata_cell_line",
    "Metadata_gene_name",
    "Metadata_pert_name"
  )

all_same_cols_non_rep <-
  c(
    "Metadata_cell_line",
    "Metadata_Plate"
  )

all_different_cols_non_rep <-
  c("Metadata_gene_name")

all_same_cols_group <-
  c(
    "Metadata_cell_line",
    "Metadata_gene_name"
  )

any_different_cols_group <-
  c(
    "Metadata_cell_line",
    "Metadata_gene_name",
    "Metadata_pert_name"
  )

annotation_cols <-
  c(
    "Metadata_cell_line",
    "Metadata_gene_name",
    "Metadata_pert_name"
  )

## -----------------------------------------------------------------------------

collated_sim <-
  matric::sim_collate(
    sim_df,
    reference,
    all_same_cols_rep = all_same_cols_rep,
    all_same_cols_rep_ref = all_same_cols_rep_ref,
    all_same_cols_ref = all_same_cols_ref,
    any_different_cols_non_rep = any_different_cols_non_rep,
    all_same_cols_non_rep = all_same_cols_non_rep,
    all_different_cols_non_rep = all_different_cols_non_rep,
    any_different_cols_group = any_different_cols_group,
    all_same_cols_group = all_same_cols_group,
    annotation_cols = annotation_cols,
    drop_group = drop_group
  )

## -----------------------------------------------------------------------------

cellhealthmetrics <-
  matric::sim_metrics(collated_sim, "ref", calculate_grouped = TRUE)


usethis::use_data(cellhealthmetrics, overwrite = TRUE)
