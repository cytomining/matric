## code to prepare `cellhealth` dataset goes here
library(dplyr)
library(readr)
url <-
  "https://github.com/broadinstitute/grit-benchmark/raw/main/1.calculate-metrics/cell-health/data/cell_health_merged_feature_select.csv.gz"

cellhealth <-
  read_csv(
    url,
    col_types = cols_only(
      Cells_AreaShape_Compactness = col_double(),
      Cells_AreaShape_Extent = col_double(),
      Cells_AreaShape_Zernike_0_0 = col_double(),
      Metadata_Plate = col_character(),
      Metadata_Well = col_character(),
      Metadata_cell_line = col_character(),
      Metadata_gene_name = col_character(),
      Metadata_pert_name = col_character()
    )
  )

strata_cols <-
  cellhealth %>%
  select(matches("Metadata_")) %>%
  select(-Metadata_Well) %>%
  colnames()

genes <-
  cellhealth %>%
  distinct(Metadata_gene_name) %>%
  slice(1:3) %>%
  bind_rows(data.frame(Metadata_gene_name = "EMPTY")) %>%
  distinct()

cellhealth <-
  cellhealth %>%
  inner_join(genes) %>%
  group_by(across(all_of(strata_cols))) %>%
  slice(1:2) %>%
  ungroup()

usethis::use_data(cellhealth, overwrite = TRUE)
