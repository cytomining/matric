test_that("`sim_metrics` works", {
  sim_df <- matric::sim_calculate(matric::cellhealth)

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

  metrics <-
    matric::sim_metrics(collated_sim, "ref", calculate_grouped = TRUE)

  answer <-
    structure(
      list(
        sim_scaled_mean_ref_i = 0.0308888497189268,
        sim_scaled_median_ref_i = 0.0754900029366019,
        sim_mean_i = 0.135553031836495,
        sim_median_i = 0.171754795930839,
        sim_mean_stat_ref_i = 0.101138990315028,
        sim_sd_stat_ref_i = 0.79702010493852
      ),
      row.names = c(
        NA,
        -1L
      ),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equal(
    answer,
    metrics$level_1_0 %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("sim"), mean))
  )

  answer <-
    structure(
      list(
        sim_scaled_mean_ref_i_mean_i = 0.0308888497189268,
        sim_scaled_mean_ref_i_median_i = 0.0652792460842569,
        sim_scaled_median_ref_i_mean_i = 0.0754900029366019,
        sim_scaled_median_ref_i_median_i = 0.173017446963813,
        sim_mean_i_mean_i = 0.135553031836495,
        sim_mean_i_median_i = 0.0971383669904163,
        sim_median_i_mean_i = 0.171754795930839,
        sim_median_i_median_i = 0.192531255798829,
        sim_mean_stat_ref_i_mean_i = 0.101138990315028,
        sim_mean_stat_ref_i_median_i = 0.161240002427761,
        sim_sd_stat_ref_i_mean_i = 0.79702010493852,
        sim_sd_stat_ref_i_median_i = 0.80835316991924
      ),
      row.names = c(
        NA,
        -1L
      ),
      class = c("tbl_df", "tbl", "data.frame")
    )
  expect_equal(
    answer,
    metrics$level_1 %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("sim"), mean))
  )


  answer <-
    structure(
      list(
        sim_scaled_mean_ref_g = 0.00792946303872089,
        sim_scaled_median_ref_g = 0.225443580806355,
        sim_mean_g = 0.107640219211251,
        sim_median_g = 0.287140533081703,
        sim_mean_stat_ref_g = 0.101138990315028,
        sim_sd_stat_ref_g = 0.835088742570834
      ),
      row.names = c(
        NA,
        -1L
      ),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equal(
    answer,
    metrics$level_2_1 %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("sim"), mean))
  )
})
