test_that("`sim_metrics` works", {
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

  # ---- regular lazy sim_df ----

  sim_df <- matric::sim_calculate(matric::cellhealth)

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
        sim_scaled_mean_ref_i = 0.0141851594582033,
        sim_scaled_median_ref_i = 0.0587863126758784,
        sim_ranked_relrank_mean_ref_i = 0.551018518518519,
        sim_ranked_relrank_median_ref_i = 0.53712962962963,
        sim_mean_i = 0.135553031836495,
        sim_median_i = 0.171754795930839,
        sim_mean_stat_ref_i = 0.102638280742606,
        sim_sd_stat_ref_i = 0.797300368641178,
        sim_retrieval_average_precision_ref_i = 0.364021581450503,
        sim_retrieval_r_precision_ref_i = 0.206666666666667
      ),
      class = c(
        "tbl_df",
        "tbl", "data.frame"
      ),
      row.names = c(NA, -1L)
    )

  expect_equal(
    answer,
    metrics$level_1_0 %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("sim"), mean))
  )

  answer <-
    structure(
      list(
        sim_scaled_mean_ref_i_mean_i = 0.0141851594582033,
        sim_scaled_mean_ref_i_median_i = 0.0614189218851382,
        sim_scaled_median_ref_i_mean_i = 0.0587863126758784,
        sim_scaled_median_ref_i_median_i = 0.169157122764694,
        sim_ranked_relrank_mean_ref_i_mean_i = 0.551018518518519,
        sim_ranked_relrank_mean_ref_i_median_i = 0.554444444444444,
        sim_ranked_relrank_median_ref_i_mean_i = 0.53712962962963,
        sim_ranked_relrank_median_ref_i_median_i = 0.530555555555556,
        sim_mean_i_mean_i = 0.135553031836495,
        sim_mean_i_median_i = 0.0971383669904163,
        sim_median_i_mean_i = 0.171754795930839,
        sim_median_i_median_i = 0.192531255798829,
        sim_mean_stat_ref_i_mean_i = 0.102638280742606,
        sim_mean_stat_ref_i_median_i = 0.156868735917279,
        sim_sd_stat_ref_i_mean_i = 0.797300368641178,
        sim_sd_stat_ref_i_median_i = 0.811216514006836,
        sim_retrieval_average_precision_ref_i_mean_i = 0.364021581450503,
        sim_retrieval_average_precision_ref_i_median_i = 0.321487174045998,
        sim_retrieval_r_precision_ref_i_mean_i = 0.206666666666667,
        sim_retrieval_r_precision_ref_i_median_i = 0.173333333333333
      ),
      class = c(
        "tbl_df",
        "tbl", "data.frame"
      ),
      row.names = c(NA, -1L)
    )
  expect_equal(
    answer,
    metrics$level_1 %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("sim"), mean))
  )

  answer <-
    structure(
      list(
        sim_scaled_mean_ref_g = 0.00685852024686913,
        sim_scaled_median_ref_g = 0.222212778308099,
        sim_ranked_relrank_mean_ref_g = 0.527532407407407,
        sim_ranked_relrank_median_ref_g = 0.522361111111111,
        sim_mean_g = 0.107640219211251,
        sim_median_g = 0.287140533081703,
        sim_mean_stat_ref_g = 0.102638280742606,
        sim_sd_stat_ref_g = 0.837821072124129,
        sim_retrieval_average_precision_ref_g = 0.59015449527355,
        sim_retrieval_r_precision_ref_g = 0.588518518518519
      ),
      class = c(
        "tbl_df",
        "tbl", "data.frame"
      ),
      row.names = c(NA, -1L)
    )
  expect_equal(
    answer,
    metrics$level_2_1 %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("sim"), mean))
  )

  metrics_orig <- metrics

  # ---- lazy sim_df ----

  # without non_reps, group_reps, and all_same_cols_rep_ref = all_same_cols_rep


  sim_df <-
    matric::sim_calculate(matric::cellhealth, method = "cosine")

  collated_sim <-
    matric::sim_collate(
      sim_df,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = NULL,
      all_same_cols_non_rep = NULL,
      all_different_cols_non_rep = NULL,
      any_different_cols_group = NULL,
      all_same_cols_group = NULL,
      annotation_cols = annotation_cols,
      drop_group = drop_group
    )

  metrics <-
    matric::sim_metrics(collated_sim, "ref", calculate_grouped = TRUE)

  index <- matric::sim_calculate(matric::cellhealth,
    method = "cosine",
    lazy = TRUE
  )

  collated_sim_lazy <-
    matric::sim_collate(
      index,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = NULL,
      all_same_cols_non_rep = NULL,
      all_different_cols_non_rep = NULL,
      any_different_cols_group = NULL,
      all_same_cols_group = NULL,
      annotation_cols = annotation_cols,
      drop_group = drop_group
    )

  collated_sim_lazy <-
    sim_calculate_ij(matric::cellhealth, collated_sim_lazy)

  metrics_lazy <-
    matric::sim_metrics(collated_sim_lazy, "ref", calculate_grouped = TRUE)

  expect_equal(metrics, metrics_lazy)

  # ---- optimized lazy sim_df ----

  # without non_reps, group_reps, and all_same_cols_rep_ref = all_same_cols_rep

  index_optimized_lazy <- matric::sim_calculate(
    matric::cellhealth,
    method = "cosine",
    lazy = TRUE,
    all_same_cols_rep_or_group = all_same_cols_rep,
    all_same_cols_ref = all_same_cols_ref,
    all_same_cols_rep_ref = all_same_cols_rep,
    reference = reference
  )

  collated_sim_optimized_lazy <-
    matric::sim_collate(
      index_optimized_lazy,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = NULL,
      all_same_cols_non_rep = NULL,
      all_different_cols_non_rep = NULL,
      any_different_cols_group = NULL,
      all_same_cols_group = NULL,
      annotation_cols = annotation_cols,
      drop_group = drop_group
    )

  collated_sim_optimized_lazy <-
    sim_calculate_ij(matric::cellhealth, collated_sim_optimized_lazy)

  metrics_optimized_lazy <-
    matric::sim_metrics(collated_sim_optimized_lazy, "ref",
      calculate_grouped = TRUE
    )

  expect_equal(metrics, metrics_optimized_lazy)

  metrics_optimized_lazy_furrr <-
    matric::sim_metrics(collated_sim_optimized_lazy, "ref",
      calculate_grouped = TRUE, use_furrr = TRUE
    )

  expect_equal(metrics, metrics_optimized_lazy_furrr)

  # ---- annotation works ----

  sim_df_extra_annotation <- matric::sim_calculate(matric::cellhealth %>%
                                    dplyr::mutate(Metadata_dummy = "dummy"))

  extra_annotation_cols <- c("Metadata_dummy", "Metadata_Well", "Metadata_Plate")

  collated_sim_extra_annotation <-
    matric::sim_collate(
      sim_df_extra_annotation,
      reference,
      all_same_cols_rep = all_same_cols_rep,
      all_same_cols_rep_ref = all_same_cols_rep_ref,
      all_same_cols_ref = all_same_cols_ref,
      any_different_cols_non_rep = any_different_cols_non_rep,
      all_same_cols_non_rep = all_same_cols_non_rep,
      all_different_cols_non_rep = all_different_cols_non_rep,
      any_different_cols_group = any_different_cols_group,
      all_same_cols_group = all_same_cols_group,
      annotation_cols = c(annotation_cols, extra_annotation_cols),
      drop_group = drop_group
    )

  metrics_extra_annotation <-
    matric::sim_metrics(collated_sim_extra_annotation, "ref", calculate_grouped = TRUE)

  expect_equal(
    metrics_extra_annotation$level_1 %>%
      dplyr::select(-any_of(extra_annotation_cols)) %>%
      dplyr::distinct(),
    metrics_orig$level_1
  )

  expect_equal(
    metrics_extra_annotation$level_1_0 %>%
      dplyr::select(-all_of(extra_annotation_cols)),
    metrics_orig$level_1_0
  )

})

test_that("`r_precision` works", {
  df <- data.frame(truth = c("signal", "background", "background"))
  expect_equal(matric:::r_precision(df), 1)

  df <- data.frame(truth = c("background", "signal", "background"))
  expect_equal(matric:::r_precision(df), 0)

  df <- data.frame(truth = c("background", "signal", "signal", "background"))
  expect_equal(matric:::r_precision(df), 0.5)

  df <- data.frame(truth = c("background", "background"))
  expect_equal(matric:::r_precision(df), NaN)
})
