test_that("`sim_metrics_signif` works", {
  level_1_0_metrics <-
    matric::cellhealthmetrics$level_1_0 %>%
    dplyr::filter(Metadata_cell_line == "HCC44") %>%
    dplyr::slice_head(n = 50) %>%
    matric::sim_metrics_signif(
      background_type = "ref",
      level_identifier = "i",
      n_iterations = 100,
      metric_name = "average_precision"
    )

  # -log10(p) should increase with higher effect size

  expect_gte(
    level_1_0_metrics %>%
      dplyr::filter(
        sim_stat_signal_n_ref_i == 5 &
          sim_stat_background_n_ref_i == 48
      ) %>%
      dplyr::arrange(sim_retrieval_average_precision_ref_i) %>%
      dplyr::mutate(
        nlog10pvalue_diff =
          sim_retrieval_average_precision_ref_i_nlog10pvalue -
            lag(sim_retrieval_average_precision_ref_i_nlog10pvalue)
      ) %>%
      dplyr::select(nlog10pvalue_diff) %>%
      na.omit() %>%
      dplyr::filter(nlog10pvalue_diff < 0) %>%
      nrow(),
    0
  )
})
