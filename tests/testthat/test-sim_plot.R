test_that("`sim_plot` works", {
  n <- 100

  set.seed(42)
  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b", "c", "d"), n, replace = TRUE),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n)
  )

  annotation_cols <- c("Metadata_group", "Metadata_type")
  sim_df <- matric::sim_calculate(population, method = "pearson")
  row_metadata <- attr(sim_df, "row_metadata")
  sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
  annotation_column <- "Metadata_group"

  # We'd do it more thoroughly if we really wanted to
  # https://stackoverflow.com/a/31041168
  #
  p <- matric::sim_plot(sim_df, annotation_column, calculate_sim_rank = TRUE)

  expect_equal(p$labels$x, "Metadata_group1")
  expect_equal(p$labels$y, "Metadata_group2")
})
