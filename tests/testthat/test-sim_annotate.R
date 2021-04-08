test_that("`sim_annotate` works", {
  n <- 4

  set.seed(42)
  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b"), n, replace = TRUE),
    Metadata_type = sample(c("x", "y"), n, replace = TRUE),
    x = rnorm(n),
    y = x + rnorm(n) / 100,
    z = y + rnorm(n) / 1000
  )
  annotation_cols <- c("Metadata_group")
  # this is not a great test because it tests more than one function
  sim_df <- matric::sim_calculate(population, method = "pearson")
  row_metadata <- attr(sim_df, "row_metadata")
  sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)

  expect_equal(
    sort(unique(sim_df$Metadata_group1)),
    sort(unique(sim_df$Metadata_group2))
  )

  expect_equal(
    sort(unique(
      matric::sim_annotate(
        sim_df,
        row_metadata,
        annotation_cols,
        index = "left"
      )$Metadata_group
    )),
    sort(unique(
      matric::sim_annotate(
        sim_df,
        row_metadata,
        annotation_cols,
        index = "right"
      )$Metadata_group
    ))
  )
})
