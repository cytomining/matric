test_that("`sim_annotate` works", {
  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
    Metadata_type = sample(c("x", "y"), 4, replace = TRUE),
    x = rnorm(4),
    y = x + rnorm(4) / 100,
    z = y + rnorm(4) / 1000
  )
  annotation_cols <- c("Metadata_group")
  # this is not a great test because it tests more than one function
  sim_df <- matric::sim_calculate(population, method = "pearson")
  sim_df <- matric::sim_annotate(sim_df, annotation_cols)

  expect_equal(
    sort(unique(sim_df$Metadata_group1)),
    sort(unique(sim_df$Metadata_group2))
  )

  expect_equal(
    sort(unique(
      matric::sim_annotate(sim_df,
        annotation_cols,
        index = "left"
      )$Metadata_group
    )),
    sort(unique(
      matric::sim_annotate(sim_df,
        annotation_cols,
        index = "right"
      )$Metadata_group
    ))
  )
})
