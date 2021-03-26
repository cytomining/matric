test_that("`sim_filter` works", {
  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
    Metadata_type = sample(c("x", "y"), 4, replace = TRUE),
    x = rnorm(4),
    y = x + rnorm(4) / 100,
    z = y + rnorm(4) / 1000
  )
  annotation_cols <- c("Metadata_group", "Metadata_type")
  # this is not a great test because it tests more than one function
  sim_df <- matric::sim_calculate(population, method = "pearson")
  row_metadata <- attr(sim_df, "row_metadata")
  sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
  filter_keep <-
    tibble::tibble(Metadata_group = "a", Metadata_type = "x")
  filter_drop <-
    tibble::tibble(Metadata_group = "a", Metadata_type = "x")

  s1 <-
    matric::sim_filter(sim_df, row_metadata, filter_keep = filter_keep, filter_side = "left")
  s2 <-
    matric::sim_filter(sim_df, row_metadata, filter_drop = filter_drop, filter_side = "left")

  expect_equal(
    dplyr::bind_rows(s1, s2) %>% dplyr::arrange(id1, id2),
    sim_df %>% dplyr::arrange(id1, id2)
  )

  s1 <-
    matric::sim_filter(sim_df, row_metadata, filter_keep = NULL, filter_side = "left")
  s2 <-
    matric::sim_filter(sim_df, row_metadata, filter_drop = NULL, filter_side = "left")


  expect_equal(
    s1, s2
  )
})
