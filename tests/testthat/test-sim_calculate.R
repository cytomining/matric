test_that("`sim_calculate` works", {
  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
    x = rnorm(4),
    y = x + rnorm(4) / 100,
    z = y + rnorm(4) / 1000
  )
  sim_df <- matric::sim_calculate(population, method = "pearson")
  expect_equal(attr(sim_df, "row_metadata") %>% dplyr::select(-id),
               population %>% dplyr::select(Metadata_group))
})
