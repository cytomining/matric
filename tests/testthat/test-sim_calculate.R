test_that("`sim_calculate` works", {
  # TODO:
  # - Break this out into smaller tests
  #
  population <- tibble::tribble(
    ~Metadata_group, ~x, ~y, ~z,
    1, -1, 5, -5,
    2, 0, 6, -4,
    3, 7, -4, 3,
    4, 14, -8, 6,
    5, -4, 1, -1,
    6, 4, -1, 1
  )

  # ------ Pearson
  sim_df <- matric::sim_calculate(population, method = "pearson")

  expect_equal(
    attr(sim_df, "row_metadata") %>% dplyr::select(-id),
    population %>% dplyr::select(Metadata_group)
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 1 & id2 == 2) %>%
      dplyr::pull("sim"),
    1
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 3 & id2 == 4) %>%
      dplyr::pull("sim"),
    1
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 5 & id2 == 6) %>%
      dplyr::pull("sim"),
    -1
  )

  # ------ Cosine

  sim_df <- matric::sim_calculate(population, method = "cosine")

  expect_equal(
    attr(sim_df, "row_metadata") %>% dplyr::select(-id),
    population %>% dplyr::select(Metadata_group)
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 1 & id2 == 2) %>%
      dplyr::pull("sim"),
    0.97091955
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 3 & id2 == 4) %>%
      dplyr::pull("sim"),
    1
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 5 & id2 == 6) %>%
      dplyr::pull("sim"),
    -1
  )

  # ------ Euclidean

  sim_df <- matric::sim_calculate(population, method = "euclidean")

  expect_equal(
    attr(sim_df, "row_metadata") %>% dplyr::select(-id),
    population %>% dplyr::select(Metadata_group)
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 1 & id2 == 2) %>%
      dplyr::pull("sim"),
    sqrt(3)
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 3 & id2 == 4) %>%
      dplyr::pull("sim"),
    sqrt(sum(c(7, -4, 3)^2))
  )

  expect_equal(
    sim_df %>%
      dplyr::filter(id1 == 5 & id2 == 6) %>%
      dplyr::pull("sim"),
    2 * sqrt(sum(c(-4, 1, -1)^2))
  )

})
