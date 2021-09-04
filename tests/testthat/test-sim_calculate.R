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

test_that("`sim_calculate` handles `NA`s", {
  population <- tibble::tribble(
    ~Metadata_batch, ~Metadata_group, ~x, ~y, ~z, ~b,
    1, 1, -1, 5, -5, NA,
    1, 2, 0, 6, -4, NA,
    1, 3, 7, -4, 3, NA,
    2, 4, 14, -8, 6, 0,
    2, 5, -4, 1, -1, 2,
    2, 6, 4, -1, 1, -2
  )

  test_na <- function(method) {
    sim_df <-
      matric::sim_calculate(population,
        method = method
      )

    sim_df1 <-
      matric::sim_calculate(population %>%
        dplyr::select(-b),
      method = method
      )

    sim_df2 <-
      matric::sim_calculate(population %>%
        dplyr::filter(Metadata_batch == 2),
      method = method
      )

    expect_equal(
      sim_df %>%
        dplyr::filter(id1 == 1 & id2 == 2) %>%
        dplyr::pull("sim"),
      sim_df1 %>%
        dplyr::filter(id1 == 1 & id2 == 2) %>%
        dplyr::pull("sim")
    )

    expect_equal(
      sim_df %>%
        dplyr::filter(id1 == 3 & id2 == 4) %>%
        dplyr::pull("sim"),
      sim_df1 %>%
        dplyr::filter(id1 == 3 & id2 == 4) %>%
        dplyr::pull("sim")
    )

    expect_lte(
      sim_df %>%
        dplyr::filter(id1 == 5 & id2 == 6) %>%
        dplyr::pull("sim"),
      sim_df2 %>%
        dplyr::filter(id1 == 2 & id2 == 3) %>%
        dplyr::pull("sim")
    )
  }

  test_na("pearson")
  test_na("euclidean")
  test_na("cosine")
})

test_that("stratified `sim_calculate` works", {
  population <- tibble::tribble(
    ~Metadata_batch, ~Metadata_group, ~x, ~y, ~z, ~b,
    1, 1, -1, 5, -5, NA,
    1, 2, 0, 6, -4, NA,
    1, 3, 7, -4, 3, NA,
    2, 4, 14, -8, 6, 0,
    2, 5, -4, 1, -1, 2,
    2, 6, 4, -1, 1, -2
  )

  sim_df <-
    matric::sim_calculate(population,
      method = "pearson"
    )

  row_metadata <- attr(sim_df, "row_metadata")

  sim_df <- sim_df %>%
    sim_annotate(row_metadata,
      annotation_cols =
        c(
          "Metadata_batch",
          "Metadata_group"
        )
    )

  sim_df_s <-
    matric::sim_calculate(population,
      method = "pearson",
      strata = c("Metadata_batch")
    )

  row_metadata_s <- attr(sim_df_s, "row_metadata")

  sim_df_s <- sim_df_s %>%
    sim_annotate(row_metadata_s,
      annotation_cols =
        c(
          "Metadata_batch",
          "Metadata_group"
        )
    )

  expect_equal(
    dplyr::anti_join(
      sim_df_s %>% dplyr::select(-sim),
      sim_df %>% dplyr::select(-sim),
      by = c(
        "id1", "id2",
        "Metadata_batch1", "Metadata_group1",
        "Metadata_batch2", "Metadata_group2"
      )
    ) %>%
      nrow(),
    0
  )

  expect_equal(
    row_metadata,
    row_metadata_s
  )

  expect_equal(
    row_metadata_s %>% dplyr::select(-id),
    population %>% dplyr::select(Metadata_batch, Metadata_group)
  )

  expect_equal(
    sim_df_s %>%
      dplyr::filter(id1 == 1 & id2 == 2) %>%
      dplyr::pull("sim"),
    1
  )

  expect_equal(
    nrow(sim_df_s),
    12
  )

  expect_equal(
    sim_df_s %>%
      dplyr::filter(id1 == 3 & id2 == 4) %>%
      nrow(),
    0
  )

  expect_equal(
    sim_df_s %>%
      dplyr::filter(id1 == 5 & id2 == 6) %>%
      dplyr::pull("sim"),
    -1
  )
})


test_that("stratified `sim_calculate` works", {
  n <- 100
  strata <- c("g1", "g2")

  set.seed(42)
  population <- tibble::tibble(
    g1 = sample(c("a", "b"), n, TRUE),
    g2 = sample(c("a", "b"), n, TRUE),
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  ) %>%
    dplyr::arrange(across(all_of(strata)))

  # ^^ sorting is needed for comparison because the stratified version sorts
  # the rows

  method <- "pearson"

  sim_df <-
    matric::sim_calculate(population,
      annotation_prefix = "g",
      method = method
    )

  row_metadata <- attr(sim_df, "row_metadata")

  sim_df <- sim_df %>%
    sim_annotate(row_metadata, annotation_cols = strata)

  sim_df_s <-
    matric::sim_calculate(population,
      annotation_prefix = "g",
      method = method,
      strata = strata
    )

  row_metadata_s <- attr(sim_df_s, "row_metadata")

  sim_df_s <- sim_df_s %>%
    sim_annotate(row_metadata_s, annotation_cols = strata)

  expect_equal(
    dplyr::anti_join(
      sim_df_s %>% dplyr::select(-sim),
      sim_df %>% dplyr::select(-sim),
      by = c(
        "id1", "id2",
        "g11", "g21",
        "g12", "g22"
      )
    ) %>%
      nrow(),
    0
  )

  expect_equal(
    row_metadata,
    row_metadata_s
  )

  expect_equal(
    row_metadata_s %>% dplyr::select(-id),
    population %>% dplyr::select(g1, g2)
  )
})

test_that("`sim_calculate_ij` works", {
  population <- tibble::tribble(
    ~Metadata_group, ~x, ~y, ~z,
    1, -1, 5, -5,
    2, 0, 6, -4,
    3, 7, -4, 3,
    4, 14, -8, 6,
    5, -4, 1, -1,
    6, 4, -1, 1
  )

  # ------ Cosine

  n <- nrow(population)

  rows <- expand.grid(id1 = seq(n), id2 = seq(n))

  sim_df <- matric::sim_calculate_ij(population, rows, method = "cosine")

  # expect_equal(
  #   attr(sim_df, "row_metadata") %>% dplyr::select(-id),
  #   population %>% dplyr::select(Metadata_group)
  # )

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

})
