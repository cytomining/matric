test_that("`get_annotation` works", {
  population <- tibble::tibble(
    Metadata_group = c(
      "control",
      "control",
      "control",
      "control",
      "experiment",
      "experiment",
      "experiment",
      "experiment"
    ),
    Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
  )

  row_metadata <-
    matric::get_annotation(population, annotation_prefix = "Metadata_")

  expect_equal(
    row_metadata %>% dplyr::select(-id),
    population %>%
      dplyr::select(Metadata_group, Metadata_batch)
  )
})

test_that("`drop_annotation` works", {
  population <- tibble::tibble(
    Metadata_group = c(
      "control",
      "control",
      "control",
      "control",
      "experiment",
      "experiment",
      "experiment",
      "experiment"
    ),
    Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
  )

  expect_equal(
    matric::drop_annotation(population, annotation_prefix = "Metadata_"),
    population %>%
      dplyr::select(AreaShape_Area)
  )
})

test_that("`preprocess_data works", {
  population <- tibble::tibble(
    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7),
    AreaShape_Compactness = c(10, 12, NA, 16, 8, 8, 7, 7)
  )

  expect_equal(
    matric::preprocess_data(population, annotation_prefix = "Metadata_"),
    population %>% dplyr::select(-AreaShape_Compactness)
  )
})
