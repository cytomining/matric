test_that("`sim_wider` works", {
  n <- 10

  set.seed(42)
  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b", "c", "d"), n, replace = TRUE),
    x1 <- rnorm(n),
    x2 <- rnorm(n),
    x3 <- rnorm(n),
    x4 <- rnorm(n),
    x5 <- rnorm(n)
  )

  population$Metadata_id <- as.character(seq(nrow(population)))
  metadata <- matric::get_annotation(population)
  annotation_cols <- c("Metadata_group", "Metadata_id")

  sim_df <- matric::sim_calculate(population, method = "pearson")
  row_metadata <- attr(sim_df, "row_metadata")
  sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)

  annotation_column <- "Metadata_group"
  primary_key_column <- "Metadata_id"

  res <-
    matric::sim_wider(sim_df, annotation_column, primary_key_column)

  resmat <-
    as.matrix(res)

  expect_true(is.na(unique(diag(resmat))))

  expect_equal(
    sort(resmat[upper.tri(resmat)]),
    sort(resmat[lower.tri(resmat)])
  )

  mapped <-
    data.frame(id = rownames(res)) %>%
    dplyr::inner_join(attr(res, "map"), by = "id")

  expect_equal(
    population %>%
      dplyr::distinct(across(all_of(
        c(primary_key_column, annotation_column)
      ))) %>%
      dplyr::arrange(across(all_of(
        c(primary_key_column, annotation_column)
      ))) %>%
      as.data.frame() %>%
      unname(),
    mapped %>%
      dplyr::distinct(primary_key, annotation) %>%
      dplyr::arrange(primary_key, annotation) %>%
      unname(),
    ignore_attr = TRUE
  )
})
