test_that("`sim_new` works", {
  x <- data.frame(id1 = 1, id2 = 1, sim = 1)

  row_metadata <- data.frame(id = 1, Metadata_type = "a")

  metric_metadata <- list(method = "pearson")

  expect_s3_class(sim_new(x, row_metadata, metric_metadata), "matric_sim")

  expect_error(sim_new(x))
})

test_that("`sim_validate` works", {
  x <- data.frame(id1 = 1, id2 = 1, sim = 1)

  row_metadata <- data.frame(id = 1, Metadata_type = "a")

  row_metadata_bad <- data.frame(id = 2, Metadata_type = "a")

  metric_metadata <- list(method = "pearson")

  expect_s3_class(sim_validate(sim_new(x, row_metadata, metric_metadata)), "matric_sim")

  expect_error(sim_validate(sim_new(x, row_metadata_bad, metric_metadata)))
})


test_that("`sim_restore` works", {
  sim_df <-
    matric::sim_new(
      data.frame(id1 = 1, id2 = 2, sim = 1),
      data.frame(id = c(1, 2), Metadata_group = c("a", "b")),
      list(method = "pearson")
    )
  sim_df_attr <- attributes(sim_df)
  "matric_sim" %in% class(sim_df)
  "matric_sim" %in% class(sim_df %>% dplyr::slice(1))
  expect_false("matric_sim" %in%
    class(
      sim_df %>%
        dplyr::group_by(id1, id2) %>%
        dplyr::summarize(sim = mean(sim), .groups = "keep")
    ))
  expect_true(
    "matric_sim" %in%
      class(
        sim_df %>%
          dplyr::group_by(id1, id2) %>%
          dplyr::summarize(sim = mean(sim), .groups = "keep") %>%
          matric::sim_restore(sim_df_attr)
      )
  )
})
