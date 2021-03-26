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

test_that("`preserve_sim` works", {
  x <- data.frame(id1 = 1, id2 = 1, sim = 1)

  row_metadata <- data.frame(id = 1, Metadata_type = "a")

  row_metadata_bad <- data.frame(id = 2, Metadata_type = "a")

  metric_metadata <- list(method = "pearson")

  x <- sim_new(x, row_metadata, metric_metadata)

  invisible(sim_validate(x))

  x <- x %>%
    group_by(id1) %>%
    mutate(sim = sim / 2)

  expect_error(invisible(sim_validate(x)))

  x <- sim_new(x, row_metadata, metric_metadata)

  attr_x <- attributes(x)

  x <- x %>%
    group_by(id1) %>%
    mutate(sim = sim / 2) %>%
    sim_restore(attr_x)

  expect_s3_class(sim_validate(x), "matric_sim")
})
