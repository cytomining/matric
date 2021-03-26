test_that("`new_sim` works", {
  x <- data.frame(id1 = 1, id2 = 1, sim = 1)

  row_metadata <- data.frame(id = 1, Metadata_type = "a")

  metric_metadata <- list(method = "pearson")

  expect_s3_class(new_sim(x, row_metadata, metric_metadata), "sim")

  expect_error(new_sim(x))
})

test_that("`validate_sim` works", {
  x <- data.frame(id1 = 1, id2 = 1, sim = 1)

  row_metadata <- data.frame(id = 1, Metadata_type = "a")

  row_metadata_bad <- data.frame(id = 2, Metadata_type = "a")

  metric_metadata <- list(method = "pearson")

  expect_s3_class(validate_sim(new_sim(x, row_metadata, metric_metadata)), "sim")

  expect_error(validate_sim(new_sim(x, row_metadata_bad, metric_metadata)))
})
