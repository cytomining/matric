test_that("`sim_write` works", {

  population <- tibble::tibble(
    Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
    x = rnorm(4),
    y = x + rnorm(4) / 100,
    z = y + rnorm(4) / 1000
  )

  tmp_dir <- tempdir(check = TRUE)

  tmp_path_prefix <- sprintf(file.path(tmp_dir, "test"))

  sim_df <- matric::sim_calculate(population, method = "pearson")

  row_metadata <- attr(sim_df, "row_metadata")
  metric_metadata <- attr(sim_df, "metric_metadata")

  sim_df %>% matric::sim_write(tmp_path_prefix, file_format = "csv")

  sim_file <- file.path(tmp_path_prefix, "test.csv")
  metadata_file <- file.path(tmp_path_prefix, "test_metadata.csv")
  json_file <- file.path(tmp_path_prefix, "test_metadata.json")

  expect_equal(sim_df,
               readr::read_csv(
                 sim_file,
                 col_types = readr::cols(
                   id1 = readr::col_double(),
                   id2 = readr::col_double(),
                   sim = readr::col_double()
                 )
               ),
               ignore_attr = TRUE)

  expect_equal(
    row_metadata,
    readr::read_csv(
      metadata_file,
      col_types = readr::cols(
        id = readr::col_double(),
        Metadata_group = readr::col_character()
      )
    ),
    ignore_attr = TRUE
  )

  expect_equal(metric_metadata$method,
               jsonlite::read_json(json_file)$method[[1]])

  sim_file <- paste(tmp_path_prefix, "parquet", sep = ".")

  sim_df %>% matric::sim_write(sim_file)
  sim_df_in <- arrow::read_parquet(sim_file)

  expect_equal(
    sim_df,
    sim_df_in
  )
})
