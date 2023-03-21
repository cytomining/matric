utils::globalVariables(c("."))
#' Write similarity matrix.
#'
#' \code{sim_write} writes similarity matrix.
#'
#' The output format can be either CSV or Parquet.
#'
#' With the CSV format, the \code{row_metadata} and \code{metric_metadata}
#' attributes are saved as separate files.
#'
#' This is not required for Parquet because it saves the attributes as well.
#'
#' @param sim_df \code{metric_sim} object.
#' @param output character string specifying the output directory or filename.
#' @param file_format character string specify file format. This must be one of \code{csv} or \code{parquet}(default).
#'
#' @return No return value, called for side effects
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
#'   x = rnorm(4),
#'   y = x + rnorm(4) / 100,
#'   z = y + rnorm(4) / 1000
#' )
#' tmpdir <- tempdir()
#' tmpfile_prefix <- file.path(tmpdir, "test")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' sim_df %>% matric::sim_write(tmpfile_prefix, file_format = "csv")
#' readr::read_csv(file.path(tmpfile_prefix, "test.csv"))
#' readr::read_csv(file.path(tmpfile_prefix, "test_metadata.csv"))
#' jsonlite::read_json(file.path(tmpfile_prefix, "test_metadata.json"))
#' sim_df %>% matric::sim_write(paste0(tmpfile_prefix, ".parquet"))
#' sim_df_in <- arrow::read_parquet(paste0(tmpfile_prefix, ".parquet"))
#' attr(sim_df_in, "row_metadata")
#' attr(sim_df_in, "metric_metadata")
#' @export
sim_write <- function(sim_df, output, file_format = "parquet") {
  invisible(sim_validate(sim_df))

  if (file_format == "csv") {
    logger::log_info("Creating {output} ...")
    dir.create(output, showWarnings = FALSE)

    sim_filename <-
      file.path(output, paste(basename(output), file_format, sep = "."))

    row_metadata_filename <-
      file.path(output, paste(paste0(basename(output), "_metadata"), file_format, sep = "."))

    metric_metadata_filename <-
      file.path(output, paste(paste0(basename(output), "_metadata"), "json", sep = "."))

    logger::log_info("Writing {sim_filename} ...")

    sim_df %>% readr::write_csv(sim_filename)

    logger::log_info("Writing {row_metadata_filename} ...")

    attr(sim_df, "row_metadata") %>% readr::write_csv(row_metadata_filename)

    logger::log_info("Writing {metric_metadata_filename} ...")

    attr(sim_df, "metric_metadata") %>% jsonlite::write_json(metric_metadata_filename)
  } else {
    logger::log_info("Writing {output} ...")

    sim_df %>% arrow::write_parquet(output)
  }

  NULL
}


#' Read similarity matrix.
#'
#' \code{sim_read} reads similarity matrix.
#'
#' @param input character string specifying the input filename or directory.
#' @param file_format character string specify file format. This must be one of \code{csv} or \code{parquet}(default).
#'
#' @return \code{metric_sim} object.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
#'   x = rnorm(4),
#'   y = x + rnorm(4) / 100,
#'   z = y + rnorm(4) / 1000
#' )
#' tmpdir <- tempdir()
#' tmpfile_prefix <- file.path(tmpdir, "test")
#' tmpfile_parquet <- file.path(tmpdir, "test.pqrquet")
#' sim_df <- sim_calculate(population, method = "pearson")
#' sim_df %>% sim_write(tmpfile_prefix, file_format = "csv")
#' sim_df_csv <- sim_read(tmpfile_prefix, file_format = "csv")
#' sim_df %>% sim_write(tmpfile_parquet)
#' sim_df_parquet1 <- sim_read(tmpfile_parquet)
#' sim_df %>% arrow::write_parquet(tmpfile_parquet)
#' sim_df_parquet2 <- arrow::read_parquet(tmpfile_parquet)
#' all(sim_df_parquet1 == sim_df_parquet2)
#' all(sim_df_parquet1 == sim_df_csv)
#' @export
sim_read <- function(input, file_format = "parquet") {
  stopifnot(file_format %in% c("csv", "parquet"))

  if (file_format == "csv") {
    sim_filename <-
      file.path(input, paste(basename(input), file_format, sep = "."))

    row_metadata_filename <-
      file.path(input, paste(paste0(basename(input), "_metadata"), file_format, sep = "."))

    metric_metadata_filename <-
      file.path(input, paste(paste0(basename(input), "_metadata"), "json", sep = "."))

    stopifnot(file.exists(sim_filename) &&
      file.exists(row_metadata_filename) &&
      file.exists(metric_metadata_filename))

    logger::log_info("Reading {sim_filename} ...")

    # https://www.tidyverse.org/blog/2018/12/readr-1-3-1/#tibble-subclass
    sim_df <- readr::read_csv(sim_filename, col_types = readr::cols())[]

    logger::log_info("Reading {row_metadata_filename} ...")

    # https://www.tidyverse.org/blog/2018/12/readr-1-3-1/#tibble-subclass
    row_metadata <- readr::read_csv(row_metadata_filename, col_types = readr::cols())[]

    logger::log_info("Reading {metric_metadata_filename} ...")

    metric_metadata <-
      jsonlite::read_json(metric_metadata_filename,
        simplifyVector = TRUE
      )
  } else {
    sim_df <- arrow::read_parquet(input)

    row_metadata <- attr(sim_df, "row_metadata")

    metric_metadata <- attr(sim_df, "metric_metadata")
  }

  sim_validate(sim_new(sim_df, row_metadata, metric_metadata))
}
