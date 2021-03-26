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
#' @param sim_df tbl with melted similarity matrix.
#' @param output character string specifying the output directory or filename.
#' @param file_format character string specify file format. This must be one of \code{csv} or \code{parquet}(default).
#'
#' @return \code{sim_df}.
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
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' sim_df %>% matric::sim_write("/tmp/test", file_format = "csv")
#' readr::read_csv("/tmp/test/test.csv")
#' readr::read_csv("/tmp/test/test_metadata.csv")
#' jsonlite::read_json("/tmp/test/test_metadata.json")
#' sim_df %>% matric::sim_write("/tmp/test.parquet")
#' sim_df_in <- arrow::read_parquet("/tmp/test.parquet")
#' attr(sim_df_in, "row_metadata")
#' attr(sim_df_in, "metric_metadata")
#' @export
sim_write <- function(sim_df, output, file_format = "parquet") {
  stopifnot(!is.null(attr(sim_df, "row_metadata")))

  stopifnot(!is.null(attr(sim_df, "metric_metadata")))

  if (file_format == "csv") {
    futile.logger::flog.info(glue::glue("Creating {output} ..."))
    dir.create(output, showWarnings = FALSE)

    sim_filename <- paste(basename(output), file_format, sep = ".")

    row_metadata_filename <-
      paste(paste0(basename(output), "_metadata"), file_format, sep = ".")

    metric_metadata_filename <-
      paste(paste0(basename(output), "_metadata"), "json", sep = ".")

    sim_filename %<>% file.path(output, .)
    row_metadata_filename %<>% file.path(output, .)
    metric_metadata_filename %<>% file.path(output, .)

    futile.logger::flog.info(glue::glue("Writing {sim_filename} ..."))

    sim_df %>% readr::write_csv(sim_filename)

    futile.logger::flog.info(glue::glue("Writing {row_metadata_filename} ..."))

    attr(sim_df, "row_metadata") %>% readr::write_csv(row_metadata_filename)

    futile.logger::flog.info(glue::glue("Writing {metric_metadata_filename} ..."))

    attr(sim_df, "metric_metadata") %>% jsonlite::write_json(metric_metadata_filename)
  } else {
    futile.logger::flog.info(glue::glue("Writing {output} ..."))

    sim_df %>% arrow::write_parquet(output, compression = "gzip", compression_level = 9)
  }

  NULL
}


#' Read similarity matrix.
#'
#' \code{sim_read} reads similarity matrix.
#'
#' With the CSV format, the \code{row_metadata} and \code{metric_metadata}
#' attributes are read in and added as attributes to the \code{sim_df} tbl.
#'
#' This is not required for Parquet because it the attributes are saved.
#' For Parquet format, \code{sim_read} is identical to
#' \code{arrow::read_parquet}
#'
#' @param input character string specifying the input filename or directory.
#' @param file_format character string specify file format. This must be one of \code{csv} or \code{parquet}(default).
#'
#' @return tbl with similarity matrix.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
#'   x = rnorm(4),
#'   y = x + rnorm(4) / 100,
#'   z = y + rnorm(4) / 1000
#' )
#' sim_df <- sim_calculate(population, method = "pearson")
#' sim_df %>% sim_write("/tmp/test", file_format = "csv")
#' sim_df_csv <- sim_read("/tmp/test", file_format = "csv")
#' sim_df %>% sim_write("/tmp/test.parquet")
#' sim_df_parquet1 <- sim_read("/tmp/test.parquet")
#' sim_df %>% arrow::write_parquet("/tmp/test.parquet")
#' sim_df_parquet2 <- arrow::read_parquet("/tmp/test.parquet")
#' all(sim_df_parquet1 == sim_df_parquet2)
#' all(sim_df_parquet1 == sim_df_csv)
#' @export
sim_read <- function(input, file_format = "parquet") {
  stopifnot(file_format %in% c("csv", "parquet"))

  if (file_format == "csv") {
    sim_filename <-
      paste(basename(input), "csv", sep = ".")

    row_metadata_filename <-
      paste(paste0(basename(input), "_metadata"), file_format, sep = ".")

    metric_metadata_filename <-
      paste(paste0(basename(input), "_metadata"), "json", sep = ".")

    sim_filename %<>% file.path(input, .)
    row_metadata_filename %<>% file.path(input, .)
    metric_metadata_filename %<>% file.path(input, .)

    stopifnot(file.exists(sim_filename) &&
      file.exists(row_metadata_filename) &&
      file.exists(metric_metadata_filename))

    futile.logger::flog.info(glue::glue("Reading {sim_filename} ..."))

    # https://www.tidyverse.org/blog/2018/12/readr-1-3-1/#tibble-subclass
    sim_df <- readr::read_csv(sim_filename)[]

    futile.logger::flog.info(glue::glue("Reading {row_metadata_filename} ..."))

    # https://www.tidyverse.org/blog/2018/12/readr-1-3-1/#tibble-subclass
    attr(sim_df, "row_metadata") <- readr::read_csv(row_metadata_filename)[]

    futile.logger::flog.info(glue::glue("Reading {metric_metadata_filename} ..."))

    attr(sim_df, "metric_metadata") <-
      jsonlite::read_json(metric_metadata_filename,
                        simplifyVector = TRUE)
  } else {
    sim_df <- arrow::read_parquet(input)
  }

  stopifnot(!is.null(attr(sim_df, "row_metadata")) &&
    !is.null(attr(sim_df, "metric_metadata")))

  sim_df
}
