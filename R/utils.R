utils::globalVariables(c("id", "value"))
#' Get row annotations.
#'
#' \code{get_annotation} gets row annotations.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#' observation variables.
#'
#' @param annotation_prefix optional character string specifying prefix for
#' annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return data.frame with row annotations of the same class as
#' \code{population}.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment", "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
#' )
#' matric::get_annotation(population, annotation_prefix = "Metadata_")
#' @export
get_annotation <-
  function(population,
           annotation_prefix = "Metadata_") {
    population %>%
      dplyr::select(dplyr::matches(annotation_prefix)) %>%
      dplyr::mutate(id = seq_len(nrow(population))) %>%
      dplyr::select(id, dplyr::everything())
  }


#' Drop row annotations.
#'
#' \code{drop_annotation} drops row annotations.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#' observation variables.
#'
#' @param annotation_prefix optional character string specifying prefix for
#' annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return data.frame with all columns except row annotations.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment", "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
#' )
#' matric::drop_annotation(population, annotation_prefix = "Metadata_")
#' @export
drop_annotation <-
  function(population,
           annotation_prefix = "Metadata_") {
    population %>%
      dplyr::select(-dplyr::matches(annotation_prefix))
  }

#' Preprocess data.
#' \code{preprocess_data} preprocesses data.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#' observation variables.
#'
#' @param annotation_prefix optional character string specifying prefix for
#' annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return data.frame after preprocessing.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr across any_of matches everything
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7),
#'   AreaShape_Compactness = c(10, 12, NA, 16, 8, 8, 7, 7)
#' )
#' matric::drop_annotation(population, annotation_prefix = "Metadata_")
#' @export
preprocess_data <-
  function(population,
           annotation_prefix = "Metadata_") {
    drop_columns <-
      population %>%
      dplyr::summarise(across(!matches(annotation_prefix), ~ sum(is.na(.)))) %>%
      tidyr::pivot_longer(everything()) %>%
      dplyr::filter(value != 0) %>%
      purrr::pluck("name")

    logger::log_debug("Number of columns before NA filtering = {n}",
      n = ncol(population)
    )

    population <-
      population %>%
      dplyr::select(-any_of(drop_columns))

    logger::log_debug("Number of columns after NA filtering = {n}",
      n = ncol(population)
    )

    population
  }
