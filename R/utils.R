#' Get row annotations.
#'
#' \code{get_annotation} gets row annotations.
#'
#' @param population tbl with annotations (a.k.a. metadata) and observation variables.
#' @param annotation_prefix optional character string specifying prefix for annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return row annotations of the same class as \code{population}
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
      dplyr::mutate(id = seq(nrow(population))) %>%
      dplyr::select(id, dplyr::everything())
  }


#' Drop row annotations.
#'
#' \code{drop_annotation} drops row annotations.
#'
#' @param population tbl with annotations (a.k.a. metadata) and observation variables.
#' @param annotation_prefix optional character string specifying prefix for annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return data with all columns except row annotations of the same class as \code{population}
#' @export
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
