utils::globalVariables(c("id1", "id2"))
#' Calculate melted similarity matrix.
#'
#' \code{sim_calculate} calculates a melted similarity matrix.
#'
#' @param population tbl with annotations (a.k.a. metadata) and observation variables.
#' @param annotation_prefix optional character string specifying prefix for annotation columns.
#' @param method optional character string specifying method for \code{stats::cor} to calculate similarity.  This must be one of the strings \code{"pearson"} (default), \code{"kendall"}, \code{"spearman"}.
#'
#' @return data.frame of melted similarity matrix.
#'
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
#'   x = rnorm(4),
#'   y = x + rnorm(4) / 100,
#'   z = y + rnorm(4) / 1000
#' )
#' matric::sim_calculate(population, method = "pearson")
#' @export
sim_calculate <-
  function(population,
           annotation_prefix = "Metadata_",
           method = "pearson") {
    # get data matrix
    data_matrix <-
      population %>%
      dplyr::select(-dplyr::matches(annotation_prefix))

    # get metadata
    row_metadata <-
      population %>%
      dplyr::select(dplyr::matches(annotation_prefix)) %>%
      tibble::rowid_to_column(var = "id")

    # measure similarities between treatments
    stopifnot(method %in% c("pearson", "kendall", "spearman"))

    sim_df <- stats::cor(t(data_matrix), method = method)

    colnames(sim_df) <- seq(1, ncol(sim_df))

    sim_df %<>%
      tibble::as_tibble() %>%
      tibble::rowid_to_column(var = "id1") %>%
      tidyr::pivot_longer(-id1, names_to = "id2", values_to = "sim") %>%
      dplyr::mutate(id2 = as.integer(id2)) %>%
      dplyr::filter(id1 != id2)

    sim_validate(sim_new(sim_df, row_metadata, list(method = method)))
  }
