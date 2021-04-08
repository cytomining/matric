utils::globalVariables(c("id1", "id2"))
#' Calculate a melted similarity matrix.
#'
#' \code{sim_calculate} calculates a melted similarity matrix.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#'   observation variables.
#' @param annotation_prefix optional character string specifying prefix
#'   for annotation columns.
#' @param strata optional character vector specifying stratification columns.
#' @param method optional character string specifying method for
#'   to calculate similarity. This must be one of the
#'   strings \code{"pearson"} (default), \code{"kendall"}, \code{"spearman"},
#'   \code{"euclidean"}, \code{"cosine"}.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tribble(
#'   ~Metadata_group, ~x, ~y, ~z,
#'   1, -1, 5, -5,
#'   2, 0, 6, -4,
#'   3, 7, -4, 3,
#'   4, 14, -8, 6
#' )
#' sim_pearson <- matric::sim_calculate(population, method = "pearson")
#' sim_cosine <- matric::sim_calculate(population, method = "cosine")
#' sim_euclidean <- matric::sim_calculate(population, method = "euclidean")
#'
#' sim_pearson %>%
#'   dplyr::inner_join(sim_cosine,
#'     by = c("id1", "id2"),
#'     suffix = c("_pearson", "_cosine")
#'   ) %>%
#'   dplyr::inner_join(sim_euclidean %>% dplyr::rename(sim_euclidean = sim),
#'     by = c("id1", "id2")
#'   )
#' @export
sim_calculate <-
  function(population,
           annotation_prefix = "Metadata_",
           strata = NULL,
           method = "pearson") {
    stopifnot(is.data.frame(population))

    # get data matrix
    data_matrix <- drop_annotation(population, annotation_prefix)

    # get metadata
    row_metadata <- get_annotation(population, annotation_prefix)

    # calculate
    sim_df <- sim_calculate_helper(data_matrix, method)

    # construct object
    sim_df <- sim_validate(sim_new(sim_df, row_metadata, list(method = method)))

    sim_df
  }

#' Helper function to calculate a melted similarity matrix.
#'
#' \code{sim_calculate_helper} helps calculate a melted similarity matrix.
#'
#' @param data_matrix matrix with observation variables.
#' @param method optional character string specifying method for
#'   to calculate similarity. This must be one of the
#'   strings \code{"pearson"} (default), \code{"kendall"}, \code{"spearman"},
#'   \code{"euclidean"}, \code{"cosine"}.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
sim_calculate_helper <- function(data_matrix, method = "pearson") {
  distances <- c("euclidean")
  correlations <- c("pearson", "kendall", "spearman")
  similarities <- c("cosine")

  stopifnot(method %in% c(distances, correlations, similarities))

  # drop NA
  # TODO:
  #   - Handle this more elegantly
  futile.logger::flog.debug(
    glue::glue("Number of columns before NA filtering = {n}",
               n = ncol(data_matrix)))

  data_matrix <- Filter(function(x)!any(is.na(x)), data_matrix)

  futile.logger::flog.debug(
    glue::glue("Number of columns after NA filtering = {n}",
               n = ncol(data_matrix)))

  if (method %in% distances) {
    sim_df <-
      as.matrix(stats::dist(
        data_matrix,
        method = method,
        diag = TRUE,
        upper = TRUE
      ))
  } else if (method %in% correlations) {
    sim_df <-
      stats::cor(t(data_matrix),
                 method = method,
                 use = "pairwise.complete.obs"
      )
  } else if (method %in% similarities) {
    if (method == "cosine") {
      data_matrix <-
        data_matrix / apply(data_matrix, 1, function(x) {
          sqrt(sum(x^2, na.rm = TRUE))
        })

      sim_df <-
        as.matrix(stats::dist(
          data_matrix,
          method = "euclidean",
          diag = TRUE,
          upper = TRUE
        ))

      sim_df <- 1 - (sim_df^2) / 2
    }
  }

  colnames(sim_df) <- seq(1, ncol(sim_df))

  sim_df <- sim_df %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(var = "id1") %>%
    tidyr::pivot_longer(-id1, names_to = "id2", values_to = "sim") %>%
    dplyr::mutate(id2 = as.integer(id2)) %>%
    dplyr::filter(id1 != id2)
}


map_df_stratified <- function(.x, .f, .strata, ...) {
  reduct <- function(partition) {
    dplyr::inner_join(.x, partition, by = names(partition)) %>%
      .f(...)
  }

  output <-
    .x %>%
    dplyr::select(all_of(.strata)) %>%
    dplyr::group_by(across(all_of(.strata))) %>%
    dplyr::summarise(reduct(dplyr::cur_group()), .groups = "keep") %>%
    dplyr::ungroup()

  output
}
