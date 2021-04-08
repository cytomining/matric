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
    distances <- c("euclidean")
    correlations <- c("pearson", "kendall", "spearman")
    similarities <- c("cosine")

    stopifnot(method %in% c(distances, correlations, similarities))

    stopifnot(is.data.frame(population))

    # get data matrix
    data_matrix <- drop_annotation(population, annotation_prefix)

    futile.logger::flog.debug(
      glue::glue("Number of columns before NA filtering = {n}",
                 n = ncol(data_matrix)))

    # drop NA
    data_matrix <- Filter(function(x)!any(is.na(x)), data_matrix)

    futile.logger::flog.debug(
      glue::glue("Number of columns after NA filtering = {n}",
                 n = ncol(data_matrix)))

    # get metadata
    row_metadata <- get_annotation(population, annotation_prefix)

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

    sim_validate(sim_new(sim_df, row_metadata, list(method = method)))
  }
