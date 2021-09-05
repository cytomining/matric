utils::globalVariables(c("id1", "id2", "i", "sim"))
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
#' @param lazy optional boolean specifying whether to lazily evaluate
#'   similarity.
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
           method = "pearson",
           lazy = FALSE) {
    population <- preprocess_data(population)

    if (lazy) {
      helper <- sim_calculate_helper_lazy
    } else {
      helper <- sim_calculate_helper
    }

    # calculate
    if (is.null(strata)) {
      sim_df <-
        helper(
          population = population,
          annotation_prefix = annotation_prefix,
          method = method
        )
    } else {
      population <-
        population %>% dplyr::arrange(across(all_of(strata))) # nolint

      reduct <- function(partition, partition_row_indices) {
        population_partition <-
          dplyr::inner_join(population, partition, by = names(partition))

        starting_index <- min(partition_row_indices)

        helper(
          population = population_partition,
          annotation_prefix = annotation_prefix,
          method = method,
          starting_index = starting_index
        )
      }

      sim_df <-
        population %>%
        dplyr::select(all_of(strata)) %>%
        dplyr::group_by(across(all_of(strata))) %>%
        dplyr::summarise(reduct(dplyr::cur_group(),
                                dplyr::cur_group_rows()),
                         .groups = "keep") %>%
        dplyr::ungroup() %>%
        dplyr::select(id1, id2, sim)
    }

    # get metadata
    row_metadata <-
      get_annotation(population, annotation_prefix) # nolint

    # construct object
    sim_df <-
      sim_validate(sim_new(sim_df, row_metadata, list(method = method)))

    sim_df
  }

#' Helper function to calculate a melted similarity matrix.
#'
#' \code{sim_calculate_helper} helps calculate a melted similarity matrix.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#'   observation variables.
#' @param annotation_prefix optional character string specifying prefix
#'   for annotation columns.
#' @param method optional character string specifying method for
#'   to calculate similarity. This must be one of the
#'   strings \code{"pearson"} (default), \code{"kendall"}, \code{"spearman"},
#'   \code{"euclidean"}, \code{"cosine"}.
#' @param starting_index optional integer specifying starting index.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
#' @noRd
sim_calculate_helper <- function(population,
                                 annotation_prefix = "Metadata_",
                                 method = "pearson",
                                 starting_index = 1) {
  futile.logger::flog.debug(glue::glue("starting_index = {starting_index}"))

  distances <- c("euclidean")
  correlations <- c("pearson", "kendall", "spearman")
  similarities <- c("cosine")

  stopifnot(is.data.frame(population))

  stopifnot(method %in% c(distances, correlations, similarities))

  # get data matrix
  data_matrix <-
    drop_annotation(population, annotation_prefix) # nolint

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
                 use = "pairwise.complete.obs")
  } else if (method %in% similarities) {
    if (method == "cosine") {
      data_matrix <-
        data_matrix / apply(data_matrix, 1, function(x) {
          sqrt(sum(x ^ 2, na.rm = TRUE))
        })

      sim_df <-
        as.matrix(stats::dist(
          data_matrix,
          method = "euclidean",
          diag = TRUE,
          upper = TRUE
        ))

      sim_df <- 1 - (sim_df ^ 2) / 2
    }
  }

  colnames(sim_df) <- seq(1, ncol(sim_df))

  sim_df <- sim_df %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column(var = "id1") %>%
    tidyr::pivot_longer(-id1, names_to = "id2", values_to = "sim") %>%
    dplyr::mutate(id2 = as.integer(id2)) %>%
    dplyr::filter(id1 != id2) %>%
    dplyr::mutate(id1 = id1 + starting_index - 1) %>%
    dplyr::mutate(id2 = id2 + starting_index - 1)

  sim_df
}

#' Helper function to calculate a melted similarity matrix.
#'
#' \code{sim_calculate_helper_lazy} helps calculate a melted similarity matrix.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#'   observation variables.
#' @param starting_index optional integer specifying starting index.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
#' @noRd
sim_calculate_helper_lazy <- function(population,
                                      annotation_prefix = "Metadata_",
                                      method = "pearson",
                                      starting_index = 1) {
  futile.logger::flog.debug(glue::glue("starting_index = {starting_index}"))

  stopifnot(is.data.frame(population))

  n_rows <- nrow(population)

  sim_df <- expand.grid(id1 = seq(n_rows), id2 = seq(n_rows))

  sim_df <- sim_df %>%
    dplyr::filter(id1 != id2) %>%
    dplyr::mutate(id1 = id1 + starting_index - 1) %>%
    dplyr::mutate(id2 = id2 + starting_index - 1) %>%
    dplyr::mutate(sim = NA)

  sim_df
}

#' Calculate similarities given pairs of rows
#'
#' \code{sim_calculate_ij} calculates similarities given pairs of rows.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#'   observation variables.
#' @param rows data.frame with at least two columns \code{id1} and \code{id2}
#'   specifying rows of `population`.
#' @param annotation_prefix optional character string specifying prefix
#'   for annotation columns.
#' @param method optional character string specifying method for
#'   to calculate similarity. Currently only \code{"cosine"} is implemented.
#'
#' @return \code{rows}, with new column `sim` containing similarities.
#'
#' @importFrom foreach %do%
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
#'
#' n <- nrow(population)
#'
#' rows <- expand.grid(id1 = seq(n), id2 = seq(n))
#'
#' sim_cosine <- matric::sim_calculate_ij(population, rows, method = "cosine")
#'
#' sim_cosine
#' @export
sim_calculate_ij <-
  function(population,
           rows,
           annotation_prefix = "Metadata_",
           method = "cosine") {
    similarities <- c("cosine")

    stopifnot(is.data.frame(rows))

    stopifnot(all(c("id1", "id2") %in% names(rows)))

    stopifnot(is.data.frame(population))

    stopifnot(method %in% c(similarities))

    if ("sim" %in% names(rows)) {
      rows <- rows %>% dplyr::select(-sim)

    }

    population <- preprocess_data(population)

    # get data matrix
    X <-
      drop_annotation(population, annotation_prefix) %>%
      as.matrix()

    id1 <- rows$id1

    id2 <- rows$id2

    if (method %in% similarities) {
      if (method == "cosine") {
        X <- X / sqrt(rowSums(X * X))

        S <-
          foreach::foreach(i = seq_along(id1), .combine = "c") %do%
          sum(X[id1[i], ] * X[id2[i], ])

      }
    }

    sim_df <-
      tibble::tibble(id1 = id1,
                     id2 = id2,
                     sim = as.vector(S))


    if (!purrr::is_empty(setdiff(names(rows), c("id1", "id2")))) {
      rows <-
        rows %>%
        dplyr::inner_join(sim_df, by = c("id1", "id2"))

    } else {
      rows <- sim_df
    }

    rows
  }
