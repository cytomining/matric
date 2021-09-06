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

    # calculate
    if (is.null(strata)) {
      sim_df <-
        sim_calculate_helper(
          population = population,
          annotation_prefix = annotation_prefix,
          method = method,
          lazy = lazy
        )
    } else {
      population <-
        population %>% dplyr::arrange(across(all_of(strata))) # nolint

      reduct <- function(partition, partition_row_indices) {
        population_partition <-
          dplyr::inner_join(population, partition, by = names(partition))

        starting_index <- min(partition_row_indices)

        sim_calculate_helper(
          population = population_partition,
          annotation_prefix = annotation_prefix,
          method = method,
          starting_index = starting_index,
          lazy = lazy
        )
      }

      sim_df <-
        population %>%
        dplyr::select(all_of(strata)) %>%
        dplyr::group_by(across(all_of(strata))) %>%
        dplyr::summarise(reduct(
          dplyr::cur_group(),
          dplyr::cur_group_rows()
        ),
        .groups = "keep"
        ) %>%
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
#' @param lazy optional boolean specifying whether to lazily evaluate
#'   similarity.
#' @param starting_index optional integer specifying starting index.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
#' @noRd
sim_calculate_helper <- function(population,
                                 annotation_prefix = "Metadata_",
                                 method = "pearson",
                                 starting_index = 1,
                                 lazy = FALSE) {
  futile.logger::flog.debug(glue::glue("starting_index = {starting_index}"))

  distances <- c("euclidean")
  correlations <- c("pearson", "kendall", "spearman")
  similarities <- c("cosine")

  stopifnot(is.data.frame(population))

  stopifnot(method %in% c(distances, correlations, similarities))

  # setup similarity dataframe
  n_rows <- nrow(population)

  sim_df <-
    expand.grid(
      id1 = seq(n_rows),
      id2 = seq(n_rows),
      KEEP.OUT.ATTRS = FALSE
    )

  # get data matrix
  X <-
    drop_annotation(population, annotation_prefix) %>%
    as.matrix()

  if (!lazy) {
    if (method %in% distances) {
      S <-
        as.matrix(stats::dist(
          X,
          method = method,
          diag = TRUE,
          upper = TRUE
        ))
    } else if (method %in% correlations) {
      S <-
        stats::cor(t(X),
          method = method,
          use = "pairwise.complete.obs"
        )
    } else if (method %in% similarities) {
      if (method == "cosine") {
        X <- X / sqrt(rowSums(X * X))

        S <- X %*% t(X)
      }
    }

    sim_df <- sim_df %>% dplyr::mutate(sim = as.vector(S))
  }

  sim_df <- sim_df %>%
    dplyr::filter(id1 != id2) %>%
    dplyr::mutate(id1 = id1 + starting_index - 1) %>%
    dplyr::mutate(id2 = id2 + starting_index - 1)

  sim_df
}

#' Calculate similarities given pairs of rows
#'
#' \code{sim_calculate_ij} calculates similarities given pairs of rows.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#'   observation variables.
#' @param index data.frame with at least two columns \code{id1} and
#'   \code{id2} specifying rows of \code{population}, and an attribute
#'   \code{method}, which is a character string specifying method for to
#'   calculate similarity. Currently only \code{"cosine"} is implemented.
#' @param annotation_prefix optional character string specifying prefix
#'   for annotation columns.
#' @param cores optional integer specifying number of CPU cores used for
#'   parallel computing using \code{doParallel}.
#'
#' @return data.frame which is the same as \code{index}, but with a new
#'   column `sim` containing similarities.
#'
#' @importFrom foreach %dopar%
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
#' index <-
#'   expand.grid(id1 = seq(n), id2 = seq(n), KEEP.OUT.ATTRS = FALSE)
#'
#' attr(index, "metric_metadata") <- list(method = "cosine")
#'
#' sim_cosine <- matric::sim_calculate_ij(population, index)
#'
#' sim_cosine
#' @export
sim_calculate_ij <-
  function(population,
           index,
           annotation_prefix = "Metadata_",
           cores = 1) {
    doParallel::registerDoParallel(cores = cores)

    similarities <- c("cosine")

    stopifnot(is.data.frame(index))

    stopifnot(!is.null(attr(index, "metric_metadata")))

    stopifnot(!is.null(attr(index, "metric_metadata")$method))

    method <- attr(index, "metric_metadata")$method

    stopifnot(all(c("id1", "id2") %in% names(index)))

    stopifnot(is.data.frame(population))

    stopifnot(method %in% c(similarities))

    if ("sim" %in% names(index)) {
      index <- index %>% dplyr::select(-sim)
    }

    index_distinct <-
      index %>%
      dplyr::select(id1, id2) %>%
      dplyr::distinct()

    population <- preprocess_data(population)

    # get data matrix
    X <-
      drop_annotation(population, annotation_prefix) %>%
      as.matrix()

    id1 <- index_distinct$id1

    id2 <- index_distinct$id2

    if (method %in% similarities) {
      if (method == "cosine") {
        X <- X / sqrt(rowSums(X * X))

        S <-
          foreach::foreach(i = seq_along(id1), .combine = "c") %dopar%
          sum(X[id1[i], ] * X[id2[i], ])
      }
    }

    sim_df <-
      tibble::tibble(
        id1 = id1,
        id2 = id2,
        sim = as.vector(S)
      )

    index <-
      index %>%
      dplyr::inner_join(sim_df, by = c("id1", "id2"))

    index
  }
