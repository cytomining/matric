utils::globalVariables(c("id1", "id2", "id1_l", "id2_l", "i", "sim"))
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
#' @param all_same_cols_rep_or_group optional character vector specifying
#'   columns.
#' @param all_same_cols_ref optional character vector specifying columns.
#' @param all_same_cols_rep_ref optional character vector specifying columns.
#' @param reference optional character string specifying reference.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tribble(
#'   ~Metadata_group1, ~Metadata_group2, ~x, ~y, ~z,
#'   1, 1, -1, 5, -5,
#'   1, 2, -1.2, 5.1, -5.2,
#'   2, 1, 0, 6, -4,
#'   2, 2, 0.3, 6.2, -4.4,
#'   3, 1, 7, -4, 3,
#'   3, 2, 7.2, -4.1, 3.7
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
#'
#' sim_cosine <-
#'   matric::sim_calculate(population,
#'     strata = "Metadata_group1",
#'     method = "cosine",
#'     lazy = TRUE
#'   )
#'
#' matric::sim_calculate(population,
#'   method = "cosine",
#'   lazy = TRUE,
#'   all_same_cols_rep_or_group = c("Metadata_group2")
#' )
#'
#' matric::sim_calculate(population,
#'   method = "cosine",
#'   lazy = TRUE,
#'   all_same_cols_rep_or_group = c("Metadata_group2"),
#'   all_same_cols_ref = c("Metadata_group1"),
#'   reference = data.frame(Metadata_group2 = 2)
#' )
#'
#' matric::sim_calculate(population,
#'   method = "cosine",
#'   lazy = TRUE,
#'   all_same_cols_rep_or_group = c("Metadata_group2"),
#'   all_same_cols_ref = c("Metadata_group1"),
#'   all_same_cols_rep_ref = c("Metadata_group2"),
#'   reference = data.frame(Metadata_group2 = 2)
#' )
#' @export
sim_calculate <-
  function(population,
           annotation_prefix = "Metadata_",
           strata = NULL,
           method = "pearson",
           lazy = FALSE,
           all_same_cols_rep_or_group = NULL,
           all_same_cols_ref = NULL,
           all_same_cols_rep_ref = NULL,
           reference = NULL) {
    # ---- Checks ----
    stopifnot(is.null(strata) | is.null(all_same_cols_rep_or_group))

    if (!is.null(strata)) {
      all_same_cols_rep_or_group <- strata
    }

    distances <- c("euclidean")
    correlations <- c("pearson", "kendall", "spearman")
    similarities <- c("cosine")

    stopifnot(is.data.frame(population))

    stopifnot(method %in% c(distances, correlations, similarities))

    # ---- Setup ----
    calculate_optimized <-
      !is.null(all_same_cols_rep_or_group) |
        !is.null(all_same_cols_ref) |
        !is.null(all_same_cols_rep_ref) |
        !is.null(reference)

    sim_calculate_cases <- function(X) {
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
          stats::cor(t(X), method = method, use = "pairwise.complete.obs")
      } else if (method %in% similarities) {
        if (method == "cosine") {
          S <- tcrossprod(X / sqrt(rowSums(X * X)))
        }
      }

      S
    }

    # ---- Preprocess ----
    population <- preprocess_data(population)

    # ---- Calculate similarity ----
    if (!calculate_optimized) {
      # ---- * All pairs  ----

      n_rows <- nrow(population)

      sim_df <-
        expand.grid(
          id1 = seq(n_rows),
          id2 = seq(n_rows),
          KEEP.OUT.ATTRS = FALSE
        )

      if (!lazy) {
        X <-
          drop_annotation(population, annotation_prefix) %>%
          as.matrix()

        S <- sim_calculate_cases(X)

        sim_df <-
          sim_df %>%
          dplyr::mutate(sim = as.vector(S)) %>%
          dplyr::filter(id1 != id2)
      }
    } else {
      # ---- * Some pairs  ----

      # ---- ** Setup  ----

      metadata <- get_annotation(population, annotation_prefix)

      mapper <- function(strata, reductor, metadata_subset) {
        metadata %>%
          dplyr::select(all_of(strata)) %>%
          dplyr::group_by(across(all_of(strata))) %>%
          dplyr::summarise(reductor(dplyr::cur_group(), metadata_subset), .groups = "keep") %>%
          dplyr::ungroup() %>%
          dplyr::select(id1, id2)
      }

      reduct_all_same_cols_rep_or_group <-
        function(partition, metadata_subset) {
          id_partition <-
            dplyr::inner_join(metadata_subset,
              partition,
              by = names(partition)
            ) %>%
            purrr::pluck("id")

          expand.grid(
            id1 = id_partition,
            id2 = id_partition,
            KEEP.OUT.ATTRS = FALSE
          )
        }

      reduct_all_same_cols_ref <-
        function(partition, metadata_subset) {
          metadata_partition <-
            dplyr::inner_join(metadata_subset,
              partition,
              by = names(partition)
            )

          id_reference <-
            dplyr::inner_join(metadata_partition,
              reference,
              by = names(reference)
            ) %>%
            purrr::pluck("id")

          id_non_reference <-
            dplyr::anti_join(metadata_partition,
              reference,
              by = names(reference)
            ) %>%
            purrr::pluck("id")

          expand.grid(
            id1 = id_non_reference,
            id2 = id_reference,
            KEEP.OUT.ATTRS = FALSE
          )
        }

      reduct_all_same_cols_rep_ref <-
        function(partition, metadata_subset) {
          id_reference <-
            dplyr::inner_join(metadata_subset,
              partition,
              by = names(partition)
            ) %>%
            purrr::pluck("id")

          expand.grid(
            id1 = id_reference,
            id2 = id_reference,
            KEEP.OUT.ATTRS = FALSE
          )
        }

      # ---- ** Calculate  ----

      sim_df <- data.frame()

      if (!is.null(all_same_cols_rep_or_group)) {
        if (!is.null(reference)) {
          metadata_subset <-
            metadata %>%
            dplyr::anti_join(reference, by = names(reference))
        } else {
          metadata_subset <- metadata
        }

        sim_df <-
          dplyr::bind_rows(
            sim_df,
            mapper(
              all_same_cols_rep_or_group,
              reduct_all_same_cols_rep_or_group,
              metadata_subset
            )
          )
      }

      if (!is.null(reference)) {
        metadata_subset <-
          metadata

        sim_df <-
          dplyr::bind_rows(
            sim_df,
            mapper(
              all_same_cols_ref,
              reduct_all_same_cols_ref,
              metadata_subset
            )
          )
      }

      if (!is.null(all_same_cols_rep_ref) & !is.null(reference)) {
        metadata_subset <-
          metadata %>%
          dplyr::inner_join(reference, by = names(reference))

        sim_df <-
          dplyr::bind_rows(
            sim_df,
            mapper(
              all_same_cols_rep_ref,
              reduct_all_same_cols_rep_ref,
              metadata_subset
            )
          )
      }

      if (!lazy) {
        sim_df <- sim_calculate_ij(population, sim_df, method = method, annotation_prefix = annotation_prefix)
      }

      sim_df
    }

    # ---- Package results ----

    # get metadata
    row_metadata <-
      get_annotation(population, annotation_prefix)

    # construct object
    sim_df <-
      sim_validate(sim_new(sim_df, row_metadata, list(method = method)))

    sim_df
  }

#' Calculate similarities given pairs of rows
#'
#' \code{sim_calculate_ij} calculates similarities given pairs of rows.
#'
#' @param population data.frame with annotations (a.k.a. metadata) and
#'   observation variables.
#' @param index data.frame with at least two columns \code{id1} and
#'   \code{id2} specifying rows of \code{population}, and an optional attribute
#'   \code{metric_metadata$method}, which is a character string specifying
#    method for to calculate similarity. Currently only \code{"cosine"} is
#'   implemented. Preserve the diagonal entries when constructing \code{index}
#    to allow for  optimizations. \code{sim_calculate_ij} filters out the
#    diagonal in the result.
#' @param method optional character string specifying method for
#'   to calculate similarity. \code{method}, if specified, overrides
#'   \code{attr(index, "metric_metadata")$method}.
#' @param annotation_prefix optional character string specifying prefix
#'   for annotation columns.
#' @param cores optional integer specifying number of CPU cores used for
#'   parallel computing using \code{doParallel}.
#'
#' @return data.frame which is the same as \code{index}, but with a new
#'   column `sim` containing similarities, and with the diagonals filtered out.
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
#' matric::sim_calculate_ij(population, index, method = "cosine")
#'
#' attr(index, "metric_metadata") <- list(method = "cosine")
#'
#' matric::sim_calculate_ij(population, index)
#' @export
sim_calculate_ij <-
  function(population,
           index,
           method = NULL,
           annotation_prefix = "Metadata_",
           cores = 1) {
    doParallel::registerDoParallel(cores = cores)

    correlations <- c("pearson")
    similarities <- c("cosine")

    stopifnot(is.data.frame(index))

    stopifnot(!is.null(attr(index, "metric_metadata")) |
      !is.null(method))

    stopifnot(!is.null(attr(index, "metric_metadata")$method) |
      !is.null(method))

    if (is.null(method)) {
      method <- attr(index, "metric_metadata")$method
    } else {
      method -> attr(index, "metric_metadata")$method
    }

    stopifnot(all(c("id1", "id2") %in% names(index)))

    stopifnot(is.data.frame(population))

    stopifnot(method %in% c(correlations, similarities))

    if ("sim" %in% names(index)) {
      index <- index %>% dplyr::select(-sim)
    }

    # remove duplicates because it will be inner joined back later
    index_distinct <-
      index %>%
      dplyr::select(id1, id2) %>%
      dplyr::distinct()

    population <- preprocess_data(population)

    # get data matrix
    X <-
      drop_annotation(population, annotation_prefix) %>%
      as.matrix()

    if (method %in% c(correlations, similarities)) {
      if (method == "cosine") {
        sim_df <- cosine_sparse(X, index_distinct$id1, index_distinct$id2)
      }
      if (method == "pearson") {
        sim_df <- pearson_sparse(X, index_distinct$id1, index_distinct$id2)
      }
    }

    # filter out diagonal here
    sim_df <- sim_df %>%
      dplyr::filter(id1 != id2)

    index <-
      index %>%
      dplyr::inner_join(sim_df, by = c("id1", "id2"))

    index
  }
