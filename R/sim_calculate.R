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
#' @param reference optional character string specifying reference.
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
           lazy = FALSE,
           all_same_cols_rep_or_group = NULL,
           all_same_cols_ref = NULL,
           reference = NULL) {
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
          lazy = lazy,
          all_same_cols_rep_or_group = all_same_cols_rep_or_group,
          all_same_cols_ref = all_same_cols_ref,
          reference = reference
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
#' @param starting_index optional integer specifying starting index.
#' @param lazy opational boolean specifying whether to lazily evaluate
#'   similarity.
#' @param all_same_cols_rep_or_group optional character vector specifying
#'   columns.
#' @param all_same_cols_ref optional character vector specifying columns.
#' @param reference optional character string specifying reference.
#'
#' @return \code{metric_sim} object, with similarity matrix and related metadata
#' @noRd
sim_calculate_helper <- function(population,
                                 annotation_prefix = "Metadata_",
                                 method = "pearson",
                                 starting_index = 1,
                                 lazy = FALSE,
                                 all_same_cols_rep_or_group = NULL,
                                 all_same_cols_ref = NULL,
                                 reference = NULL) {
  futile.logger::flog.debug(glue::glue("starting_index = {starting_index}"))

  distances <- c("euclidean")
  correlations <- c("pearson", "kendall", "spearman")
  similarities <- c("cosine")

  stopifnot(is.data.frame(population))

  stopifnot(method %in% c(distances, correlations, similarities))

  # setup similarity dataframe

  n_rows <- nrow(population)

  if (!lazy) {
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

    sim_df <-
      sim_df %>%
      dplyr::mutate(sim = as.vector(S))

    # filter out diagonal here only if not lazy
    # if lazy then filter out diagonal in `sim_calculate_ij`
    sim_df <- sim_df %>%
      dplyr::filter(id1 != id2)

  } else {

    sim_df <-
      expand.grid(
        id1 = seq(n_rows),
        id2 = seq(n_rows),
        KEEP.OUT.ATTRS = FALSE
      )

  }

  sim_df <- sim_df %>%
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
#'   Preserve the diagonal entries when constructing \code{index} to allow for
#'   optimizations. \code{sim_calculate_ij} filters out the diagonal in the
#'   result.
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
#' attr(index, "metric_metadata") <- list(method = "cosine")
#'
#' sim_cosine <- matric::sim_calculate_ij(population, index)
#'
#' sim_cosine
#'
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

    if (method %in% similarities) {
      if (method == "cosine") {
        sim_df <- cosine_sparse(X, index_distinct$id1, index_distinct$id2)
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

#' Compute cross product between two sets of rows of a matrix.
#'
#' \code{tcrossprod_ij} computes cross product between two sets of rows of a
#'   matrix.
#'
#' @param X matrix
#' @param id1 vector of integers specifying the list of rows of \code{X}
#'   (first set)
#' @param id2 vector of integers specifying the list of rows of \code{X},
#' (second set), same length as \code{id1}.
#'
#' @return matrix containing the cross product of \code{X[id1, ]} and
#'   \code{X[id2, ]}.
#'
#' @examples
#'
#' set.seed(42)
#' X <- matrix(rnorm(5 * 3), 5, 3)
#'
#' id1 <- c(1, 3)
#' id2 <- c(5, 4)
#'
#' (s1 <- matric::tcrossprod_ij(X, id1, id2))
#'
#' (s2 <- tcrossprod(X)[id1, id2])
#'
#' all.equal(s1, s2)
#'
#' @export
tcrossprod_ij <- function(X, id1, id2) {
  X1 <- X[id1, ]
  X2 <- X[id2, ]
  n1 <- length(id1)
  n2 <- length(id2)
  n <- ncol(X)
  if (n1 == 1) {
    X1 <- matrix(X1, 1, n)
  }
  if (n2 == 1) {
    X2 <- matrix(X2, 1, n)
  }
  tcrossprod(X1, X2)
}

#' Compute cosine similarity between pairs of rows of a matrix
#'
#' \code{cosine_sparse} computes cosine similarity between pairs of rows of a
#'   matrix.
#'
#' @param X matrix
#' @param id1 vector of integers specifying the list of rows of \code{X}
#'   (first set)
#' @param id2 vector of integers specifying the list of rows of \code{X},
#' (second set), same length as \code{id1}.
#'
#' @return data.frame with the same number of rows as the length of \code{id1}
#'   (and \code{id2}) containing the cosine similarity between the pairs of rows
#'   of \code{X}. \code{sim[i] == cosine(X[id1[i], ], X[id2[i], ])}.
#'
#' @examples
#'
#' set.seed(42)
#' X <- matrix(rnorm(5 * 3), 5, 3)
#'
#' id1 <- c(1, 3)
#' id2 <- c(5, 4)
#'
#' s1 <- matric::cosine_sparse(X, id1, id2) %>% dplyr::arrange(id1, id2)
#'
#' Xn <- X / sqrt(rowSums(X * X))
#'
#' n_rows <- nrow(Xn)
#'
#' s2 <-
#'   expand.grid(
#'     id1 = seq(n_rows),
#'     id2 = seq(n_rows),
#'     KEEP.OUT.ATTRS = FALSE
#'   ) %>%
#'   dplyr::mutate(sim = as.vector(tcrossprod(Xn))) %>%
#'   dplyr::inner_join(s1 %>% dplyr::select(id1, id2)) %>%
#'   dplyr::arrange(id1, id2)
#'
#' s1
#'
#' all.equal(s1, s2)
#' @export
cosine_sparse <- function(X, id1, id2) {
  X <- X / sqrt(rowSums(X * X))

  index_nest <-
    data.frame(id1, id2) %>%
    dplyr::arrange(id1, id2) %>%
    dplyr::group_by(id1) %>%
    tidyr::nest(id2_l = c(id2)) %>%
    dplyr::group_by(id2_l) %>%
    tidyr::nest(id1_l = c(id1))

  sim_df <-
    purrr::map2_dfr(
      index_nest$id1_l,
      index_nest$id2_l,
      function(l1, l2) {
        index_sub <-
          expand.grid(
            id1 = l1[[1]],
            id2 = l2[[1]],
            KEEP.OUT.ATTRS = FALSE
          )

        S <-
          as.vector(tcrossprod_ij(X, l1[[1]], l2[[1]]))

        index_sub <- index_sub %>% dplyr::mutate(sim = S)

        index_sub
      }
    ) %>%
    dplyr::arrange(id1, id2)

  sim_df
}
