#' Compute similarity between pairs of rows of a matrix
#'
#' \code{sparse_pairwise} computes similarity between pairs of rows of a
#'   matrix.
#'
#' @param X matrix
#' @param id1 vector of integers specifying the list of rows of \code{X}
#'   (first set)
#' @param id2 vector of integers specifying the list of rows of \code{X},
#'   (second set), same length as \code{id1}.
#' @param pairwise_function function that takes a matrix and a pair of indices
#'   specifying rows of the matrix, and computes an operation of each pair of
#'   rows
#' @param use_furrr boolean indicating whether to use the furrr library
#'   for parallel processing.
#'
#' @return data.frame with the same number of rows as the length of \code{id1}
#'   (and \code{id2}) containing the similarity between the pairs of rows
#'   of \code{X}. \code{sim[i] == pairwise_function(X[id1[i], ], X[id2[i], ])}.
sparse_pairwise <- function(X, id1, id2, pairwise_function, use_furrr = FALSE) {
  if (use_furrr) {
    logger::log_trace("Using furrr for parallel processing")
    x_map2_dfr <- furrr::future_map2_dfr
  } else {
    logger::log_trace("Using purrr for sequential processing")
    x_map2_dfr <- purrr::map2_dfr
  }

  index_nest <-
    data.frame(id1, id2) %>%
    dplyr::arrange(id1, id2) %>%
    dplyr::group_by(id1) %>%
    tidyr::nest(id2_l = c(id2)) %>%
    dplyr::group_by(id2_l) %>%
    tidyr::nest(id1_l = c(id1))

  sim_df <-
    x_map2_dfr(
      index_nest$id1_l,
      index_nest$id2_l,
      sparse_pairwise_helper,
      X,
      pairwise_function
    ) %>%
    dplyr::arrange(id1, id2)

  sim_df
}


#' Help compute similarity between pairs of rows of a matrix
#'
#' \code{sparse_pairwise} helps computes similarity between pairs of rows of a
#'   matrix.
#'
#' @param X matrix
#' @param idl1 list of integer vectors specifying the list of rows of \code{X}
#'   (first set)
#' @param idl2 list of integer vectors specifying the list of rows of \code{X},
#'   (second set), same length as \code{idl1}.
#' @param pairwise_function function that takes a matrix and a pair of indices
#'   specifying rows of the matrix, and computes an operation of each pair of
#'   rows
#'
#' @return data.frame with the same number of rows as the length of \code{idl1}
#'   (and \code{id2}) containing the similarity between the pairs of rows
#'   of \code{X}.
#'   \code{sim[i] == pairwise_function(X[idl1[i], ], X[idl2[i], ])} where
#'   idl1[i] and idl2[i] are each *lists* of indices.
sparse_pairwise_helper <- function(idl1, idl2, X, pairwise_function) {
  index_sub <-
    expand.grid(
      id1 = idl1[[1]],
      id2 = idl2[[1]],
      KEEP.OUT.ATTRS = FALSE
    )

  S <-
    as.vector(pairwise_function(X, idl1[[1]], idl2[[1]]))

  index_sub <- index_sub %>% dplyr::mutate(sim = S)

  index_sub
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

#' Compute similarity between pairs of rows of a matrix
#'
#' \code{cosine_sparse} computes cosine similarity between pairs of rows of a
#'   matrix.
#' \code{pearson_sparse} computes pearson similarity between pairs of rows of a
#'   matrix.
#'
#' @param X matrix
#' @param id1 vector of integers specifying the list of rows of \code{X}
#'   (first set)
#' @param id2 vector of integers specifying the list of rows of \code{X},
#' (second set), same length as \code{id1}.
#' @param ... arguments passed downstream for parallel processing.
#'
#' @return data.frame with the same number of rows as the length of \code{id1}
#'   (and \code{id2}) containing the similarity between the pairs of rows
#'   of \code{X}. \code{sim[i] == similarity(X[id1[i], ], X[id2[i], ])}.
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
#'
#' Xm <- X - rowMeans(X)
#' s3 <- matric::cosine_sparse(Xm, id1, id2) %>% dplyr::arrange(id1, id2)
#' s4 <- matric::pearson_sparse(X, id1, id2) %>% dplyr::arrange(id1, id2)
#'
#' all.equal(s3, s4)
#' @name sparse_similarity
NULL

#' @export
#' @rdname sparse_similarity
cosine_sparse <- function(X, id1, id2, ...) {
  X <- X / sqrt(rowSums(X * X))

  sparse_pairwise(X, id1, id2, tcrossprod_ij, ...)
}

#' @export
#' @rdname sparse_similarity
pearson_sparse <- function(X, id1, id2, ...) {
  X <- X - rowMeans(X)

  cosine_sparse(X, id1, id2, ...)
}
