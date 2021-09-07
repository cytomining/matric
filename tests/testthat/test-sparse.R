test_that("tcrossprod_ij works", {
  set.seed(42)

  X <- matrix(rnorm(5 * 3), 5, 3)

  id1 <- c(1, 3)
  id2 <- c(5, 4)

  (s1 <- matric::tcrossprod_ij(X, id1, id2))

  (s2 <- tcrossprod(X)[id1, id2])

  expect_equal(s1, s2)
})

test_that("cosine_sparse works", {
  set.seed(42)

  X <- matrix(rnorm(5 * 3), 5, 3)

  id1 <- c(1, 3)
  id2 <- c(5, 4)

  s1 <- matric::cosine_sparse(X, id1, id2) %>% dplyr::arrange(id1, id2)

  Xn <- X / sqrt(rowSums(X * X))

  n_rows <- nrow(Xn)

  s2 <-
    expand.grid(
      id1 = seq(n_rows),
      id2 = seq(n_rows),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
    dplyr::mutate(sim = as.vector(tcrossprod(Xn))) %>%
    dplyr::inner_join(s1 %>% dplyr::select(id1, id2), by = c("id1", "id2")) %>%
    dplyr::arrange(id1, id2)

  s1

  expect_equal(s1, s2)
})
