utils::globalVariables(c("all_same_col"))
#' Filter rows of the melted similarity matrix to keep pairs with the same values in specific columns.
#'
#' \code{sim_all_same} Filters melted similarity matrix to keep pairs with the same values in specific columns.
#'
#' @param sim_df tbl with melted similarity matrix.
#' @param all_same_cols character vector specifying columns.
#' @param annotation_cols optional character vector specifying which columns from \code{metadata} to annotate the left index of the filtered \code{sim_df} with.
#' @param include_group_tag optional boolean specifying whether to include an identifier for the pairs using the values in the \code{all_same_cols} columns.
#' @param drop_lower optional boolean specifying whether to drop the pairs where the first index is smaller than the second index. This is equivalent to dropping the lower triangular of  \code{sim_df}.
#'
#' @return filtered \code{sim_df} where only pairs with the same values in \code{all_same_cols} columns are kept.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' n <- 5
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), n, replace = TRUE),
#'   Metadata_type = sample(c("x", "y"), n, replace = TRUE),
#'   x = rnorm(n),
#'   y = x + rnorm(n) / 100,
#'   z = y + rnorm(n) / 1000
#' )
#' annotation_cols <- c("Metadata_group", "Metadata_type")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' sim_df <- matric::sim_annotate(sim_df, annotation_cols)
#' all_same_cols <- c("Metadata_group")
#' include_group_tag <- TRUE
#' drop_lower <- FALSE
#' matric::sim_all_same(sim_df, all_same_cols, annotation_cols, include_group_tag, drop_lower)
#' @export
sim_all_same <-
  function(sim_df,
           all_same_cols,
           annotation_cols = NULL,
           include_group_tag = FALSE,
           drop_lower = FALSE) {
    metadata <- attr(sim_df, "row_metadata")

    stopifnot(!is.null(metadata))

    metadata_i <-
      metadata %>%
      dplyr::select(id, dplyr::all_of(all_same_cols)) %>%
      tidyr::unite("all_same_col", dplyr::all_of(all_same_cols), sep = ":")

    ids <-
      dplyr::inner_join(metadata_i,
        metadata_i,
        by = "all_same_col",
        suffix = c("1", "2")
      )

    if (include_group_tag) {
      ids %<>% dplyr::select(id1, id2, group = all_same_col)
    } else {
      ids %<>% dplyr::select(id1, id2)
    }

    if (drop_lower) {
      sim_df %<>% dplyr::filter(id1 > id2)
    }

    sim_df %<>%
      dplyr::inner_join(ids, by = c("id1", "id2"))

    if (!is.null(annotation_cols)) {
      sim_df %<>%
        sim_annotate(annotation_cols,
          index = "left"
        )
    }

    sim_df
  }

#' Filter rows of the melted similarity matrix to keep pairs with the same values in specific columns, and keep only some of these pairs.
#'
#' \code{sim_all_same} Filters melted similarity matrix to keep pairs with the same values in specific columns, keeping only some of these pairs.
#'
#' @param sim_df tbl with melted similarity matrix.
#' @param all_same_cols character vector specifying columns.
#' @param filter_keep_right tbl of metadata specifying which rows to keep on the right index.
#' @param annotation_cols optional character vector specifying which columns from \code{metadata} to annotate the left index of the filtered \code{sim_df} with.
#' @param drop_reference optional boolean specifying whether to filter (drop) pairs using \code{filter_keep_right} on the left index.
#' @param sim_cols optional character string specifying minimal set of columns for a similarity matrix
#'
#' @return filtered \code{sim_df} where only pairs with the same values in \code{all_same_cols} columns are kept, with further filtering using \code{filter_keep_right}.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' n <- 20
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), n, replace = TRUE),
#'   Metadata_type = sample(c("x", "y"), n, replace = TRUE),
#'   x = rnorm(n),
#'   y = x + rnorm(n) / 100,
#'   z = y + rnorm(n) / 1000
#' )
#' annotation_cols <- c("Metadata_group", "Metadata_type")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' sim_df <- matric::sim_annotate(sim_df, annotation_cols)
#' all_same_cols <- c("Metadata_group")
#' filter_keep_right <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' drop_reference <- FALSE
#' matric::sim_all_same_keep_some(
#'   sim_df,
#'   all_same_cols,
#'   filter_keep_right,
#'   annotation_cols,
#'   drop_reference
#' )
#' @export
sim_all_same_keep_some <-
  function(sim_df,
           all_same_cols,
           filter_keep_right,
           annotation_cols = NULL,
           drop_reference = TRUE,
           sim_cols = c("id1", "id2", "sim")) {
    metadata <- attr(sim_df, "row_metadata")

    stopifnot(!is.null(metadata))

    sim_df %<>%
      sim_all_same(all_same_cols) %>%
      sim_filter(
        filter_keep = filter_keep_right,
        filter_side = "right"
      )

    if (drop_reference) {
      filter_drop_left <- filter_keep_right

      sim_df %<>%
        sim_filter(
          filter_drop = filter_drop_left,
          filter_side = "left"
        )
    }

    if (!is.null(annotation_cols)) {
      sim_df %<>%
        dplyr::select(dplyr::all_of(sim_cols)) %>%
        sim_annotate(annotation_cols,
          index = "left"
        )
    }

    sim_df
  }

#' Filter rows of the melted similarity matrix to keep pairs with the same values in specific columns, and keep only some of these pairs.
#'
#' \code{sim_some_different_drop_some} Filters melted similarity matrix to keep pairs with the same values in specific columns, keeping only some of these pairs.
#'
#' @param sim_df tbl with melted similarity matrix.
#' @param any_different_cols character vector specifying columns.
#' @param all_same_cols optional character vector specifying columns.
#' @param all_different_cols optional character vector specifying columns.
#' @param filter_drop_left tbl of metadata specifying which rows to drop on the left index.
#' @param filter_drop_right tbl of metadata specifying which rows to drop on the right index.
#' @param annotation_cols optional character vector specifying which columns from \code{metadata} to annotate the left index of the filtered \code{sim_df} with.
#'
#' @return filtered \code{sim_df} keeping only pairs that have same values in all columns of \code{all_same_cols_non_rep}, different values in all columns \code{all_different_cols_non_rep}, and different values in at least one column of \code{any_different_cols_non_rep}, with further filtering using \code{filter_drop_left} and \code{filter_drop_right}.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
#'   Metadata_type1 = sample(c("x", "y"), 4, replace = TRUE),
#'   Metadata_type2 = sample(c("p", "q"), 4, replace = TRUE),
#'   x = rnorm(4),
#'   y = x + rnorm(4) / 100,
#'   z = y + rnorm(4) / 1000
#' )
#' annotation_cols <- c("Metadata_group", "Metadata_type")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' sim_df <- matric::sim_annotate(sim_df, annotation_cols)
#' all_same_cols <- c("Metadata_group")
#' all_different_cols <- c("Metadata_type1")
#' any_different_cols <- c("Metadata_type2")
#' filter_drop_left <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' filter_drop_right <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' drop_reference <- FALSE
#' matric::sim_some_different_drop_some(
#'   sim_df,
#'   any_different_cols,
#'   all_same_cols,
#'   all_different_cols,
#'   filter_drop_left,
#'   filter_drop_right,
#'   annotation_cols
#' )
#' @export
sim_some_different_drop_some <-
  function(sim_df,
           any_different_cols,
           all_same_cols = NULL,
           all_different_cols = NULL,
           filter_drop_left = NULL,
           filter_drop_right = NULL,
           annotation_cols = NULL) {
    metadata <- attr(sim_df, "row_metadata")

    stopifnot(!is.null(metadata))

    stopifnot(!any(all_same_cols %in% all_different_cols))

    metadata_i <- metadata

    if (is.null(all_same_cols)) {
      # create a dummy column on which to join
      metadata_i %<>% dplyr::mutate(all_same_col = 0)
      all_same_cols <- "all_same_col"
    } else {
      # create a unified column on which to join
      metadata_i %<>%
        tidyr::unite(
          "all_same_col",
          dplyr::all_of(all_same_cols),
          sep = ":",
          remove = FALSE
        )
    }

    # ignore any_different_cols if superseded by all_different_cols
    if (any(all_different_cols %in% any_different_cols)) {
      any_different_cols <- NULL
    }

    # remove from any_different_cols its intersection with all_same_cols
    any_different_cols <- setdiff(any_different_cols, all_same_cols)

    # create a unified column for any_different_cols and include that new column
    # in all_different_cols
    if (!is.null(any_different_cols)) {
      metadata_i %<>%
        tidyr::unite(
          "any_different_col",
          dplyr::all_of(any_different_cols),
          sep = ":",
          remove = FALSE
        )

      all_different_cols <-
        c(all_different_cols, "any_different_col")
    }

    # create left and right metadata
    f_metadata_filter <-
      function(filter_drop) {
        if (is.null(filter_drop)) {
          metadata_i %>%
            dplyr::select(id, all_same_col)
        } else {
          metadata_i %>%
            dplyr::anti_join(filter_drop, by = colnames(filter_drop)) %>%
            dplyr::select(id, all_same_col)
        }
      }

    metadata_left <- f_metadata_filter(filter_drop_left)
    metadata_right <- f_metadata_filter(filter_drop_right)

    # list of rows that should be the same (weak constraint)
    ids_all_same <-
      dplyr::inner_join(metadata_left,
        metadata_right,
        by = "all_same_col",
        suffix = c("1", "2")
      )

    # list of rows that should be the different (strong constraint)
    ids_all_different <-
      purrr::map_df(
        all_different_cols,
        function(all_different_col) {
          dplyr::inner_join(
            metadata_i %>% dplyr::select(id, dplyr::all_of(all_different_col)),
            metadata_i %>% dplyr::select(id, dplyr::all_of(all_different_col)),
            by = all_different_col,
            suffix = c("1", "2")
          ) %>%
            dplyr::select(id1, id2)
        }
      ) %>%
      dplyr::distinct()

    # impose strong constraint on weak constraint
    ids <-
      dplyr::anti_join(ids_all_same,
        ids_all_different,
        by = c("id1", "id2")
      )

    ids %<>% dplyr::select(id1, id2)

    # filter similarity matrix
    sim_df %<>%
      dplyr::inner_join(ids, by = c("id1", "id2"))

    # add annotations
    if (!is.null(annotation_cols)) {
      sim_df %<>%
        sim_annotate(annotation_cols,
          index = "left"
        )
    }

    sim_df
  }
