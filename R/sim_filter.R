utils::globalVariables(c("all_same_col"))

#' Filter a melted similarity matrix to remove or keep specified rows.
#'
#' \code{sim_filter_keep_or_drop_some} filters a melted similarity matrix to
#' remove or keep specified rows.
#'
#' @param sim_df data.frame with melted similarity matrix.
#'
#' @param row_metadata data.frame with row metadata.
#'
#' @param filter_keep optional data.frame of metadata specifying which
#' rows to keep.
#'
#' @param filter_drop optional data.frame of metadata specifying which
#' rows to drop.
#'
#' @param filter_side character string specifying which index to filter on.
#' This must be one of the strings \code{"left"} or \code{"right"}.
#'
#' @return Filtered \code{sim_df} as a data.frame, with some rows kept and
#' some rows dropped. No filters applied if both \code{filter_keep} and
#' \code{filter_drop} are NULL.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 4, replace = TRUE),
#'   Metadata_type = sample(c("x", "y"), 4, replace = TRUE),
#'   x = rnorm(4),
#'   y = x + rnorm(4) / 100,
#'   z = y + rnorm(4) / 1000
#' )
#' annotation_cols <- c("Metadata_group", "Metadata_type")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' row_metadata <- attr(sim_df, "row_metadata")
#' sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
#' filter_keep <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' filter_drop <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' matric::sim_filter_keep_or_drop_some(sim_df, row_metadata,
#'  filter_keep = filter_keep, filter_side = "left")
#' matric::sim_filter_keep_or_drop_some(sim_df, row_metadata,
#'  filter_drop = filter_drop, filter_side = "left")
#' @export
sim_filter_keep_or_drop_some <-
  function(sim_df,
           row_metadata,
           filter_keep = NULL,
           filter_drop = NULL,
           filter_side = NULL) {
    sim_df %<>% as.data.frame()

    stopifnot(!is.null(filter_side))

    stopifnot(filter_side %in% c("left", "right"))

    # if there's nothing to keep and nothing to drop, then assume there is
    # nothing to drop
    if (is.null(filter_drop) & is.null(filter_keep)) {
      return(sim_df)
    }

    join_str <- c("id")

    # join_str will be either c("id1" = "id") or c("id2" = "id")
    names(join_str) <-
      paste0("id", ifelse(filter_side == "left", 1, 2))

    if (!is.null(filter_keep)) {
      filter_ids <-
        row_metadata %>%
        dplyr::inner_join(filter_keep, by = colnames(filter_keep)) %>%
        dplyr::select(id)

      sim_df %<>%
        dplyr::inner_join(filter_ids, by = join_str)
    }

    if (!is.null(filter_drop)) {
      filter_ids <-
        row_metadata %>%
        dplyr::inner_join(filter_drop, by = colnames(filter_drop)) %>%
        dplyr::select(id)

      sim_df %<>%
        dplyr::anti_join(filter_ids, by = join_str)
    }

    sim_df
  }

#' Filter a melted similarity matrix to keep pairs with the same
#' values in specific columns.
#'
#' \code{sim_filter_all_same} filters a melted similarity matrix to keep pairs with the
#' same values in specific columns.
#'
#' @param sim_df data.frame with melted similarity matrix.
#'
#' @param row_metadata data.frame with row metadata.
#'
#' @param all_same_cols character vector specifying columns.
#'
#' @param annotation_cols optional character vector specifying which columns
#' from \code{metadata} to annotate the left index of the filtered
#' \code{sim_df} with.
#'
#' @param include_group_tag optional boolean specifying whether to include an
#' identifier for the pairs using the values in the \code{all_same_cols}
#' columns.
#'
#' @param drop_lower optional boolean specifying whether to drop the pairs
#' where the first index is smaller than the second index. This is equivalent
#' to dropping the lower triangular of  \code{sim_df}.
#'
#' @return Filtered \code{sim_df} as a data.frame, where only pairs with the
#' same values in \code{all_same_cols} columns are kept. Rows are annotated
#' based on the first index, if specified.
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
#' row_metadata <- attr(sim_df, "row_metadata")
#' sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
#' all_same_cols <- c("Metadata_group")
#' include_group_tag <- TRUE
#' drop_lower <- FALSE
#' matric::sim_filter_all_same(
#'   sim_df,
#'   row_metadata,
#'   all_same_cols,
#'   annotation_cols,
#'   include_group_tag,
#'   drop_lower
#' )
#' @export
sim_filter_all_same <-
  function(sim_df,
           row_metadata,
           all_same_cols,
           annotation_cols = NULL,
           include_group_tag = FALSE,
           drop_lower = FALSE) {
    sim_df %<>% as.data.frame()

    metadata_i <-
      row_metadata %>%
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
        sim_annotate(row_metadata, annotation_cols,
          index = "left"
        )
    }

    sim_df
  }

#' Filter a melted similarity matrix to keep pairs with the same values in
#' specific columns, and keep only some of these pairs.
#'
#' \code{sim_filter_all_same} filters a melted similarity matrix to keep pairs with
#' the same values in specific columns, keeping only some of these pairs.
#'
#' @param sim_df data.frame with melted similarity matrix.
#'
#' @param row_metadata data.frame with row metadata.
#'
#' @param all_same_cols character vector specifying columns.
#'
#' @param filter_keep_right data.frame of metadata specifying which rows to
#' keep on the right index.
#'
#' @param annotation_cols optional character vector specifying which columns
#' from \code{metadata} to annotate the left index of the filtered
#' \code{sim_df} with.
#'
#' @param drop_reference optional boolean specifying whether to filter (drop)
#' pairs using \code{filter_keep_right} on the left index.
#'
#' @param sim_cols optional character string specifying minimal set of columns
#' for a similarity matrix
#'
#' @return Filtered \code{sim_df} as a data.frame, where only pairs with the
#' same values in \code{all_same_cols} columns are kept, with further filtering
#' using \code{filter_keep_right}.Rows are annotated based on the first index,
#' if specified.
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
#' row_metadata <- attr(sim_df, "row_metadata")
#' sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
#' all_same_cols <- c("Metadata_group")
#' filter_keep_right <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' drop_reference <- FALSE
#' matric::sim_filter_all_same_keep_some(
#'   sim_df,
#'   row_metadata,
#'   all_same_cols,
#'   filter_keep_right,
#'   annotation_cols,
#'   drop_reference
#' )
#' @export
sim_filter_all_same_keep_some <-
  function(sim_df,
           row_metadata,
           all_same_cols,
           filter_keep_right,
           annotation_cols = NULL,
           drop_reference = TRUE,
           sim_cols = c("id1", "id2", "sim")) {
    sim_df %<>% as.data.frame()

    sim_df %<>%
      sim_filter_all_same(row_metadata, all_same_cols) %>%
      sim_filter_keep_or_drop_some(
        row_metadata,
        filter_keep = filter_keep_right,
        filter_side = "right"
      )

    if (drop_reference) {
      filter_drop_left <- filter_keep_right

      sim_df %<>%
        sim_filter_keep_or_drop_some(
          row_metadata,
          filter_drop = filter_drop_left,
          filter_side = "left"
        )
    }

    if (!is.null(annotation_cols)) {
      sim_df %<>%
        dplyr::select(dplyr::all_of(sim_cols)) %>%
        sim_annotate(row_metadata, annotation_cols,
          index = "left"
        )
    }

    sim_df
  }

#' Filter a melted similarity matrix to keep pairs with the same
#' values in specific columns, and other constraints.
#'
#' \code{sim_filter_some_different_drop_some} filters a melted similarity matrix to
#' keep pairs with the same values in specific columns, and other constraints.
#'
#' @param sim_df data.frame with melted similarity matrix.
#'
#' @param row_metadata data.frame with row metadata.
#'
#' @param any_different_cols character vector specifying columns.
#'
#' @param all_same_cols optional character vector specifying columns.
#'
#' @param all_different_cols optional character vector specifying columns.
#'
#' @param filter_drop_left data.frame of metadata specifying which rows to
#' drop on the left index.
#'
#' @param filter_drop_right data.frame of metadata specifying which rows to
#' drop on the right index.
#'
#' @param annotation_cols optional character vector specifying which columns
#' from \code{metadata} to annotate the left index of the filtered
#' \code{sim_df} with.
#'
#' @return Filtered \code{sim_df} as a data.frame, keeping only pairs that have
#'   - same values in all columns of \code{all_same_cols},
#'   - different values in all columns \code{all_different_cols}, and
#'   - different values in at least one column of \code{any_different_cols},
#'
#' with further filtering using \code{filter_drop_left} and
#' \code{filter_drop_right}. Rows are annotated based on the first index,
#' if specified.
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
#' row_metadata <- attr(sim_df, "row_metadata")
#' sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
#' all_same_cols <- c("Metadata_group")
#' all_different_cols <- c("Metadata_type1")
#' any_different_cols <- c("Metadata_type2")
#' filter_drop_left <- tibble::tibble(Metadata_group = "a", Metadata_type1 = "x")
#' filter_drop_right <- tibble::tibble(Metadata_group = "a", Metadata_type1 = "x")
#' drop_reference <- FALSE
#' matric::sim_filter_some_different_drop_some(
#'   sim_df,
#'   row_metadata,
#'   any_different_cols,
#'   all_same_cols,
#'   all_different_cols,
#'   filter_drop_left,
#'   filter_drop_right,
#'   annotation_cols
#' )
#' @export
sim_filter_some_different_drop_some <-
  function(sim_df,
           row_metadata,
           any_different_cols,
           all_same_cols = NULL,
           all_different_cols = NULL,
           filter_drop_left = NULL,
           filter_drop_right = NULL,
           annotation_cols = NULL) {
    sim_df %<>% as.data.frame()

    stopifnot(!any(all_same_cols %in% all_different_cols))

    metadata_i <- row_metadata

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
        sim_annotate(row_metadata, annotation_cols,
          index = "left"
        )
    }

    sim_df
  }
