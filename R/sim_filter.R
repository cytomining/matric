#' Filter rows of the melted similarity matrix.
#'
#' \code{sim_filter} filters rows of the melted similarity matrix.
#'
#' @param sim_df tbl with melted similarity matrix.
#' @param row_metadata tbl with row metadata.
#' @param filter_keep optional tbl of metadata specifying which rows to keep.
#' @param filter_drop optional tbl of metadata specifying which rows to drop.
#' @param filter_side character string specifying which index to filter on. This must be one of the strings \code{"left"} or \code{"right"}.
#'
#' @return filtered \code{sim_df} with some rows kept and some rows dropped. No filters applied if both \code{filter_keep} and \code{filter_drop} are NULL.
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
#' sim_df <- matric::sim_annotate(sim_df, annotation_cols)
#' filter_keep <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' filter_drop <- tibble::tibble(Metadata_group = "a", Metadata_type = "x")
#' matric::sim_filter(sim_df, filter_keep = filter_keep, filter_side = "left")
#' matric::sim_filter(sim_df, filter_drop = filter_drop, filter_side = "left")
#' @export
sim_filter <-
  function(sim_df,
           row_metadata,
           filter_keep = NULL,
           filter_drop = NULL,
           filter_side = NULL) {
    sim_df %<>% as.data.frame(sim_df)

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
