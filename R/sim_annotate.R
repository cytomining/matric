utils::globalVariables(c("id"))
#' Annotate melted similarity matrix.
#'
#' \code{sim_annotate} annotates a melted similarity matrix.
#'
#' @param sim_df data.frame with melted similarity matrix.
#' @param row_metadata data.frame with row metadata.
#' @param annotation_cols character vector specifying annotation columns.
#' @param index optional character string specifying whether to annotate left index, right index, or both.  This must be one of the strings \code{"both"} (default), \code{"left"}, \code{"right"}.
#' @param sim_cols optional character string specifying minimal set of columns for a similarity matrix
#'
#' @return annotated melted similarity matrix of the same class as \code{sim_df}.
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
#' annotation_cols <- c("Metadata_group")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' row_metadata <- attr(sim_df, "row_metadata")
#' matric::sim_annotate(sim_df, row_metadata, annotation_cols)
#' @export
sim_annotate <-
  function(sim_df,
           row_metadata,
           annotation_cols,
           index = "both",
           sim_cols = c("id1", "id2", "sim")) {
    sim_df %<>% as.data.frame()

    metadata_i <-
      row_metadata %>%
      dplyr::select(id, dplyr::any_of(annotation_cols))

    sim_df %<>% dplyr::select(dplyr::all_of(sim_cols))

    if (index == "left") {
      sim_df %<>%
        dplyr::inner_join(metadata_i,
          by = c("id1" = "id"),
          suffix = c("1", "2")
        )
    }

    if (index == "right") {
      sim_df %<>%
        dplyr::inner_join(metadata_i,
          by = c("id2" = "id"),
          suffix = c("1", "2")
        )
    }

    if (index == "both") {
      sim_df %<>%
        dplyr::inner_join(metadata_i,
          by = c("id1" = "id")
        ) %>%
        dplyr::inner_join(metadata_i,
          by = c("id2" = "id"),
          suffix = c("1", "2")
        )
    }

    sim_df
  }
