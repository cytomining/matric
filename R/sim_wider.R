#' Widen a symmetric similarity matrix.
#'
#' \code{sim_plot} Plots similarity matrix.
#'
#' @param sim_df data.frame with melted similarity matrix.
#' @param primary_key_column character string specifying the column in \code{sim_df} to use to uniquely identify rows and columns
#' @param annotation_column character string specifying the column in \code{sim_df} to use to annotate rows and columns
#'
#' @return data.frame of widened similarity matrix, with some attributes.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom dplyr across all_of everything
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b", "c", "d"), 100, replace = TRUE),
#'   x1 <- rnorm(100),
#'   x2 <- rnorm(100),
#'   x3 <- rnorm(100),
#'   x4 <- rnorm(100),
#'   x5 <- rnorm(100)
#' )
#' population$Metadata_id <- seq(nrow(population))
#' metadata <- matric::get_annotation(population)
#' annotation_cols <- c("Metadata_group", "Metadata_id")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' row_metadata <- attr(sim_df, "row_metadata")
#' sim_df <- matric::sim_annotate(sim_df, row_metadata, annotation_cols)
#' annotation_column <- "Metadata_group"
#' primary_key_column <- "Metadata_id"
#' res <- matric::sim_wider(sim_df, annotation_column, primary_key_column)
#' res
#' data.frame(id = rownames(res)) %>% dplyr::inner_join(attr(res, "map"))
#' @export
sim_wider <-
  function(sim_df,
           annotation_column,
           primary_key_column) {
    primary_key_column1 <- paste0(primary_key_column, "1")
    primary_key_column2 <- paste0(primary_key_column, "2")
    primary_key_columns <-
      c(primary_key_column1, primary_key_column2)

    sim_df %<>% as.data.frame()

    annotation_column1 <- paste0(annotation_column, "1")
    annotation_column2 <- paste0(annotation_column, "2")
    annotation_columns <- c(annotation_column1, annotation_column2)

    annotation_column_unique1 <-
      paste(annotation_column1, "uniq", sep = "_")
    annotation_column_unique2 <-
      paste(annotation_column2, "uniq", sep = "_")

    sim_df_wider <-
      sim_df %>%
      dplyr::select(all_of(c(primary_key_columns, "sim"))) %>%
      dplyr::arrange(across(all_of(primary_key_columns))) %>%
      tidyr::pivot_wider(
        names_from = all_of(primary_key_column2),
        values_from = "sim"
      ) %>%
      tibble::column_to_rownames(primary_key_column1)

    # assumes symmetric matrix
    sim_df_wider %<>%
      dplyr::select(all_of(row.names(sim_df_wider)))

    stopifnot(colnames(sim_df_wider) == row.names(sim_df_wider))

    map1 <-
      sim_df %>%
      dplyr::select(all_of(c(
        primary_key_column1, annotation_column1
      ))) %>%
      dplyr::distinct() %>%
      dplyr::arrange(across(all_of(
        c(primary_key_column1, annotation_column1)
      )))
    map2 <-
      sim_df %>%
      dplyr::select(all_of(c(
        primary_key_column2, annotation_column2
      ))) %>%
      dplyr::distinct() %>%
      dplyr::arrange(across(all_of(
        c(primary_key_column2, annotation_column2)
      )))

    stopifnot(all(map1 == map2))

    map1[[annotation_column_unique1]] <-
      paste(map1[[annotation_column1]],
        seq_along(map1[[annotation_column1]]),
        sep = ":"
      )

    map1[[primary_key_column1]] <-
      as.character(map1[[primary_key_column1]])

    key1 <- data.frame(x = as.character(row.names(sim_df_wider)))
    names(key1) <- primary_key_column1

    value1 <-
      key1 %>%
      dplyr::inner_join(map1, by = primary_key_column1) %>%
      dplyr::pull(all_of(annotation_column1))

    value_unique1 <-
      key1 %>%
      dplyr::inner_join(map1, by = primary_key_column1) %>%
      dplyr::pull(all_of(annotation_column_unique1))

    row.names(sim_df_wider) <- value_unique1
    colnames(sim_df_wider) <- row.names(sim_df_wider)

    map1 %<>% dplyr::select(
      id = all_of(annotation_column_unique1),
      annotation = all_of(annotation_column1),
      primary_key = all_of(primary_key_column1)
    )

    attr(sim_df_wider, "map") <- map1

    sim_df_wider
  }
