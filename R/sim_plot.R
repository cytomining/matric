utils::globalVariables(c(".data", "sim", "sim_mean", "sim_rank", "sim_sd"))
#' Plot similarity matrix.
#'
#' \code{sim_plot} Plots similarity matrix.
#'
#' @param sim_df tbl with melted similarity matrix.
#' @param annotation_column character string specifying the column in \code{sim_df} to use to annotate rows and columns.
#' @param calculate_sim_rank boolean specifying whether to calculate rank of similarity.
#' @param trim_label optional integer specifying the trim length for tick labels.
#'
#' @return \code{ggplot} object of the plot.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b", "c", "d"), 100, replace = TRUE),
#'   x1 = rnorm(100),
#'   x2 = rnorm(100),
#'   x3 = rnorm(100),
#'   x4 = rnorm(100),
#'   x5 = rnorm(100)
#' )
#' annotation_cols <- c("Metadata_group", "Metadata_type")
#' sim_df <- matric::sim_calculate(population, method = "pearson")
#' sim_df <- matric::sim_annotate(sim_df, annotation_cols)
#' annotation_column <- "Metadata_group"
#' matric::sim_plot(sim_df, annotation_column, calculate_sim_rank = TRUE)
#' @export
sim_plot <-
  function(sim_df,
           annotation_column,
           calculate_sim_rank = FALSE,
           trim_label = NULL) {
    invisible(validate_sim(sim_df))

    col1 <- paste0(annotation_column, "1")
    col2 <- paste0(annotation_column, "2")
    col1_short <- paste0(annotation_column, "1")
    col2_short <- paste0(annotation_column, "2")
    col1_short_sym <- rlang::sym(col1_short)
    col2_short_sym <- rlang::sym(col2_short)

    if (!is.null(trim_label)) {
      sim_df[[col1_short]] <- stringr::str_sub(sim_df[[col1]], 1, trim_label)
      sim_df[[col2_short]] <-
        stringr::str_sub(sim_df[[col2]], 1, trim_label)
    } else {
      col1_short <- col1
      col2_short <- col2
    }

    sim_df %<>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(
        col1_short, col2_short
      )))) %>%
      dplyr::summarise(dplyr::across(dplyr::any_of(c("sim", "sim_rank")), mean),
        .groups = "keep"
      )

    if (calculate_sim_rank) {
      sim_df %<>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(col1_short)))) %>%
        dplyr::mutate(sim_rank = rank(-sim) / length(sim))
    } else {
      stopifnot("sim_rank" %in% names(sim_df))
    }

    p <- sim_df %>%
      ggplot2::ggplot(ggplot2::aes(
        .data[[col1_short_sym]],
        .data[[col2_short_sym]],
        fill = sim_rank,
        label = sprintf("%d%%\n(%.2f)", as.integer(sim_rank * 100), sim)
      )) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(color = "white", size = 3) +
      ggplot2::coord_equal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )) +
      ggplot2::scale_fill_continuous(limits = c(0, 1))

    p
  }
