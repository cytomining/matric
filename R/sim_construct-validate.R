#' Constructor for \code{matric_sim} S3 class.
#'
#' \code{sim_new} creates an object of class \code{matric_sim}.
#'
#' \code{matric_sim} is just a \code{data.frame} with two attributes and at least
#' three columns.
#'
#' Columns:
#' - \code{id1} and \code{id2}: integers, indicating row ids.
#' - \code{sim} similarity: double, indicating similarity between the rows.
#'
#' Attributes:
#' - \code{row_metadata}: data.frame of row annotations, with `id` column.
#' - \code{metric_metadata}: information about the similarity metric.
#'
#' This is somewhat similar to \code{Biobase::AnnotatedDataFrame}.
#'
#' @param x tbl with similarity matrix.
#' @param row_metadata tbl with row metadata.
#' @param metric_metadata list with metric information
#'
#' @return object of class \code{matric_sim}
#' @export
sim_new <- function(x, row_metadata, metric_metadata) {
  stopifnot(is.data.frame(x))

  stopifnot(!is.null(row_metadata))

  stopifnot(is.data.frame(row_metadata))

  stopifnot(!is.null(metric_metadata))

  stopifnot(is.list(metric_metadata))

  tibble::new_tibble(
    x,
    row_metadata = row_metadata,
    metric_metadata = metric_metadata,
    nrow = nrow(x),
    class = "matric_sim"
  )
}

#' Validator for \code{matric_sim} S3 class.
#'
#' \code{sim_validate} validates that an object is of class class \code{matric_sim}.
#'
#' @param x object.
#'
#' @return object of class \code{matric_sim} if \code{x} is a valid object of that class
#' @export
sim_validate <- function(x) {
  row_metadata <- attr(x, "row_metadata")

  metric_metadata <- attr(x, "metric_metadata")

  stopifnot(is.data.frame(x))

  stopifnot(!is.null(row_metadata))

  stopifnot(is.data.frame(row_metadata))

  stopifnot(!is.null(metric_metadata))

  stopifnot(is.list(metric_metadata))

  stopifnot(all(c("id1", "id2", "sim") %in% names(x)))

  stopifnot("id" %in% names(row_metadata))

  stopifnot(all(x$id1 %in% row_metadata$id))

  stopifnot(all(x$id2 %in% row_metadata$id))

  x
}

#' Restorer for \code{matric_sim} S3 class.
#'
#' \code{restore} restores the attributes of class \code{matric_sim}.
#'
#' This is a workaround until tibble inheritance improves
#'
#' https://github.com/tidyverse/tibble/issues/275
#' https://adv-r.hadley.nz/s3.html#inheritance.
#'
#' https://github.com/tidyverse/dplyr/issues/5480#issuecomment-682620522
#' "dplyr is not really ready for extension in this way"
#'
#' These are some of the \code{dplyr} verbs that will necessitate restoration:
#'
#' - \code{summarise}
#' - \code{group_by}
#'
#' There are likely more!
#'
#' @param x object to preserve.
#' @param x_attributes list of attributes of class \code{matric_sim}.
#'
#' @return object of class \code{matric_sim} if \code{x} is a valid object of that class
#'
#' @examples
#'
#' sim_df <-
#'   matric::sim_new(
#'     data.frame(id1 = 1, id2 = 2, sim = 1),
#'     data.frame(id = c(1, 2), Metadata_group = c("a", "b")),
#'     list(method = "pearson")
#'   )
#' sim_df_attr <- attributes(sim_df)
#' "matric_sim" %in% class(sim_df)
#' "matric_sim" %in% class(sim_df %>% dplyr::slice(1))
#' "matric_sim" %in%
#'   class(
#'     sim_df %>%
#'       dplyr::group_by(id1, id2) %>%
#'       dplyr::summarize(sim = mean(sim), .groups = "keep")
#'   )
#'
#' "matric_sim" %in%
#'   class(
#'     sim_df %>%
#'       dplyr::group_by(id1, id2) %>%
#'       dplyr::summarize(sim = mean(sim), .groups = "keep") %>%
#'       matric::sim_restore(sim_df_attr)
#'   )
#' @export
sim_restore <- function(x, x_attributes) {
  sim_validate(sim_new(x,
                       x_attributes$row_metadata,
                       x_attributes$metric_metadata))

}
