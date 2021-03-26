#' Constructor for \code{sim} S3 class.
#'
#' \code{sim_new} creates an object of class \code{sim}.
#'
#' \code{sim} is just a \code{data.frame} with two attributes and at least
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
#' @param x tbl with similarity matrix.
#' @param row_metadata tbl with row metadata.
#' @param metric_metadata list with metric information
#'
#' @return object of class \code{sim}
sim_new <- function(x, row_metadata, metric_metadata) {
  stopifnot(is.data.frame(x))

  stopifnot(!is.null(row_metadata))

  stopifnot(is.data.frame(row_metadata))

  stopifnot(!is.null(metric_metadata))

  stopifnot(is.list(metric_metadata))

  structure(
    x,
    row_metadata = row_metadata,
    metric_metadata = metric_metadata,
    class = unique(c("sim", class(x))) # this might not be the right way to do it!
  )
}

#' Validator for \code{sim} S3 class.
#'
#' \code{sim_validate} validates that an object is of class class \code{sim}.
#'
#' @param x object.
#'
#' @return object of class \code{sim} if \code{x} is a valid object of that class
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

#' Preserver for \code{sim} S3 class.
#'
#' \code{sim_preserve} preserves the attributes of class \code{sim}.
#'
#' This is a workaround until we figure out S3 inheritance better
#' https://adv-r.hadley.nz/s3.html#inheritance.
#'
#' https://cran.r-project.org/web/packages/sticky/vignettes/introduction.html
#' likely attempts something similar.
#'
#'
#' @param x object to preserve.
#' @param x_attributes list of attributes of class \code{sim}.
#'
#' @return object of class \code{sim} if \code{x} is a valid object of that class
#'
#' @export
sim_preserve <- function(x, x_attributes) {
  mostattributes(x) <- x_attributes

  sim_validate(x)
}
