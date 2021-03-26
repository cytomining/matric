#' Constructor for \code{sim} S3 class.
#'
#' \code{new_sim} creates an object of class \code{sim}.
#'
#' @param x tbl with similarity matrix.
#' @param row_metadata tbl with row metadata.
#' @param metric_metadata list with metric information
#'
#' @return object of class \code{sim}
new_sim <- function(x, row_metadata, metric_metadata) {
  stopifnot(is.data.frame(x))

  stopifnot(!is.null(row_metadata))

  stopifnot(is.data.frame(row_metadata))

  stopifnot(!is.null(metric_metadata))

  stopifnot(is.list(metric_metadata))

  structure(
    x,
    row_metadata = row_metadata,
    metric_metadata = metric_metadata,
    class = c("sim", class(x)) # this might not be the right way to do it!
  )

}

#' Validator for \code{sim} S3 class.
#'
#' \code{validate_sim} validates that an object is of class class \code{sim}.
#'
#' @param x object.
#'
#' @return object of class \code{sim} if \code{x} is a valid object of that class
validate_sim <- function(x) {

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
