#' Collate several subsets of a melted simolarity matrix, required for computing
#' metrics.
#'
#' \code{sim_collate} collates several subsets of a melted simolarity matrix,
#' required for computing metrics.
#'
#' @details
#'
#'
#' ## 0. Filter out some rows
#'
#' Filter out pairs that match \code{drop_group} in either right or left indices
#'
#' ## 1. Similarity to reference
#'
#' Fetch similarities between
#'
#' - (a) all rows (except, optionally those containing \code{reference}), and
#' - (b) all rows containing \code{reference}
#'
#' Do so only for those (a, b) pairs that
#'
#' - have *same* values in *all* columns of \code{all_same_cols_ref}
#'
#' ## 2. Similarity to replicates (no references)
#'
#' Fetch similarities between
#' - (a) all rows except \code{reference} rows, and
#' - (b) all rows except \code{reference} rows (i.e. to each other)
#'
#' Do so for only those (a, b) pairs that
#' - have *same* values in *all* columns of \code{all_same_cols_rep}
#'
#' Keep, both, (a, b) and (b, a)
#'
#' ## 3. Similarity to replicates (only references)
#'
#' Fetch similarities between
#' - (a) all rows containing \code{reference}, and
#' - (b) all rows containing \code{reference} (i.e. to each other)
#'
#' Do so for only those (a, b) pairs that
#' - have *same* values in *all* columns of \code{all_same_cols_rep_ref}.
#'
#' Keep, both, (a, b) and (b, a)
#'
#' ## 4. Similarity to non-replicates
#'
#' Fetch similarities between
#' - (a) all rows (except, optionally, \code{reference} rows), and
#' - (b) all rows except \code{reference} rows
#'
#' Do so for only those (a, b) pairs that
#' - have *same* values in *all* columns of \code{all_same_cols_non_rep}
#' - have *different* values in *all* columns \code{all_different_cols_non_rep}
#' - have *different* values in *at least one* column of \code{any_different_cols_non_rep}
#'
#' Keep, both, (a, b) and (b, a)
#'
#' ## 5. Similarity to group
#'
#' Fetch similarities between
#' - (a) all rows (except, optionally, \code{reference} rows), and
#' - (b) all rows (except, optionally, \code{reference} rows)
#'
#' Do so for only those (a, b) pairs that
#' - have *same* values in *all* columns of \code{all_same_cols_group}
#' - have *different* values in *at least one* column of \code{any_different_cols_group}
#'
#' Keep, both, (a, b) and (b, a)
#'
#'
#' @param sim_df \code{metric_sim} object.
#' @param annotation_cols character vector specifying which columns from \code{metadata} to annotate the left index of the filtered \code{sim_df} with.
#' @param all_same_cols_rep optional character vector specifying columns.
#' @param all_same_cols_ref optional character vector specifying columns.
#' @param all_same_cols_rep_ref optional character vector specifying columns.
#' @param any_different_cols_non_rep optional character vector specifying columns.
#' @param all_same_cols_non_rep optional character vector specifying columns.
#' @param all_different_cols_non_rep optional character vector specifying columns.
#' @param any_different_cols_group optional character vector specifying columns.
#' @param all_same_cols_group optional character vector specifying columns.
#' @param reference optional character string specifying reference.
#' @param drop_reference optional boolean specifying whether to filter (drop) pairs using \code{reference} on the left index.
#' @param drop_group optional tbl; rows that match on \code{drop_group} on the left or right index are dropped.
#'
#' @return \code{metric_sim} object comprising a filtered \code{sim_df} with sets of pairs, preserving the same \code{metric_sim} attributes as \code{sim_df}.
#'
#' @examples
#'
#' sim_df <- matric::sim_calculate(matric::cellhealth)
#'
#' drop_group <-
#'   data.frame(Metadata_gene_name = "EMPTY")
#'
#' reference <-
#'   data.frame(Metadata_gene_name = c("Chr2"))
#'
#' all_same_cols_ref <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_Plate"
#'   )
#'
#' all_same_cols_rep <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_gene_name",
#'     "Metadata_pert_name"
#'   )
#'
#' all_same_cols_rep_ref <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_gene_name",
#'     "Metadata_pert_name",
#'     "Metadata_Plate"
#'   )
#'
#' any_different_cols_non_rep <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_gene_name",
#'     "Metadata_pert_name"
#'   )
#'
#' all_same_cols_non_rep <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_Plate"
#'   )
#'
#' all_different_cols_non_rep <-
#'   c("Metadata_gene_name")
#'
#' all_same_cols_group <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_gene_name"
#'   )
#'
#' any_different_cols_group <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_gene_name",
#'     "Metadata_pert_name"
#'   )
#'
#' annotation_cols <-
#'   c(
#'     "Metadata_cell_line",
#'     "Metadata_gene_name",
#'     "Metadata_pert_name"
#'   )
#'
#' collated_sim <-
#'   matric::sim_collate(
#'     sim_df,
#'     reference,
#'     all_same_cols_rep = all_same_cols_rep,
#'     all_same_cols_rep_ref = all_same_cols_rep_ref,
#'     all_same_cols_ref = all_same_cols_ref,
#'     any_different_cols_non_rep = any_different_cols_non_rep,
#'     all_same_cols_non_rep = all_same_cols_non_rep,
#'     all_different_cols_non_rep = all_different_cols_non_rep,
#'     any_different_cols_group = any_different_cols_group,
#'     all_same_cols_group = all_same_cols_group,
#'     annotation_cols = annotation_cols,
#'     drop_group = drop_group
#'   )
#'
#' head(collated_sim)
#'
#' collated_sim %>%
#'   dplyr::group_by(type) %>%
#'   dplyr::tally()
#' @export
#'
sim_collate <-
  function(sim_df,
           all_same_cols_rep,
           annotation_cols,
           all_same_cols_ref = NULL,
           all_same_cols_rep_ref = NULL,
           any_different_cols_non_rep = NULL,
           all_same_cols_non_rep = NULL,
           all_different_cols_non_rep = NULL,
           any_different_cols_group = NULL,
           all_same_cols_group = NULL,
           reference = NULL,
           drop_reference = FALSE,
           drop_group = NULL) {
    invisible(sim_validate(sim_df))

    sim_df_attr <- attributes(sim_df)

    row_metadata <- attr(sim_df, "row_metadata")

    # ---- 0. Filter out some rows ----

    if (!is.null(drop_group)) {
      sim_df %<>%
        sim_filter_keep_or_drop_some(row_metadata = row_metadata, filter_drop = drop_group, filter_side = "left") %>%
        sim_filter_keep_or_drop_some(row_metadata = row_metadata, filter_drop = drop_group, filter_side = "right")
    }

    fetch_ref <-
      !is.null(all_same_cols_ref) &&
        !is.null(reference)

    fetch_rep_ref <-
      !is.null(all_same_cols_ref) &&
        !is.null(reference) &&
        !is.null(all_same_cols_rep_ref)

    fetch_non_rep <-
      !is.null(any_different_cols_non_rep) &&
        !is.null(all_same_cols_non_rep) &&
        !is.null(all_different_cols_non_rep)

    fetch_rep_group <-
      !is.null(any_different_cols_group) &&
        !is.null(all_same_cols_group)

    # ---- 1. Similarity to reference ----

    # Fetch similarities between
    # a. all rows (except, optionally those containing `reference`)
    # and
    # b. all rows containing `reference`
    # Do so only for those (a, b) pairs that
    # - have *same* values in *all* columns of `all_same_cols_ref`

    if (fetch_ref) {
      ref <-
        sim_df %>%
        sim_all_same_keep_some(
          row_metadata = row_metadata,
          all_same_cols_ref,
          filter_keep_right = reference,
          drop_reference = drop_reference,
          annotation_cols
        )
    }

    # ---- 2. Similarity to replicates (no references) ----

    # Fetch similarities between
    # a. all rows except `reference` rows
    # and
    # b. all rows except `reference` rows (i.e. to each other)
    #
    # Do so for only those (a, b) pairs that
    # - have *same* values in *all* columns of `all_same_cols_rep
    #
    # Keep, both, (a, b) and (b, a)

    rep <-
      sim_df %>%
      sim_filter_keep_or_drop_some(row_metadata = row_metadata, filter_drop = reference, filter_side = "left") %>%
      sim_filter_keep_or_drop_some(row_metadata = row_metadata, filter_drop = reference, filter_side = "right") %>%
      sim_all_same(
        row_metadata = row_metadata,
        all_same_cols_rep,
        annotation_cols,
        drop_lower = FALSE
      )

    # ---- 3. Similarity to replicates (only references) ----

    # Fetch similarities between
    # a. all rows containing `reference`
    # and
    # b. all rows containing `reference` (i.e. to each other)
    #
    # Do so for only those (a, b) pairs that
    # - have *same* values in *all* columns of `all_same_cols_rep_ref`.
    #
    # Keep, both, (a, b) and (b, a)

    if (fetch_rep_ref) {
      rep_ref <-
        sim_df %>%
        sim_filter_keep_or_drop_some(
          row_metadata = row_metadata,
          filter_keep = reference,
          filter_side = "left"
        ) %>%
        sim_filter_keep_or_drop_some(
          row_metadata = row_metadata,
          filter_keep = reference,
          filter_side = "right"
        ) %>%
        sim_all_same(
          row_metadata = row_metadata,
          all_same_cols = all_same_cols_rep_ref,
          annotation_cols = annotation_cols,
          drop_lower = FALSE
        )
    }

    # ---- 4. Similarity to non-replicates ----

    # Fetch similarities between
    # a. all rows (except, optionally, `reference` rows)
    # and
    # b. all rows except `reference` rows
    #
    # Do so for only those (a, b) pairs that
    # - have *same* values in *all* columns of `all_same_cols_non_rep`
    # - have *different* values in *all* columns `all_different_cols_non_rep`
    # - have *different* values in *at least one* column of `any_different_cols_non_rep`
    #
    # Keep, both, (a, b) and (b, a)

    if (fetch_non_rep) {
      if (drop_reference) {
        reference_left <- reference
      } else {
        reference_left <- NULL
      }

      non_rep <-
        sim_df %>%
        sim_some_different_drop_some(
          row_metadata = row_metadata,
          any_different_cols = any_different_cols_non_rep,
          all_same_cols = all_same_cols_non_rep,
          all_different_cols = all_different_cols_non_rep,
          filter_drop_left = reference_left,
          filter_drop_right = reference,
          annotation_cols = annotation_cols
        )
    }

    # ---- 5. Similarity to group ----

    # Fetch similarities between
    # a. all rows (except, optionally, `reference` rows)
    # and
    # b. all rows (except, optionally, `reference` rows)
    #
    # Do so for only those (a, b) pairs that
    # - have *same* values in *all* columns of `all_same_cols_group`
    # - have *different* values in *at least one* column of `any_different_cols_group`
    #
    # Keep, both, (a, b) and (b, a)

    if (fetch_rep_group) {
      if (drop_reference) {
        reference_both <- reference
      } else {
        reference_both <- NULL
      }

      rep_group <-
        sim_df %>%
        sim_some_different_drop_some(
          row_metadata = row_metadata,
          any_different_cols = any_different_cols_group,
          all_same_cols = all_same_cols_group,
          filter_drop_left = reference_both,
          filter_drop_right = reference_both,
          annotation_cols = annotation_cols
        )
    }

    # 6. Combine

    combined <-
      rep %>% dplyr::mutate(type = "rep")

    if (fetch_rep_ref) {
      combined %<>%
        dplyr::bind_rows(rep_ref %>% dplyr::mutate(type = "rep")) # same tag as ref
    }

    if (fetch_non_rep) {
      combined %<>%
        dplyr::bind_rows(non_rep %>% dplyr::mutate(type = "non_rep"))
    }

    if (fetch_ref) {
      combined %<>%
        dplyr::bind_rows(ref %>% dplyr::mutate(type = "ref"))
    }

    if (fetch_rep_group) {
      combined %<>%
        dplyr::bind_rows(rep_group %>% dplyr::mutate(type = "rep_group"))
    }

    # add attributes

    attr(combined, "all_different_cols_non_rep") <-
      all_different_cols_non_rep
    attr(combined, "all_same_cols_group") <- all_same_cols_group
    attr(combined, "all_same_cols_non_rep") <- all_same_cols_non_rep
    attr(combined, "all_same_cols_ref") <- all_same_cols_ref
    attr(combined, "all_same_cols_rep") <- all_same_cols_rep
    attr(combined, "all_same_cols_rep_ref") <- all_same_cols_rep_ref
    attr(combined, "annotation_cols") <- annotation_cols
    attr(combined, "any_different_cols_group") <-
      any_different_cols_group
    attr(combined, "any_different_cols_non_rep") <-
      any_different_cols_non_rep
    attr(combined, "drop_group") <- drop_group
    attr(combined, "drop_reference") <- drop_reference
    attr(combined, "reference") <- reference

    combined %<>% as.data.frame()

    sim_restore(combined, sim_df_attr)
  }
