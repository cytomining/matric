utils::globalVariables(c("type"))
#' Compute metrics.
#'
#' \code{sim_metrics} computes metrics.
#'
#' @param collated_sim output of \code{sim_collated}.
#' @param sim_type character string specifying the background distributions for computing scaled metrics. This must be one of the strings \code{"non_rep"} or \code{"ref"}.
#' @param calculate_grouped optional boolean specifying whether to include grouped metrics.
#' @param annotation_prefix optional character string specifying prefix for annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return list of metrics.
#' @examples
#'
#' suppressMessages(suppressWarnings(library(ggplot2)))
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
#' metrics <- matric::sim_metrics(collated_sim, "ref", calculate_grouped = TRUE)
#'
#' ggplot(
#'   metrics$per_row,
#'   aes(sim_scaled_mean_ref_i, fill = Metadata_gene_name)
#' ) +
#'   geom_histogram(binwidth = .1) +
#'   facet_wrap(~Metadata_cell_line)
#'
#' ggplot(
#'   metrics$per_set,
#'   aes(sim_scaled_mean_ref_i_mean_i, fill = Metadata_gene_name)
#' ) +
#'   geom_histogram(binwidth = .1) +
#'   facet_wrap(~Metadata_cell_line)
#'
#' ggplot(
#'   metrics$per_set_group,
#'   aes(sim_scaled_mean_ref_g, fill = Metadata_gene_name)
#' ) +
#'   geom_histogram(binwidth = .1) +
#'   facet_wrap(~Metadata_cell_line)
#' @importFrom stats median sd mad
#'
#' @export
sim_metrics <- function(collated_sim,
                        sim_type,
                        calculate_grouped = FALSE,
                        annotation_prefix = "Metadata_") {
  if (!is.null(attr(collated_sim, "all_same_cols_rep", TRUE))) {
    rep_cols <- attr(collated_sim, "all_same_cols_rep", TRUE)
  } else {
    message("Warning: Inferring columns specifying replicates from similarity dataframe...")
    rep_cols <-
      stringr::str_subset(colnames(collated_sim), pattern = annotation_prefix)
  }

  helper_scale_aggregate <-
    function(summary_cols,
             sim_type_replication,
             identifier = NULL) {
      sim_stats <-
        collated_sim %>%
        dplyr::filter(type == sim_type) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of("sim"), list(mean = mean, sd = sd)),
          .groups = "keep"
        ) %>%
        dplyr::ungroup()

      # scale using mean and s.d. of the `sim_type` distribution

      join_cols <-
        intersect(colnames(collated_sim), colnames(sim_stats))

      sim_norm <-
        collated_sim %>%
        dplyr::filter(type == sim_type_replication) %>%
        dplyr::inner_join(sim_stats, by = join_cols) %>%
        dplyr::mutate(sim_scaled = (sim - sim_mean) / sim_sd)

      # get a summary per group
      sim_norm_agg <-
        sim_norm %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
        dplyr::summarise(dplyr::across(
          dplyr::all_of(c(
            "sim_scaled", "sim"
          )),
          list(mean = mean, median = median)
        ),
        .groups = "keep"
        ) %>%
        dplyr::rename_with(
          ~ paste(., sim_type, sep = "_"),
          dplyr::starts_with("sim_scaled")
        ) %>%
        dplyr::ungroup()

      sim_norm_agg %<>%
        dplyr::inner_join(sim_stats %>%
          dplyr::rename_with(
            ~ paste(., "stat", sim_type, sep = "_"),
            dplyr::starts_with("sim")
          ),
        by = join_cols
        )

      if (!is.null(identifier)) {
        sim_norm_agg %<>%
          dplyr::rename_with(
            ~ paste(., identifier, sep = "_"),
            dplyr::starts_with("sim")
          )
      }

      sim_norm_agg
    }

  # ---- Replicates ----
  sim_norm_agg <-
    helper_scale_aggregate(c("id1", rep_cols), "rep", "i")

  # get a summary per set

  sim_norm_agg_agg <-
    sim_norm_agg %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(rep_cols)))) %>%
    dplyr::summarise(dplyr::across(
      -dplyr::all_of("id1"),
      list(mean = mean, median = median)
    ),
    .groups = "keep"
    ) %>%
    dplyr::ungroup()

  # append identified ("_i" for "individual")

  sim_norm_agg_agg %<>%
    dplyr::rename_with(
      ~ paste(., "i", sep = "_"),
      dplyr::starts_with("sim")
    )

  result <-
    list(
      per_row = sim_norm_agg,
      per_set = sim_norm_agg_agg
    )

  # ---- Group replicates  ----

  if (calculate_grouped) {
    sim_norm_group_agg <-
      helper_scale_aggregate(rep_cols, "rep_group", "g")

    result <-
      c(
        result,
        list(per_set_group = sim_norm_group_agg)
      )
  }

  result
}
