utils::globalVariables(c("data", "sim_ranked_relrank", "type"))
#' Compute metrics.
#'
#' \code{sim_metrics} computes metrics.
#'
#' @param collated_sim output of \code{sim_collated}, which is a data.frame with some attributes.
#' @param sim_type character string specifying the background distributions for computing scaled metrics. This must be one of the strings \code{"non_rep"} or \code{"ref"}.
#' @param calculate_grouped optional boolean specifying whether to include grouped metrics.
#' @param annotation_prefix optional character string specifying prefix for annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return List of metrics.
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
#'   metrics$level_1_0,
#'   aes(sim_scaled_mean_ref_i, fill = Metadata_gene_name)
#' ) +
#'   geom_histogram(binwidth = .1) +
#'   facet_wrap(~Metadata_cell_line)
#'
#' ggplot(
#'   metrics$level_1,
#'   aes(sim_scaled_mean_ref_i_mean_i, fill = Metadata_gene_name)
#' ) +
#'   geom_histogram(binwidth = .1) +
#'   facet_wrap(~Metadata_cell_line)
#'
#' ggplot(
#'   metrics$level_2_1,
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

  # ---- Replicates ----
  sim_norm_agg <-
    helper_scale_aggregate(collated_sim,
                           sim_type, c("id1", rep_cols), "rep", "i")

  # get a summary per set

  sim_norm_agg_agg <-
    sim_norm_agg %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(rep_cols)))) %>%
    dplyr::summarise(dplyr::across(-dplyr::all_of("id1"),
                                   list(mean = mean, median = median)),
                     .groups = "keep") %>%
    dplyr::ungroup()

  # append identified ("_i" for "individual")

  sim_norm_agg_agg <- sim_norm_agg_agg %>%
    dplyr::rename_with(~ paste(., "i", sep = "_"),
                       dplyr::starts_with("sim"))

  result <-
    list(level_1_0 = sim_norm_agg,
         level_1 = sim_norm_agg_agg)

  # ---- Group replicates  ----

  if (calculate_grouped) {
    sim_norm_group_agg <-
      helper_scale_aggregate(collated_sim,
                             sim_type, rep_cols, "rep_group", "g")

    result <-
      c(result,
        list(level_2_1 = sim_norm_group_agg))
  }

  result
}

#' Helper function to compute metrics.
#'
#' \code{helper_scale_aggregate} helps compute metrics by agrregating and
#' scaling.
#'
#' @param collated_sim output of \code{sim_collated}, which is a data.frame with some attributes.
#' @param sim_type character string specifying the background distributions for computing scaled metrics. This must be one of the strings \code{"non_rep"} or \code{"ref"}.
#' @param summary_cols character list specifying columns by which to group similarities.
#' @param sim_type_replication character string specifying the type of replication being measured. This must be one of the strings \code{"rep"} or \code{"rep_group"}.
#' @param identifier character string specifying the identifier to add as a suffix to the columns containing scaled-aggregated metrics.
#'
#' @return data.frame of metrics.
helper_scale_aggregate <-
  function(collated_sim,
           sim_type,
           summary_cols,
           sim_type_replication,
           identifier = NULL) {
    # ---- Get background and signal distributions ----

    sim_background <-
      collated_sim %>%
      dplyr::filter(type == sim_type) %>%
      dplyr::select(-type)

    sim_signal <-
      collated_sim %>%
      dplyr::filter(type == sim_type_replication) %>%
      dplyr::select(-type)

    # ---- Scale with respect to background distribution ----

    # Compute statistics (mean and s.d.) on background distribution defined by
    # `sim_type`
    sim_stats <-
      sim_background %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of("sim"), list(mean = mean, sd = sd)),
                       .groups = "keep") %>%
      dplyr::ungroup()

    # scale using mean and s.d. of the `sim_type` distribution

    join_cols <-
      intersect(colnames(collated_sim), colnames(sim_stats))

    sim_norm_scaled <-
      sim_signal %>%
      dplyr::inner_join(sim_stats, by = join_cols) %>%
      dplyr::mutate(sim_scaled = (sim - sim_mean) / sim_sd)

    # ---- Rank with respect to background distribution ----

    sim_background_nested <-
      sim_background %>%
      dplyr::group_by(id1) %>%
      dplyr::arrange(dplyr::desc(sim)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(join_cols, "sim"))) %>%
      tidyr::nest(data = c(sim))

    sim_norm_ranked <-
      sim_signal %>%
      dplyr::inner_join(sim_background_nested, by = join_cols) %>%
      dplyr::mutate(sim_ranked_relrank = purrr::map2(sim, data, function(sim, df) {
        which(sim >= df$sim)[1] / nrow(df)
      })) %>%
      tidyr::unnest(sim_ranked_relrank) %>%
      dplyr::select(-data) %>%
      dplyr::mutate(sim_ranked_relrank = tidyr::replace_na(sim_ranked_relrank, 1))


    # ---- Combine scale-based and rank-based metrics ----

    # Use the columns of `sim_signal` to join (because `sim_norm_scaled` and
    # `sim_norm_ranked` add extra columns to `sim_signal`, making the columns
    # of `sim_signal` common to the two)

    sim_norm <-
      dplyr::inner_join(sim_norm_scaled,
                        sim_norm_ranked,
                        by = colnames(sim_signal))

    # ---- Summarize transformed (scale-based and rank-based) metrics ----

    # Get a summary per group (a group is defined by `summary_cols`)
    # create summaries for all
    # - `sim` (raw similarities)
    # - `sim_scaled` (centered and scaled similarities)
    # - `sim_ranked` (rank based transformations)

    sim_norm_agg <-
      sim_norm %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
      dplyr::summarise(dplyr::across(dplyr::any_of(
        c("sim_scaled", "sim_ranked_relrank", "sim")
      ),
      list(mean = mean, median = median)),
      .groups = "keep") %>%
      dplyr::rename_with(~ paste(., sim_type, sep = "_"),
                         dplyr::starts_with("sim_scaled")) %>%
      dplyr::rename_with(~ paste(., sim_type, sep = "_"),
                         dplyr::starts_with("sim_ranked_relrank")) %>%
      dplyr::ungroup()

    sim_norm_agg <- sim_norm_agg %>%
      dplyr::inner_join(sim_stats %>%
                          dplyr::rename_with(
                            ~ paste(., "stat", sim_type, sep = "_"),
                            dplyr::starts_with("sim")
                          ),
                        by = join_cols)

    # add a suffix to identify the summary columns
    if (!is.null(identifier)) {
      sim_norm_agg <- sim_norm_agg %>%
        dplyr::rename_with(~ paste(., identifier, sep = "_"),
                           dplyr::starts_with("sim"))
    }

    sim_norm_agg
  }
