#' Compute metrics.
#'
#' \code{sim_metrics} computes metrics.
#'
#' @param munged_sim output of \code{sim_collated}.
#' @param sim_type character string specifying the background distributions for computing scaled metrics. This must be one of the strings \code{"non_rep"} or \code{"ref"}.
#' @param calculate_grouped optional boolean specifying whether to include grouped metrics.
#' @param annotation_prefix optional character string specifying prefix for annotation columns (e.g. \code{"Metadata_"} (default)).
#'
#' @return list of metrics.
#'
#' @export
#'
sim_metrics <- function(munged_sim,
                        sim_type,
                        calculate_grouped = FALSE,
                        annotation_prefix = "Metadata_") {
  if (!is.null(attr(munged_sim, "all_same_cols_rep", TRUE))) {
    rep_cols <- attr(munged_sim, "all_same_cols_rep", TRUE)
  } else {
    message("Warning: Inferring columns specifying replicates from similarity dataframe...")
    rep_cols <-
      str_subset(colnames(munged_sim), pattern = annotation_prefix)
  }

  helper_scale_aggregate <-
    function(summary_cols,
             sim_type_replication,
             identifier = NULL) {
      sim_stats <-
        munged_sim %>%
        dplyr::filter(type == sim_type) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of("sim"), list(mean = mean, sd = sd)),
          .groups = "keep"
        ) %>%
        dplyr::ungroup()

      # scale using mean and s.d. of the `sim_type` distribution

      join_cols <-
        intersect(colnames(munged_sim), colnames(sim_stats))

      sim_norm <-
        munged_sim %>%
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
    )

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
