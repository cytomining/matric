utils::globalVariables(
  c(
    "data_background",
    "data_retrieval",
    "data_signal",
    "signal_probrank",
    "sim_ranked_relrank",
    "sim_mean_stat",
    "sim_sd_stat",
    "truth",
    "type",
    ".estimate"
  )
)
#' Compute metrics.
#'
#' \code{sim_metrics} computes metrics.
#'
#' @param collated_sim output of \code{sim_collated}, which is a data.frame
#'   with some attributes.
#' @param sim_type_background character string specifying the background
#'   distributions for computing scaled metrics. This must be one of the
#'   strings \code{"non_rep"} or \code{"ref"}.
#' @param calculate_grouped optional boolean specifying whether to include
#'   grouped metrics.
#' @param annotation_prefix optional character string specifying prefix for
#'   annotation columns (e.g. \code{"Metadata_"} (default)).
#' @param use_furrr boolean indicating whether to use the furrr library
#'   for parallel processing.
#' @param calculate_pvalue optional boolean specifying whether to calculate
#'   p-values for the metrics
#' @param ... arguments passed down to \code{"sim_metrics_signif}
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
                        sim_type_background,
                        calculate_grouped = FALSE,
                        annotation_prefix = "Metadata_",
                        use_furrr = FALSE,
                        calculate_pvalue = FALSE,
                        ...) {
  annotation_cols <-
    stringr::str_subset(colnames(collated_sim), pattern = annotation_prefix)

  if (!is.null(attr(collated_sim, "all_same_cols_rep", TRUE))) {
    summary_cols <- attr(collated_sim, "all_same_cols_rep", TRUE)
  } else {
    logger::log_warn("Inferring columns specifying replicates from `sim_df`...")
    summary_cols <- annotation_cols
  }

  # ---- Level 1-0 ----

  sim_metrics_collated <-
    sim_metrics_helper(
      collated_sim = collated_sim,
      sim_type_signal = "rep",
      sim_type_background = sim_type_background,
      summary_cols = c("id1", summary_cols),
      annotation_cols = annotation_cols,
      identifier = "i",
      use_furrr = use_furrr
    )

  if (calculate_pvalue) {
    metric_group <- "retrieval"

    metric_name <- "average_precision"

    sim_metrics_collated <-
      sim_metrics_collated %>%
      sim_metrics_signif(
        background_type = sim_type_background,
        level_identifier = "i",
        metric_name = "average_precision",
        ...
      )
  }

  # ---- Level 1 (aggregations of Level 1-0) ----

  sim_metrics_collated_agg <-
    sim_metrics_collated %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::matches("sim_"),
        list(mean = mean, median = median)
      ),
      .groups = "keep"
    ) %>%
    dplyr::ungroup()


  annotation_cols_full <- unique(c(summary_cols, annotation_cols))

  metadata <-
    sim_metrics_collated %>%
    dplyr::distinct(across(all_of(annotation_cols_full)))

  sim_metrics_collated_agg <-
    sim_metrics_collated_agg %>%
    dplyr::inner_join(metadata, by = summary_cols, multiple = "all") %>%
    dplyr::select(all_of(annotation_cols_full), dplyr::everything())

  # append identifier to summarized metrics ("_i" for "individual")

  sim_metrics_collated_agg <-
    sim_metrics_collated_agg %>%
    dplyr::rename_with(
      ~ paste(., "i", sep = "_"),
      dplyr::starts_with("sim")
    )

  if (calculate_pvalue) {
    level_identifier <- "i"

    metric_nlog10pvalue <- # nolint:object_usage_linter
      glue::glue(
        "sim_{metric_group}_{metric_name}_{sim_type_background}_{level_identifier}_nlog10pvalue_mean_{level_identifier}"
      )

    metric_nlog10qvalue <- # nolint:object_usage_linter
      glue::glue(
        "sim_{metric_group}_{metric_name}_{sim_type_background}_{level_identifier}_nlog10qvalue_mean_{level_identifier}"
      )

    sim_metrics_collated_agg <-
      sim_metrics_collated_agg %>%
      dplyr::mutate("{metric_nlog10qvalue}" :=
                      -log10(stats::p.adjust(10 ** -.data[[metric_nlog10pvalue]],
                                             method = "BH")))

  }

  # ---- Level 2_1 ----

  if (calculate_grouped) {
    sim_metrics_group_collated <-
      sim_metrics_helper(
        collated_sim = collated_sim,
        sim_type_signal = "rep_group",
        sim_type_background = sim_type_background,
        summary_cols = summary_cols,
        annotation_cols = annotation_cols,
        identifier = "g",
        use_furrr = use_furrr
      )

    if (calculate_pvalue) {
      metric_group <- "retrieval"

      metric_name <- "average_precision"

      sim_metrics_group_collated <-
        sim_metrics_group_collated %>%
        sim_metrics_signif(
          background_type = sim_type_background,
          level_identifier = "g",
          metric_name = "average_precision",
          ...
        )
    }
  }

  # ---- Collect metrics  ----

  result <-
    list(
      level_1_0 = sim_metrics_collated,
      level_1 = sim_metrics_collated_agg
    )


  if (calculate_grouped) {
    result <-
      c(
        result,
        list(level_2_1 = sim_metrics_group_collated)
      )
  }

  result
}

#' Helper function to compute metrics.
#'
#' \code{sim_metrics_helper} helps compute metrics by agrregating and
#' scaling.
#'
#' @param collated_sim output of \code{sim_collated}, which is a data.frame with
#'   some attributes.
#' @param sim_type_signal character string specifying the type of
#'   replication being measured. This must be one of the strings \code{"rep"}
#'   or \code{"rep_group"}.
#' @param sim_type_background character string specifying the background
#'   distributions for computing scaled metrics. This must be one of the
#'   strings \code{"non_rep"} or \code{"ref"}.
#' @param summary_cols character list specifying columns by which to group
#'   similarities.
#' @param annotation_cols character list specifying annotation columns.
#' @param identifier character string specifying the identifier to add as a
#'   suffix to the columns containing scaled-aggregated metrics.
#' @param use_furrr boolean indicating whether to use the furrr library
#'   for parallel processing.
#'
#' @return data.frame of metrics.
sim_metrics_helper <-
  function(collated_sim,
           sim_type_signal,
           sim_type_background,
           summary_cols,
           annotation_cols,
           identifier = NULL,
           use_furrr = FALSE) {
    logger::log_trace("Compute metrics for signal={sim_type_signal} background={sim_type_background}")

    if (use_furrr) {
      logger::log_trace("Using furrr for parallel processing")
      logger::log_trace(future::plan())
      x_map2_dbl <- furrr::future_map2_dbl
      x_map_dbl <- furrr::future_map_dbl
      x_map2 <- furrr::future_map2
    } else {
      logger::log_trace("Using purrr for sequential processing")
      x_map2_dbl <- purrr::map2_dbl
      x_map_dbl <- purrr::map_dbl
      x_map2 <- purrr::map2
    }

    # ---- Get background and signal distributions ----

    sim_background <-
      collated_sim %>%
      dplyr::filter(type == sim_type_background) %>%
      dplyr::select(-type)

    sim_signal <-
      collated_sim %>%
      dplyr::filter(type == sim_type_signal) %>%
      dplyr::select(-type)

    # ---- Get nested versions of background and signal distributions ----
    sim_signal_nested <-
      sim_signal %>%
      dplyr::group_by(id1) %>%
      dplyr::arrange(dplyr::desc(sim)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(summary_cols, "sim"))) %>%
      tidyr::nest(data_signal = c(sim))

    sim_background_nested <-
      sim_background %>%
      dplyr::group_by(id1) %>%
      dplyr::arrange(dplyr::desc(sim)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(summary_cols, "sim"))) %>%
      tidyr::nest(data_background = c(sim))

    logger::log_trace("  Number of rows = {n}", n = nrow(sim_signal_nested))

    # ---- Compute transformed metrics ----
    # ---- * Transform ----
    # ---- ** Transform similarity: Scale w.r.t background distribution ----

    logger::log_trace("  Computing `sim_scaled` ...")

    # Compute statistics (mean and s.d.) on background distribution defined by
    # `sim_type_background`
    # See https://github.com/tidyverse/dplyr/issues/6073
    sim_stats <-
      sim_background %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of("sim"),
          list(
            mean_stat = \(x) mean(x, na.rm = TRUE),
            sd_stat = \(x) sd(x, na.rm = TRUE)
          )
        ),
        .groups = "keep"
      ) %>%
      dplyr::ungroup()

    # scale using mean and s.d. of the `sim_type_background` distribution

    sim_signal_scaled <-
      sim_signal %>%
      dplyr::inner_join(sim_stats, by = summary_cols) %>%
      dplyr::mutate(sim_scaled = (sim - sim_mean_stat) / sim_sd_stat)

    # ---- ** Transform similarity: Rank w.r.t background distribution ----

    logger::log_trace("  Computing `sim_ranked_relrank` ...")

    sim_signal_ranked <-
      sim_signal %>%
      dplyr::inner_join(sim_background_nested, by = summary_cols) %>%
      dplyr::mutate(
        sim_ranked_relrank =
          x_map2_dbl(sim, data_background, relrank)
      ) %>%
      dplyr::mutate(
        sim_ranked_relrank =
          tidyr::replace_na(sim_ranked_relrank, 1)
      ) %>%
      dplyr::select(-data_background)


    # ---- * Collate transformed metrics ----

    # Use the columns of `sim_signal` to join (because `sim_signal_scaled` and
    # `sim_signal_ranked` add extra columns to `sim_signal`, making the columns
    # of `sim_signal` common to the two)

    sim_signal_transformed <-
      dplyr::inner_join(sim_signal_scaled,
        sim_signal_ranked,
        by = colnames(sim_signal)
      )

    # ---- * Summarize transformed metrics ----

    # Get a summary per group (a group is defined by `summary_cols`)
    # create summaries for all
    # - `sim` (raw similarities)
    # - `sim_scaled` (centered and scaled similarities)
    # - `sim_ranked` (rank based transformations)

    logger::log_trace("  Computing summaries ...")

    sim_signal_transformed_agg <-
      sim_signal_transformed %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::any_of(
            c("sim_scaled", "sim_ranked_relrank", "sim")
          ),
          list(mean = mean, median = median)
        ),
        .groups = "keep"
      )

    # include stats columns
    sim_signal_transformed_agg <- sim_signal_transformed_agg %>%
      dplyr::inner_join(sim_stats,
        by = summary_cols,
        multiple = "all"
      )

    # ---- Compute retrieval metrics ----

    # The scale-based and rank-based metrics above are computed per row
    # and then aggregated by group.
    # The metrics in this section are computed across the whole group
    # (and therefore don't need further summarizing)

    logger::log_trace("  Setting up retrieval ...")

    sim_signal_retrieval <-
      sim_signal_nested %>%
      dplyr::inner_join(sim_background_nested, by = summary_cols) %>%
      dplyr::mutate(
        data_retrieval =
          x_map2(
            data_signal,
            data_background,
            tidy_classprob_data
          )
      ) %>%
      dplyr::mutate(sim_stat_signal_n = purrr::map_int(data_signal, nrow)) %>%
      dplyr::mutate(sim_stat_background_n = purrr::map_int(data_background, nrow)) %>%
      dplyr::select(-data_background, -data_signal)

    # ---- * Average Precision ----

    logger::log_trace("  Computing average precision ...")

    sim_signal_retrieval <-
      sim_signal_retrieval %>%
      dplyr::mutate(
        sim_retrieval_average_precision =
          x_map_dbl(data_retrieval, average_precision)
      )

    # ---- * R-Precision ----

    logger::log_trace("  Computing R-precision ...")

    sim_signal_retrieval <-
      sim_signal_retrieval %>%
      dplyr::mutate(
        sim_retrieval_r_precision =
          x_map_dbl(data_retrieval, r_precision)
      )

    logger::log_trace("  Wrapping up metrics ...")

    sim_signal_retrieval <-
      sim_signal_retrieval %>%
      dplyr::select(-data_retrieval)

    # ---- Collate all metrics ----

    sim_metrics_collated <-
      dplyr::inner_join(sim_signal_transformed_agg,
        sim_signal_retrieval,
        by = summary_cols
      )

    # ---- Format result  ----

    # add a suffix to identify the background
    sim_metrics_collated <-
      sim_metrics_collated %>%
      dplyr::rename_with(
        ~ paste(., sim_type_background, sep = "_"),
        dplyr::matches("sim_.*_stat|sim_stat_.*|sim_retrieval|sim_scaled|sim_ranked")
      ) %>%
      dplyr::ungroup()
    # ^^^ we need to do "sim_.*_stat|sim_stat_.*" because there's an
    # inconsistency in naming the variables

    # add a suffix to identify the summary columns
    if (!is.null(identifier)) {
      sim_metrics_collated <-
        sim_metrics_collated %>%
        dplyr::rename_with(
          ~ paste(., identifier, sep = "_"),
          dplyr::starts_with("sim")
        )
    }

    # annotate the rows
    #
    # Note: if `annotation_cols` is more granular than `summary_cols`
    # (`summary_cols` is typically just `all_same_cols_rep`), then adding these
    # annotations will result in a duplication of rows. For example, if
    # `all_same_cols_rep` ==
    # c("Metadata_cell_line", "Metadata_gene_name", "Metadata_pert_name"
    # and
    # `annotation_cols` ==
    # c("Metadata_cell_line", "Metadata_gene_name", "Metadata_pert_name",
    #   "Metadata_Well")
    # we will end up with one entry per `Metadata_Well` in
    # `sim_metrics_collated`
    # This is as expected.

    annotation_cols_full <- unique(c(summary_cols, annotation_cols))

    metadata <-
      collated_sim %>%
      dplyr::distinct(across(all_of(annotation_cols_full)))

    sim_metrics_collated <-
      sim_metrics_collated %>%
      dplyr::inner_join(metadata, by = summary_cols, multiple = "all") %>%
      dplyr::select(all_of(annotation_cols_full), dplyr::everything())

    logger::log_trace(
      "Completed metrics for signal={sim_type_signal} background={sim_type_background}"
    )

    sim_metrics_collated
  }


#' Create a tidy data frame for using in class probability metrics
#'
#' @param signal data frame vector corresponding to signal, containing a
#'   numeric column `sim`, and possibly other columns.
#' @param background data frame corresponding to background, structered
#'   identically to `signal`.
#'
#' @return tidy data frame with two columns: `truth` (with two levels: `signal`
#'  and `background`) and `signal_probrank` indicating the relative rank
#'  (proxy for probability).
#'
#' @noRd
tidy_classprob_data <- function(signal, background) {
  dplyr::bind_rows(
    signal %>% dplyr::mutate(truth = "signal"),
    background %>% dplyr::mutate(truth = "background")
  ) %>%
    # Note for yardstick: "signal" is the second factor level
    dplyr::mutate(truth = as.factor(truth)) %>%
    dplyr::mutate(signal_probrank = rank(sim) / dplyr::n()) %>%
    dplyr::select(-sim) %>%
    dplyr::arrange(dplyr::desc(signal_probrank))
}

#' Compute relative rank of a value within a data fram
#'
#' @param sim numeric indicating the value of which to find the rank.
#' @param df data.frame containing a numeric column named `sim` in which to
#'   find the rank of `sim`
#'
#' @return numeric indicating relative rank of `sim`.
#'
#' @noRd
relrank <- function(sim, df) {
  which(sim >= df$sim)[1] / nrow(df)
}

#' Compute average precision.
#'
#' @param df data.frame generated by `tidy_classprob_data()`
#'
#' @return numeric indicating average precision of the `signal` class.
#'
#' @noRd
average_precision <- function(df) {
  df %>%
    # Set `event_level` as "second" because "signal" is the second
    # factor level in `truth`
    yardstick::average_precision(truth,
      signal_probrank,
      event_level = "second"
    ) %>%
    dplyr::pull(.estimate)
}

#' Compute R-precision.
#'
#' @param df data.frame generated by `tidy_classprob_data()` with the rows
#   sorted in descending order of `signal_probrank`.
#'
#' @return numeric indicating R-precision of the `signal` class.
#'
#' @noRd
r_precision <- function(df) {
  condition_positive <-
    df %>%
    dplyr::filter(truth == "signal") %>%
    nrow()

  # assumes df is already sorted by `desc(signal_probrank)` above
  true_positive <-
    df %>%
    dplyr::slice_head(n = condition_positive) %>%
    dplyr::filter(truth == "signal") %>%
    nrow()

  true_positive / condition_positive
}
