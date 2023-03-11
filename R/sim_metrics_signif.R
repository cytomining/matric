utils::globalVariables(
  c(
    "sim_stat_average_precision_null",
    "sim_stat_r_precision_null",
    "sim_stat_average_precision_null_samples"
  )
)

#' Report p-values for metrics
#'
#' @param background_type Background type. Either "ref" or "non_rep".
#' @param level_identifier Level identifier. Either "i" (Level 1_0) or "g" (Level 2_1).
#' @param metrics Metrics data frame, containing columns
#'  `sim_stat_signal_n_{background_type}_{level_identifier}` and
#'  `sim_stat_background_n_{background_type}_{level_identifier}`.
#' @param ... arguments passed downstream.
#'
#' @return Metrics data frame, containing additional columns
#'  `sim_retrieval_average_precision_{background_type}_{level_identifier}_nlog10pvalue`
#'
#' @export
sim_metrics_signif <-
  function(metrics,
           background_type,
           level_identifier,
           ...) {
    nulls <-
      retrieval_baseline(metrics,
                         background_type = background_type,
                         level_identifier = level_identifier,
                         ...)


    sim_retrieval_average_precision_nlog10pvalue <- # nolint:object_usage_linter
      glue::glue(
        "sim_retrieval_average_precision_{background_type}_{level_identifier}_nlog10pvalue"
      )

    sim_retrieval_average_precision <-
      glue::glue("sim_retrieval_average_precision_{background_type}_{level_identifier}")

    sim_stat_signal_n <-
      glue::glue("sim_stat_signal_n_{background_type}_{level_identifier}")

    sim_stat_background_n_mapped <-
      "sim_stat_background_n_mapped"

    metrics %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        "{sim_retrieval_average_precision_nlog10pvalue}" :=
          -log10(
            get_average_precision_p_value(
              nulls = nulls,
              m = .data[[sim_stat_signal_n]],
              n = .data[[sim_stat_background_n_mapped]],
              statistic = .data[[sim_retrieval_average_precision]]
            )
          )
      ) %>%
      dplyr::ungroup()

  }


#' Compute null thresholds for a set of metrics
#'
#' @param background_type Background type. Either "ref" or "non_rep"
#' @param level_identifier Level identifier. Either "i" (Level 1_0) or "g" (Level 2_1)
#' @param metrics Metrics data frame, containing columns
#'  `sim_stat_signal_n_{background_type}_{level_identifier}` and
#'  `sim_stat_background_n_{background_type}_{level_identifier}`
#' @param random_seed Random seed (default = 42)
#'
#' @return Nulls data frame
retrieval_baseline <-
  function(metrics,
           background_type,
           level_identifier,
           random_seed=42) {
    pow <- 1.3

    points <-
      metrics[[glue::glue("sim_stat_background_n_{background_type}_{level_identifier}")]]

    max_value <- max(points)

    break_point <-
      ceiling(seq(1, ceiling((max_value) ^ (1 / pow)), 1) ** (pow))

    points_mapped <-
      points %>% purrr::map_dbl(function(i) {
        break_point[min(which(break_point > i))]
      })

    metrics <-
      metrics %>%
      dplyr::mutate(sim_stat_background_n_mapped = points_mapped)

    nulls <-
      metrics %>%
      dplyr::distinct(across(all_of(
        c(
          glue::glue("sim_stat_signal_n_{background_type}_{level_identifier}"),
          "sim_stat_background_n_mapped"
        )
      ))) %>%
      dplyr::rename(m = 1, n = 2) %>%
      furrr::future_pmap_dfr(function(m, n) {
        logger::log_info("Compute retrieval random baseline for m = {m}, n = {n}")
        retrieval_baseline_helper(m = m,
                                  n = n,
                                  nn = 10000)
      },
      .options = furrr::furrr_options(seed = random_seed))


    nulls

  }

#' Estimate statistics of the distribution of information retrieval metrics under the null hypothesis
#'
#' @param m Number of positive examples (= number of replicates - 1)
#' @param n Number of negative examples (= number of controls, or number of non-replicates)
#' @param nn Number of simulations (default = 10000)
#'
#' @return Nulls data frame parametrized by m and n
retrieval_baseline_helper <-
  function(m,
           n,
           nn = 10000) {
    # Average precision

    y_rank <- 1 - (seq(m + n) / (m + n))

    average_precision_null_samples <-
      purrr::map_dbl(seq(nn), function(i) {
        x <- as.factor(sample(c(rep(FALSE, n), rep(TRUE, m))))

        yardstick::average_precision_vec(x, y_rank, event_level = "second")
      })

    data.frame(
      m = m,
      n = n,
      sim_stat_average_precision_null_samples = average_precision_null_samples
    ) %>%
      dplyr::group_by(m, n) %>%
      dplyr::summarize(
        sim_stat_average_precision_null_samples = list(sim_stat_average_precision_null_samples),
        .groups = "keep"
      )
  }

#' Get average precision p-value
#'
#' @param nulls Nulls data frame
#' @param m Number of positive examples (= number of replicates - 1)
#' @param n Number of negative examples (= number of controls, or number of non-replicates)
#' @param statistic Average precision statistic
#'
#' @return p-value
get_average_precision_p_value <-
  function(nulls, m, n, statistic) {
    null_samples <-
      nulls %>%
      dplyr::filter(m == m, n == n) %>%
      dplyr::pull("sim_stat_average_precision_null_samples") %>%
      purrr::pluck(1)

    (1 + sum(null_samples > statistic)) / (1 + length(null_samples))


  }
