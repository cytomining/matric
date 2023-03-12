utils::globalVariables(c("sim_stat_average_precision_null_samples", "p_value"))

#' Report p-values for metrics
#'
#' @param background_type Background type. Either \code{"ref"} or \code{"non_rep"}.
#' @param level_identifier Level identifier. Either \code{"i"} (Level 1_0) or \code{"g"} (Level 2_1).
#' @param metrics Metrics data frame, containing at least two columns
#'  \code{"sim_stat_signal_n_{background_type}_{level_identifier}"} and
#'  \code{"sim_stat_background_n_{background_type}_{level_identifier}"}.
#' @param metric_name name of metric. Only \code{"average_precision"} is currently implemented.
#' @param ... arguments passed downstream.
#'
#' @return Metrics data frame containing two extra columns: the -log10 p-value and q-value of the specified metric
#'  \code{"sim_retrieval_average_precision_{background_type}_{level_identifier}_nlog10pvalue"}
#'
#' @export
sim_metrics_signif <-
  function(metrics,
           background_type,
           level_identifier,
           metric_name,
           ...) {
    stopifnot(metric_name == "average_precision")
    metric_group <- "retrieval"

    if (nrow(metrics) == 0) {
      logger::log_warn("Empty metrics data frame; no p-values to compute")
      return(metrics)
    }

    nulls <-
      null_distribution(
        metrics,
        background_type = background_type,
        level_identifier = level_identifier,
        metric_name = metric_name,
        ...
      )


    metric_nlog10pvalue <- # nolint:object_usage_linter
      glue::glue(
        "sim_{metric_group}_{metric_name}_{background_type}_{level_identifier}_nlog10pvalue"
      )

    metric_value <-
      glue::glue("sim_{metric_group}_{metric_name}_{background_type}_{level_identifier}")

    sim_stat_signal_n <-
      glue::glue("sim_stat_signal_n_{background_type}_{level_identifier}")

    metrics %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        p_value =
          get_p_value(
            nulls = nulls,
            m = .data[[sim_stat_signal_n]],
            n = .data[["sim_stat_background_n_mapped"]],
            statistic = .data[[metric_value]],
            metric_name = metric_name
          )
      ) %>%
      dplyr::mutate("{metric_nlog10pvalue}" := -log10(p_value)) %>%
      dplyr::select(-p_value) %>%
      dplyr::ungroup()

  }


#' Compute null distribution for a set of metrics
#'
#' @param background_type Background type. Either \code{"ref"} or \code{"non_rep"}.
#' @param level_identifier Level identifier. Either \code{"i"} (Level 1_0) or \code{"g"} (Level 2_1).
#' @param metrics Metrics data frame, containing at least the column
#'  \code{"sim_stat_background_n_{background_type}_{level_identifier}"}.
#' @param metric_name name of metric. Only \code{"average_precision"} is currently implemented.
#' @param n_iterations number of iterations for generating the null distribution
#' @param random_seed Random seed (default = 42)
#'
#' @return Nulls data frame
null_distribution <-
  function(metrics,
           background_type,
           level_identifier,
           metric_name = "average_precision",
           n_iterations = 10000,
           random_seed = 42) {
    stopifnot(metric_name == "average_precision")

    metrics <-
      metrics %>%
      dplyr::mutate(sim_stat_background_n_mapped =
                      bin(metrics[[glue::glue("sim_stat_background_n_{background_type}_{level_identifier}")]]))

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
        logger::log_trace("Compute retrieval random baseline for m = {m}, n = {n}")
        null_distribution_helper(m = m,
                                 n = n,
                                 nn = n_iterations)
      },
      .options = furrr::furrr_options(seed = random_seed))


    nulls

  }


#' Bin values into increasingly wider bins
#'
#' @param x values
#'
#' @return rebinned values
bin <- function(x) {
  pow <- 1.3

  max_value <- max(x)

  break_point <-
    ceiling(seq(1, ceiling((max_value) ^ (1 / pow)), 1) ** (pow))

  x %>% purrr::map_dbl(function(i) {
    break_point[min(which(break_point > i))]
  })
}


#' Compute null distribution of metrics
#'
#' @param m Number of positive examples (= number of replicates - 1)
#' @param n Number of negative examples (= number of controls, or number of non-replicates)
#' @param nn Number of simulations (default = 10000)
#' @param metric_name name of metric. Only \code{"average_precision"} is implemented.
#'
#' @return Null distribution data frame parametrized by m and n
null_distribution_helper <-
  function(m,
           n,
           nn = 10000,
           metric_name = "average_precision") {
    stopifnot(metric_name == "average_precision")

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

#' Compute p-value against null distribution
#'
#' @param nulls Null distribution data frame
#' @param m Number of positive examples
#' @param n Number of negative examples
#' @param statistic statistic for which to compute p-value
#' @param metric_name name of metric. Only \code{"average_precision"} is implemented.
#'
#' @return p-value
get_p_value <-
  function(nulls, m, n, statistic, metric_name = "average_precision") {
    stopifnot(metric_name == "average_precision")

    null_samples <-
      nulls %>%
      dplyr::filter(m == m, n == n) %>%
      dplyr::pull(glue::glue("sim_stat_{metric_name}_null_samples")) %>%
      purrr::pluck(1)

    (1 + sum(null_samples > statistic)) / (1 + length(null_samples))


  }
