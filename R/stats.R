#' @title Vectorized KPI Computation
#' @description Computes summary statistics from a patient log data.frame using
#'   vectorized R operations (no explicit loops).

#' Compute Key Performance Indicators from a Patient Log
#'
#' Takes the \code{patient_log} data.frame produced by a completed simulation
#' and returns a named list of KPIs computed entirely with vectorized
#' operations.
#'
#' @param patient_log A data.frame with at least the columns: \code{wait_time},
#'   \code{urgency_level}, \code{urgency_label}, \code{service_time},
#'   \code{arrival_time}, \code{end_service_time}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{n_patients}{Total patients who completed treatment.}
#'   \item{mean_wait}{Mean wait time across all patients (minutes).}
#'   \item{median_wait}{Median wait time (minutes).}
#'   \item{p95_wait}{95th percentile wait time (minutes).}
#'   \item{max_wait}{Maximum wait time (minutes).}
#'   \item{mean_wait_by_urgency}{Named numeric vector: mean wait per urgency.}
#'   \item{mean_service_time}{Mean service/treatment duration (minutes).}
#'   \item{throughput_per_hour}{Patients completing treatment per hour.}
#'   \item{wait_by_urgency_df}{data.frame with per-urgency wait statistics.}
#' }
#' @importFrom dplyr group_by summarise mutate arrange n
#' @export
compute_kpis <- function(patient_log) {
  if (!is.data.frame(patient_log)) {
    stop("`patient_log` must be a data.frame.", call. = FALSE)
  }
  required_cols <- c("wait_time", "urgency_level", "urgency_label",
                     "service_time", "arrival_time", "end_service_time")
  missing_cols <- setdiff(required_cols, names(patient_log))
  if (length(missing_cols) > 0L) {
    stop("patient_log is missing columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Two subsets:
  #   - waited:    all patients with a known wait_time (served + still in queue)
  #                Used for wait-time KPIs — unserved patients inflate the average
  #                correctly, as they represent the worst patient experience.
  #   - completed: only patients who reached a resource (have end_service_time)
  #                Used for service time, throughput, and n_patients served.
  waited    <- patient_log[!is.na(patient_log$wait_time), ]
  completed <- patient_log[!is.na(patient_log$end_service_time), ]

  if (nrow(waited) == 0L) {
    return(list(
      n_patients           = 0L,
      mean_wait            = NA_real_,
      median_wait          = NA_real_,
      p95_wait             = NA_real_,
      max_wait             = NA_real_,
      mean_wait_by_urgency = c(Critical = NA_real_, Urgent = NA_real_, Standard = NA_real_),
      mean_service_time    = NA_real_,
      throughput_per_hour  = NA_real_,
      wait_by_urgency_df   = data.frame()
    ))
  }

  wt <- waited$wait_time

  # Per-urgency wait stats — dplyr pipeline (includes unserved patients)
  lvl_order <- c("Critical", "Urgent", "Standard")
  wait_by_urgency_df <- waited |>
    dplyr::mutate(urgency_label = factor(urgency_label, levels = lvl_order)) |>
    dplyr::group_by(urgency_label) |>
    dplyr::summarise(
      n           = dplyr::n(),
      mean_wait   = mean(wait_time,   na.rm = TRUE),
      median_wait = median(wait_time, na.rm = TRUE),
      p95_wait    = quantile(wait_time, 0.95, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::arrange(urgency_label) |>
    dplyr::mutate(urgency_label = as.character(urgency_label))

  # Named vector of means (used for mean_wait_by_urgency field)
  urgency_means <- setNames(wait_by_urgency_df$mean_wait,
                            wait_by_urgency_df$urgency_label)

  # Throughput: patients per hour, based on simulation span
  time_span_min <- max(completed$end_service_time, na.rm = TRUE) -
                   min(completed$arrival_time,      na.rm = TRUE)
  throughput <- if (time_span_min > 0) nrow(completed) / (time_span_min / 60) else NA_real_

  list(
    n_patients           = nrow(completed),
    mean_wait            = mean(wt,   na.rm = TRUE),
    median_wait          = median(wt, na.rm = TRUE),
    p95_wait             = unname(quantile(wt, 0.95, na.rm = TRUE)),
    max_wait             = max(wt,    na.rm = TRUE),
    mean_wait_by_urgency = urgency_means,
    mean_service_time    = mean(completed$service_time, na.rm = TRUE),
    throughput_per_hour  = throughput,
    wait_by_urgency_df   = wait_by_urgency_df
  )
}

#' Compute Queue Length Over Simulation Time
#'
#' Given a patient log, reconstructs the queue length at each event
#' (arrival or service-start) using vectorized cumulative sum operations.
#'
#' @param patient_log data.frame from a completed simulation.
#' @return A data.frame with columns \code{time} and \code{queue_length}.
#' @export
compute_queue_over_time <- function(patient_log) {
  if (nrow(patient_log) == 0L) {
    return(data.frame(time = numeric(0), queue_length = integer(0)))
  }

  # Each arrival is +1, each service-start is -1
  arrivals <- data.frame(
    time  = patient_log$arrival_time,
    delta = 1L
  )
  starts <- data.frame(
    time  = patient_log$start_service_time[!is.na(patient_log$start_service_time)],
    delta = -1L
  )

  events <- rbind(arrivals, starts)
  events <- events[order(events$time), ]

  data.frame(
    time         = events$time,
    queue_length = pmax(cumsum(events$delta), 0L)
  )
}

#' Analyse Queue Length as a Time Series
#'
#' Computes a rolling mean, linear trend, and autocorrelation structure from
#' the queue-over-time data produced by \code{\link{compute_queue_over_time}}.
#' Results are stored in \code{SimResults$queue_ts_analysis} and displayed
#' on the Results tab of the dashboard.
#'
#' @param queue_over_time data.frame with columns \code{time} and
#'   \code{queue_length}, as returned by \code{compute_queue_over_time()}.
#' @param roll_window Integer. Number of events used for the rolling mean.
#'   Default 10.
#'
#' @return A named list with:
#' \describe{
#'   \item{rolling_mean}{data.frame: time, queue_length, rolling_mean.}
#'   \item{trend_slope}{Numeric. Queue growth rate in patients/min (lm slope).}
#'   \item{trend_label}{Character. "Building", "Stable", or "Draining".}
#'   \item{acf_values}{Named numeric vector: autocorrelation at lags 1-5.}
#'   \item{peak_time}{Numeric. Simulation time when queue was longest.}
#'   \item{peak_length}{Integer. Maximum queue length observed.}
#' }
#' @importFrom stats filter lm coef acf quantile
#' @export
compute_queue_ts <- function(queue_over_time, roll_window = 10L) {
  empty <- list(
    rolling_mean = queue_over_time,
    trend_slope  = NA_real_,
    trend_label  = "Insufficient data",
    acf_values   = setNames(rep(NA_real_, 5L), paste0("lag", 1:5)),
    peak_time    = NA_real_,
    peak_length  = NA_integer_
  )
  if (nrow(queue_over_time) < 2L) return(empty)

  ql <- queue_over_time$queue_length

  # Rolling mean via stats::filter (base R, no extra dependencies)
  k   <- min(as.integer(roll_window), length(ql))
  raw <- stats::filter(ql, rep(1.0 / k, k), sides = 1)
  rolling_df <- data.frame(
    time         = queue_over_time$time,
    queue_length = ql,
    rolling_mean = as.numeric(raw)
  )

  # Linear trend: slope = patients added to queue per minute of sim time
  fit         <- lm(queue_length ~ time, data = queue_over_time)
  slope       <- unname(coef(fit)[["time"]])
  trend_label <- if      (slope >  0.05) "Building"
                 else if (slope < -0.05) "Draining"
                 else                    "Stable"

  # Autocorrelation at lags 1-5 (suppresses the base plot)
  acf_obj  <- acf(ql, lag.max = 5L, plot = FALSE)
  acf_vals <- setNames(as.numeric(acf_obj$acf[-1L]), paste0("lag", 1:5))

  # Peak queue length
  peak_idx <- which.max(ql)

  list(
    rolling_mean = rolling_df,
    trend_slope  = slope,
    trend_label  = trend_label,
    acf_values   = acf_vals,
    peak_time    = queue_over_time$time[peak_idx],
    peak_length  = ql[peak_idx]
  )
}
