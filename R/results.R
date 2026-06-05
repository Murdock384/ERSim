#' @title SimResults — Simulation Results S3 Class
#' @description S3 class holding the output of a completed simulation run, with
#'   print, summary, and plot methods.

#' Construct a SimResults Object (internal)
#'
#' @param patient_log data.frame. One row per completed patient.
#' @param kpis Named list produced by \code{\link{compute_kpis}}.
#' @param queue_over_time data.frame produced by \code{\link{compute_queue_over_time}}.
#' @param resource_log data.frame. One row per resource with utilisation data.
#' @param config The \code{SimConfig} used for this run.
#' @param elapsed_sec Numeric. Wall-clock seconds taken to run the simulation.
#'
#' @return A \code{SimResults} S3 object.
#' @keywords internal
new_sim_results <- function(patient_log,
                            kpis,
                            queue_over_time,
                            queue_ts_analysis,
                            resource_log,
                            config,
                            elapsed_sec = NA_real_) {
  structure(
    list(
      patient_log       = patient_log,
      kpis              = kpis,
      queue_over_time   = queue_over_time,
      queue_ts_analysis = queue_ts_analysis,
      resource_log      = resource_log,
      config            = config,
      elapsed_sec       = elapsed_sec
    ),
    class = "SimResults"
  )
}

# ── S3 Methods ────────────────────────────────────────────────────────────────

#' Print a SimResults Object
#'
#' @param x A \code{SimResults} object.
#' @param ... Ignored.
#' @export
print.SimResults <- function(x, ...) {
  k <- x$kpis
  cat("── ERsim Results ───────────────────────────────────────────\n")
  cat(sprintf("  Patients served    : %d\n",    k$n_patients))
  cat(sprintf("  Mean wait time     : %.1f min\n", k$mean_wait))
  cat(sprintf("  Median wait time   : %.1f min\n", k$median_wait))
  cat(sprintf("  95th pct wait      : %.1f min\n", k$p95_wait))
  cat(sprintf("  Max wait time      : %.1f min\n", k$max_wait))
  cat(sprintf("  Throughput         : %.1f patients/hr\n", k$throughput_per_hour))
  cat("  Wait by urgency    :\n")
  for (i in seq_len(nrow(k$wait_by_urgency_df))) {
    row <- k$wait_by_urgency_df[i, ]
    cat(sprintf("    %-10s  n=%-4d  mean=%.1f min  p95=%.1f min\n",
                row$urgency_label, row$n, row$mean_wait, row$p95_wait))
  }
  if (!is.null(x$resource_log) && nrow(x$resource_log) > 0) {
    cat("  Resource utilisation:\n")
    for (i in seq_len(nrow(x$resource_log))) {
      row <- x$resource_log[i, ]
      cat(sprintf("    %-14s  %.1f%%\n", row$id, row$utilisation * 100))
    }
  }
  if (!is.null(x$queue_ts_analysis)) {
    ts <- x$queue_ts_analysis
    cat(sprintf("  Queue trend        : %s (slope %.3f pts/min)\n",
                ts$trend_label, ts$trend_slope))
    cat(sprintf("  Queue peak         : %d patients at t=%.1f min\n",
                ts$peak_length, ts$peak_time))
    cat(sprintf("  ACF lag-1          : %.3f\n", ts$acf_values[["lag1"]]))
  }
  if (!is.na(x$elapsed_sec)) {
    cat(sprintf("  Simulation ran in  : %.2f sec\n", x$elapsed_sec))
  }
  cat("────────────────────────────────────────────────────────────\n")
  invisible(x)
}

#' Summary of a SimResults Object
#'
#' @param object A \code{SimResults} object.
#' @param ... Ignored.
#' @export
summary.SimResults <- function(object, ...) {
  cat("SimResults Summary\n")
  cat(sprintf("Config: %.2f arrivals/min, %d doctors, %d nurses, %.0f min duration\n",
              object$config$arrival_rate,
              object$config$n_doctors,
              object$config$n_nurses,
              object$config$sim_duration))
  print(object)
  invisible(object)
}

#' Plot a SimResults Object
#'
#' Produces a 2x2 grid of diagnostic plots:
#' 1. Wait time distribution by urgency (histogram)
#' 2. Queue length over simulation time (line)
#' 3. Mean wait time by urgency (bar)
#' 4. Resource utilisation (bar)
#'
#' @param x A \code{SimResults} object.
#' @param ... Ignored.
#' @return A \code{ggplot} object (invisibly).
#' @export
plot.SimResults <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Packages 'ggplot2' and 'gridExtra' are required for plot.SimResults().",
         call. = FALSE)
  }

  pl <- x$patient_log
  pl$urgency_label <- factor(pl$urgency_label,
                             levels = c("Critical", "Urgent", "Standard"))

  # 1. Wait time histogram
  p1 <- ggplot2::ggplot(
    pl[!is.na(pl$wait_time), ],
    ggplot2::aes(x = wait_time, fill = urgency_label)
  ) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
    ggplot2::scale_fill_manual(values = URGENCY_COLOURS) +
    ggplot2::labs(title = "Wait Time Distribution",
                  x = "Wait Time (min)", y = "Count", fill = "Urgency") +
    ggplot2::theme_minimal()

  # 2. Queue length over time
  p2 <- ggplot2::ggplot(
    x$queue_over_time,
    ggplot2::aes(x = time, y = queue_length)
  ) +
    ggplot2::geom_line(colour = "#3498db", linewidth = 0.7) +
    ggplot2::labs(title = "Queue Length Over Time",
                  x = "Time (min)", y = "Patients in Queue") +
    ggplot2::theme_minimal()

  # 3. Mean wait by urgency
  p3 <- ggplot2::ggplot(
    x$kpis$wait_by_urgency_df,
    ggplot2::aes(x = urgency_label, y = mean_wait, fill = urgency_label)
  ) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.85) +
    ggplot2::scale_fill_manual(values = URGENCY_COLOURS) +
    ggplot2::labs(title = "Mean Wait by Urgency",
                  x = "Urgency", y = "Mean Wait (min)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  # 4. Resource utilisation
  if (!is.null(x$resource_log) && nrow(x$resource_log) > 0) {
    p4 <- ggplot2::ggplot(
      x$resource_log,
      ggplot2::aes(x = reorder(id, utilisation), y = utilisation * 100,
                   fill = role)
    ) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.85) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Resource Utilisation",
                    x = NULL, y = "Utilisation (%)", fill = "Role") +
      ggplot2::theme_minimal()
  } else {
    p4 <- ggplot2::ggplot() +
      ggplot2::labs(title = "Resource Utilisation (no data)") +
      ggplot2::theme_minimal()
  }

  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2L)
  invisible(list(p1, p2, p3, p4))
}
