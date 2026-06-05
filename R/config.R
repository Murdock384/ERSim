#' @title SimConfig — Simulation Configuration S3 Class
#' @description Constructs and validates a simulation configuration object used
#'   to parameterise a \code{SimulationEngine} run.

#' Create a Simulation Configuration
#'
#' Constructs a \code{SimConfig} S3 object containing all parameters needed to
#' run an ER simulation. All inputs are validated before the object is returned.
#'
#' @param arrival_rate Numeric. Average number of patient arrivals per time unit
#'   (minute). Must be positive.
#' @param n_doctors Positive integer. Number of doctors available.
#' @param n_nurses Positive integer. Number of nurses available.
#' @param sim_duration Numeric. Total simulation duration in minutes. Must be
#'   positive.
#' @param urgency_probs Numeric vector of length 3 summing to 1. Probabilities
#'   of a patient being Critical, Urgent, or Standard respectively. Defaults to
#'   \code{c(0.2, 0.3, 0.5)}.
#' @param service_params Named list with keys \code{"1"}, \code{"2"},
#'   \code{"3"} (urgency levels). Each element is a list with \code{mean} and
#'   \code{sd} for the service time distribution (minutes). Defaults to
#'   clinically inspired values.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'
#' @return A \code{SimConfig} S3 object (a named list).
#' @export
#'
#' @examples
#' cfg <- new_sim_config(
#'   arrival_rate = 0.5,
#'   n_doctors    = 3,
#'   n_nurses     = 5,
#'   sim_duration = 480
#' )
#' print(cfg)
new_sim_config <- function(arrival_rate  = 0.5,
                           n_doctors     = 3L,
                           n_nurses      = 5L,
                           sim_duration  = 480,
                           urgency_probs = c(0.2, 0.3, 0.5),
                           service_params = list(
                             "1" = list(mean = 30, sd = 10),  # Critical
                             "2" = list(mean = 20, sd =  7),  # Urgent
                             "3" = list(mean = 15, sd =  5)   # Standard
                           ),
                           seed = NULL) {

  config <- structure(
    list(
      arrival_rate   = arrival_rate,
      n_doctors      = as.integer(n_doctors),
      n_nurses       = as.integer(n_nurses),
      sim_duration   = sim_duration,
      urgency_probs  = urgency_probs,
      service_params = service_params,
      seed           = seed
    ),
    class = "SimConfig"
  )

  validate_sim_config(config)
  config
}

#' Print a SimConfig object
#'
#' @param x A \code{SimConfig} object.
#' @param ... Ignored.
#' @export
print.SimConfig <- function(x, ...) {
  cat("── ERsim Configuration ─────────────────────────────────────\n")
  cat(sprintf("  Arrival rate   : %.3f patients/min\n", x$arrival_rate))
  cat(sprintf("  Doctors        : %d\n", x$n_doctors))
  cat(sprintf("  Nurses         : %d\n", x$n_nurses))
  cat(sprintf("  Duration       : %s (%.0f min)\n",
              format_minutes(x$sim_duration), x$sim_duration))
  cat(sprintf("  Urgency probs  : Critical=%.0f%%, Urgent=%.0f%%, Standard=%.0f%%\n",
              x$urgency_probs[1] * 100,
              x$urgency_probs[2] * 100,
              x$urgency_probs[3] * 100))
  cat("  Service times  :\n")
  for (lvl in c("1", "2", "3")) {
    p <- x$service_params[[lvl]]
    cat(sprintf("    %s: mean=%.1f min, sd=%.1f min\n",
                URGENCY_LABELS[[lvl]], p$mean, p$sd))
  }
  if (!is.null(x$seed)) {
    cat(sprintf("  Seed           : %d\n", x$seed))
  } else {
    cat("  Seed           : (not set)\n")
  }
  cat("────────────────────────────────────────────────────────────\n")
  invisible(x)
}
