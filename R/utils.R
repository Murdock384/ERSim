#' @title Utility Functions for ERsim
#' @description Internal helper functions shared across the package.
#' @keywords internal

# ── Urgency level constants ────────────────────────────────────────────────
URGENCY_CRITICAL  <- 1L
URGENCY_URGENT    <- 2L
URGENCY_STANDARD  <- 3L

URGENCY_LABELS <- c(
  "1" = "Critical",
  "2" = "Urgent",
  "3" = "Standard"
)

#' Convert urgency integer to label
#' @param urgency Integer urgency level (1, 2, or 3).
#' @return Character label.
#' @keywords internal
urgency_label <- function(urgency) {
  lbl <- URGENCY_LABELS[as.character(urgency)]
  ifelse(is.na(lbl), "Unknown", lbl)
}

#' Sample urgency level for a new patient
#' @param probs Named or unnamed numeric vector of length 3 summing to 1.
#'   Order: c(critical, urgent, standard).
#' @return Integer urgency level (1L, 2L, or 3L).
#' @keywords internal
sample_urgency <- function(probs) {
  sample(c(URGENCY_CRITICAL, URGENCY_URGENT, URGENCY_STANDARD),
         size = 1L, prob = probs)
}

#' Draw an inter-arrival time from an exponential distribution
#' @param rate Arrival rate (patients per time unit).
#' @return Non-negative numeric inter-arrival time.
#' @keywords internal
draw_interarrival <- function(rate) {
  rexp(1L, rate = rate)
}

#' Draw a service time given urgency level and service parameters
#' @param urgency Integer urgency level.
#' @param service_params List with elements named "1", "2", "3", each a list
#'   with \code{mean} and \code{sd} for a truncated-normal-like draw (uses
#'   lognormal parameterisation: meanlog/sdlog or falls back to mean/sd).
#' @return Positive numeric service time.
#' @keywords internal
draw_service_time <- function(urgency, service_params) {
  params <- service_params[[as.character(urgency)]]
  if (is.null(params)) {
    stop("No service parameters for urgency level: ", urgency)
  }
  # Use lognormal so service times are always positive
  meanlog <- log(params$mean^2 / sqrt(params$sd^2 + params$mean^2))
  sdlog   <- sqrt(log(1 + (params$sd / params$mean)^2))
  rlnorm(1L, meanlog = meanlog, sdlog = sdlog)
}

#' Format minutes as a human-readable string
#' @param minutes Numeric value in minutes.
#' @return Character string, e.g. "1h 23m" or "45m".
#' @keywords internal
format_minutes <- function(minutes) {
  minutes <- round(minutes)
  h <- minutes %/% 60L
  m <- minutes %% 60L
  if (h > 0L) paste0(h, "h ", m, "m") else paste0(m, "m")
}

#' Generate a unique patient ID string
#' @param n Sequential patient number.
#' @return Character ID, e.g. "P00042".
#' @keywords internal
make_patient_id <- function(n) {
  sprintf("P%05d", n)
}

#' Launch the ERsim Shiny Dashboard
#'
#' Opens the interactive ER simulation dashboard in the default browser.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return Does not return (runs the Shiny app).
#' @export
launch_ersim_app <- function(...) {
  app_dir <- system.file("shiny", package = "ERsim")
  if (!nzchar(app_dir)) {
    stop("Could not find Shiny app directory. Is ERsim installed correctly?",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
