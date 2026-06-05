#' @title Defensive Programming — Input Validators
#' @description All input validation functions for ERsim. Each validator either
#'   returns invisibly on success or throws an informative error via
#'   \code{stop()}. This makes them composable: call several validators in
#'   sequence and the first failure aborts with a clear message.
#' @keywords internal

# ── Generic scalar validators ───────────────────────────────────────────────

#' Assert a value is a single positive number
#' @param x Value to test.
#' @param name Name to use in the error message.
#' @keywords internal
assert_positive_number <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    stop(sprintf("`%s` must be a single positive number, got: %s",
                 name, paste(x, collapse = ", ")), call. = FALSE)
  }
  invisible(x)
}

#' Assert a value is a single non-negative number
#' @param x Value to test.
#' @param name Name to use in the error message.
#' @keywords internal
assert_non_negative_number <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    stop(sprintf("`%s` must be a single non-negative number, got: %s",
                 name, paste(x, collapse = ", ")), call. = FALSE)
  }
  invisible(x)
}

#' Assert a value is a single positive integer
#' @param x Value to test.
#' @param name Name to use in the error message.
#' @keywords internal
assert_positive_integer <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      x <= 0 || x != floor(x)) {
    stop(sprintf("`%s` must be a single positive integer, got: %s",
                 name, paste(x, collapse = ", ")), call. = FALSE)
  }
  invisible(as.integer(x))
}

#' Assert a value is a single logical
#' @param x Value to test.
#' @param name Name to use in the error message.
#' @keywords internal
assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE, got: %s",
                 name, paste(x, collapse = ", ")), call. = FALSE)
  }
  invisible(x)
}

#' Assert a value is a non-empty character string
#' @param x Value to test.
#' @param name Name to use in the error message.
#' @keywords internal
assert_string <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || nchar(x) == 0L) {
    stop(sprintf("`%s` must be a non-empty character string, got: %s",
                 name, paste(x, collapse = ", ")), call. = FALSE)
  }
  invisible(x)
}

# ── Probability vector validator ─────────────────────────────────────────────

#' Assert a vector is a valid probability distribution
#' @param probs Numeric vector. Must be length 3, all non-negative, sum to 1.
#' @param name Name to use in the error message.
#' @param tol Tolerance for sum-to-one check.
#' @keywords internal
assert_probability_vector <- function(probs,
                                      name = deparse(substitute(probs)),
                                      tol = 1e-6) {
  if (!is.numeric(probs) || length(probs) != 3L) {
    stop(sprintf("`%s` must be a numeric vector of length 3.", name),
         call. = FALSE)
  }
  if (any(is.na(probs)) || any(probs < 0)) {
    stop(sprintf("`%s` must contain non-negative values.", name),
         call. = FALSE)
  }
  if (abs(sum(probs) - 1) > tol) {
    stop(sprintf("`%s` must sum to 1 (got %.6f).", name, sum(probs)),
         call. = FALSE)
  }
  invisible(probs)
}

# ── Service parameter validator ──────────────────────────────────────────────

#' Assert service_params has valid structure for all 3 urgency levels
#' @param service_params List with keys "1", "2", "3". Each element must be a
#'   list with positive numeric \code{mean} and \code{sd}.
#' @keywords internal
assert_service_params <- function(service_params) {
  if (!is.list(service_params)) {
    stop("`service_params` must be a list.", call. = FALSE)
  }
  for (lvl in c("1", "2", "3")) {
    p <- service_params[[lvl]]
    if (is.null(p)) {
      stop(sprintf("`service_params` is missing entry for urgency level %s.", lvl),
           call. = FALSE)
    }
    if (!is.list(p) || is.null(p$mean) || is.null(p$sd)) {
      stop(sprintf("`service_params[[\"%s\"]]` must be a list with `mean` and `sd`.", lvl),
           call. = FALSE)
    }
    assert_positive_number(p$mean,
                           sprintf("service_params[[\"%s\"]]$mean", lvl))
    assert_non_negative_number(p$sd,
                               sprintf("service_params[[\"%s\"]]$sd",  lvl))
  }
  invisible(service_params)
}

# ── Top-level SimConfig validator ────────────────────────────────────────────

#' Validate a SimConfig object
#'
#' Called automatically by \code{\link{new_sim_config}}. Can also be called
#' directly to re-validate a config that has been manually modified.
#'
#' @param config A \code{SimConfig} object created by \code{new_sim_config}.
#' @return The config object invisibly, or an informative error.
#' @export
validate_sim_config <- function(config) {
  if (!inherits(config, "SimConfig")) {
    stop("`config` must be a SimConfig object created by `new_sim_config()`.",
         call. = FALSE)
  }
  assert_positive_number(config$arrival_rate,  "arrival_rate")
  assert_positive_integer(config$n_doctors,    "n_doctors")
  assert_positive_integer(config$n_nurses,     "n_nurses")
  assert_positive_number(config$sim_duration,  "sim_duration")
  assert_probability_vector(config$urgency_probs, "urgency_probs")
  assert_service_params(config$service_params)
  if (!is.null(config$seed)) {
    if (!is.numeric(config$seed) || length(config$seed) != 1L ||
        is.na(config$seed)) {
      stop("`seed` must be a single integer or NULL.", call. = FALSE)
    }
  }
  invisible(config)
}
