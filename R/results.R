#' @title SimResults — Simulation Results S3 Class
#' @description S3 class holding the output of a completed simulation run.

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

