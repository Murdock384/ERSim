#' Resource
#'
#' An R6 class representing a single medical staff member. Tracks availability,
#' the patient currently being treated, and cumulative busy time for utilisation
#' reporting.
#'
#' @param id Character. Unique resource identifier, e.g. "Doctor-1".
#' @param role Character. "doctor" or "nurse".
#' @importFrom R6 R6Class
#' @export
Resource <- R6::R6Class(
  classname = "Resource",
  cloneable  = FALSE,

  private = list(
    .busy            = FALSE,
    .current_patient = NULL,
    .total_busy_time = 0
  ),

  active = list(
    #' @field busy Logical. TRUE if this resource is currently treating a patient.
    busy = function() private$.busy,

    #' @field current_patient The \code{Patient} being treated, or \code{NULL}.
    current_patient = function() private$.current_patient,

    #' @field total_busy_time Numeric. Cumulative minutes spent treating patients.
    total_busy_time = function() private$.total_busy_time
  ),

  public = list(

    #' @field id Character. Unique resource identifier, e.g. "Doctor-1".
    id = NULL,

    #' @field role Character. "doctor" or "nurse".
    role = NULL,

    #' Create a new Resource
    #'
    #' @param id Character. Unique identifier.
    #' @param role Character. "doctor" or "nurse".
    initialize = function(id, role = c("doctor", "nurse")) {
      assert_string(id, "id")
      role <- match.arg(role)
      self$id   <- id
      self$role <- role
    },

    #' Check if this resource is available (not busy)
    #'
    #' @return Logical.
    is_available = function() !private$.busy,

    #' Assign a patient to this resource
    #'
    #' @param patient A \code{Patient} R6 object.
    #' @param current_time Numeric. Simulation time of assignment.
    assign_patient = function(patient, current_time) {
      if (private$.busy) {
        stop(sprintf("Resource '%s' is already busy.", self$id), call. = FALSE)
      }
      if (!inherits(patient, "Patient")) {
        stop("`patient` must be a Patient R6 object.", call. = FALSE)
      }
      private$.busy            <- TRUE
      private$.current_patient <- patient
      patient$start_service_time   <- current_time
      patient$assigned_resource_id <- self$id
      invisible(self)
    },

    #' Release the current patient (treatment complete)
    #'
    #' @param current_time Numeric. Simulation time of release.
    #' @return The \code{Patient} that was released.
    release_patient = function(current_time) {
      if (!private$.busy) {
        stop(sprintf("Resource '%s' is not busy.", self$id), call. = FALSE)
      }
      patient                    <- private$.current_patient
      patient$end_service_time   <- current_time
      private$.total_busy_time   <- private$.total_busy_time +
                                      (current_time - patient$start_service_time)
      private$.busy              <- FALSE
      private$.current_patient   <- NULL
      patient
    },

    #' Compute utilisation fraction over the simulation duration
    #'
    #' @param sim_duration Numeric. Total simulation duration in minutes.
    #' @return Numeric between 0 and 1.
    utilisation = function(sim_duration) {
      assert_positive_number(sim_duration, "sim_duration")
      min(private$.total_busy_time / sim_duration, 1)
    },

    #' @description Print a summary of the resource state.
    #' @param ... Ignored.
    print = function(...) {
      cat(sprintf(
        "<Resource %s [%s] | %s | Busy time: %.1f min>\n",
        self$id,
        self$role,
        if (private$.busy)
          paste0("Treating: ", private$.current_patient$id)
        else "Available",
        private$.total_busy_time
      ))
      invisible(self)
    }
  )
)

#' Build a list of Resource objects for a given role
#'
#' @param n Integer. Number of resources to create.
#' @param role Character. "doctor" or "nurse".
#' @return List of \code{Resource} R6 objects.
#' @keywords internal
make_resources <- function(n, role = c("doctor", "nurse")) {
  role <- match.arg(role)
  lapply(seq_len(n), function(i) {
    Resource$new(
      id   = sprintf("%s-%d", tools::toTitleCase(role), i),
      role = role
    )
  })
}
