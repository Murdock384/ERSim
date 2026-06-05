#' Patient
#'
#' An R6 class representing a patient in the emergency room simulation.
#' Tracks arrival, urgency, service timing, and wait time.
#'
#' @param id Character. Unique patient identifier, e.g. "P00001".
#' @param arrival_time Numeric. Arrival time in simulation minutes.
#' @param urgency_level Integer. 1 = Critical, 2 = Urgent, 3 = Standard.
#' @param service_time Numeric. Sampled treatment duration in minutes.
#' @importFrom R6 R6Class
#' @export
Patient <- R6::R6Class(
  classname = "Patient",
  cloneable  = FALSE,

  public = list(

    #' @field id Character. Unique patient identifier, e.g. "P00001".
    id = NULL,

    #' @field arrival_time Numeric. Simulation time (minutes) when patient arrived.
    arrival_time = NULL,

    #' @field urgency_level Integer. 1 = Critical, 2 = Urgent, 3 = Standard.
    urgency_level = NULL,

    #' @field service_time Numeric. Duration (minutes) of treatment.
    service_time = NULL,

    #' @field start_service_time Numeric or NA. Simulation time treatment began.
    start_service_time = NA_real_,

    #' @field end_service_time Numeric or NA. Simulation time treatment ended.
    end_service_time = NA_real_,

    #' @field assigned_resource_id Character or NA. ID of the resource treating
    #'   this patient.
    assigned_resource_id = NA_character_,

    #' Create a new Patient
    #'
    #' @param id Character. Unique identifier.
    #' @param arrival_time Numeric. Arrival time in simulation minutes.
    #' @param urgency_level Integer. 1, 2, or 3.
    #' @param service_time Numeric. Sampled treatment duration in minutes.
    initialize = function(id, arrival_time, urgency_level, service_time) {
      assert_string(id, "id")
      assert_non_negative_number(arrival_time, "arrival_time")
      if (!urgency_level %in% c(1L, 2L, 3L)) {
        stop("`urgency_level` must be 1 (Critical), 2 (Urgent), or 3 (Standard).",
             call. = FALSE)
      }
      assert_positive_number(service_time, "service_time")

      self$id             <- id
      self$arrival_time   <- arrival_time
      self$urgency_level  <- as.integer(urgency_level)
      self$service_time   <- service_time
    },

    #' Compute wait time
    #'
    #' @return Numeric wait time in minutes, or NA if service has not started.
    get_wait_time = function() {
      if (is.na(self$start_service_time)) return(NA_real_)
      self$start_service_time - self$arrival_time
    },

    #' Return patient data as a named list (for logging)
    #'
    #' @return Named list with all patient fields.
    to_list = function() {
      list(
        id                   = self$id,
        arrival_time         = self$arrival_time,
        urgency_level        = self$urgency_level,
        urgency_label        = urgency_label(self$urgency_level),
        service_time         = self$service_time,
        start_service_time   = self$start_service_time,
        end_service_time     = self$end_service_time,
        wait_time            = self$get_wait_time(),
        assigned_resource_id = self$assigned_resource_id
      )
    },

    #' @description Print a concise summary of the patient.
    #' @param ... Ignored.
    print = function(...) {
      cat(sprintf(
        "<Patient %s | Urgency: %s | Arrived: %.1f min | Wait: %s>\n",
        self$id,
        urgency_label(self$urgency_level),
        self$arrival_time,
        if (is.na(self$get_wait_time())) "pending"
        else sprintf("%.1f min", self$get_wait_time())
      ))
      invisible(self)
    }
  )
)
