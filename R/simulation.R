#' @title SimulationEngine R6 Class
#' @description Discrete event simulation engine for the ER model. Orchestrates
#'   patient arrivals, queue management, resource assignment, and departure
#'   events. The inner event-processing loop is delegated to an Rcpp function
#'   (\code{process_events_cpp}) for performance.
#' @importFrom R6 R6Class

#' SimulationEngine
#'
#' The central R6 class for running ER simulations. Accepts a
#' \code{\link{new_sim_config}} object, runs a discrete event simulation, and
#' returns a \code{SimResults} S3 object.
#'
#' @export
SimulationEngine <- R6::R6Class(
  classname = "SimulationEngine",
  cloneable  = FALSE,

  private = list(
    config         = NULL,
    queue          = NULL,
    doctors        = NULL,
    nurses         = NULL,
    all_resources  = NULL,
    event_list     = NULL,
    patient_store  = NULL,
    patient_counter = 0L,
    current_time   = 0,
    results        = NULL,

    # ── Private helpers ────────────────────────────────────────────────────

    # Find first available doctor
    find_available_doctor = function() {
      for (r in private$doctors) {
        if (r$is_available()) return(r)
      }
      NULL
    },

    # Find first available nurse
    find_available_nurse = function() {
      for (r in private$nurses) {
        if (r$is_available()) return(r)
      }
      NULL
    },

    # Schedule an event by adding a row to event_list
    schedule_event = function(type, time, patient_id = NA_character_,
                              urgency_level = NA_integer_) {
      private$event_list <- rbind(
        private$event_list,
        data.frame(
          type          = type,
          time          = time,
          patient_id    = patient_id,
          urgency_level = as.integer(urgency_level),
          stringsAsFactors = FALSE
        )
      )
    },

    # Process a single ARRIVAL event
    process_arrival = function(event) {
      private$patient_counter <- private$patient_counter + 1L
      pid <- make_patient_id(private$patient_counter)

      urgency <- as.integer(event$urgency_level)
      svc_time <- draw_service_time(urgency, private$config$service_params)

      patient <- Patient$new(
        id            = pid,
        arrival_time  = event$time,
        urgency_level = urgency,
        service_time  = svc_time
      )
      private$patient_store[[pid]] <- patient

      # Role-based assignment: Critical/Urgent -> doctor only; Standard -> nurse only
      if (urgency %in% c(1L, 2L)) {
        resource <- private$find_available_doctor()
      } else {
        resource <- private$find_available_nurse()
      }
      if (!is.null(resource)) {
        # Serve immediately
        private$serve_patient(patient, resource, event$time)
      } else {
        # Wait in queue
        private$queue$enqueue(patient)
        # Standard patients escalate after 20 min wait (eff: 40 -> 20 at rate
        # 1/min). Schedule an ESCALATION event so a free doctor can pick them
        # up as soon as they cross the Urgent threshold, without waiting for
        # a doctor departure event to fire first.
        # Only schedule if the escalation time falls within the simulation
        # window — events past sim_duration cause negative busy-time on the
        # resource (assign_time > clamped departure_time).
        if (urgency == 3L) {
          esc_time <- event$time + 20.0
          if (esc_time < private$config$sim_duration) {
            private$schedule_event("ESCALATION", esc_time, pid, urgency)
          }
        }
      }
    },

    # Assign a patient to a resource and schedule their departure
    serve_patient = function(patient, resource, current_time) {
      resource$assign_patient(patient, current_time)
      departure_time <- current_time + patient$service_time
      # Clamp departure to sim_duration so the resource is held busy until
      # the boundary and no further patients are incorrectly assigned to it.
      clamped_time <- min(departure_time, private$config$sim_duration)
      private$schedule_event("DEPARTURE", clamped_time,
                             patient$id, patient$urgency_level)
    },

    # Process a single DEPARTURE event
    process_departure = function(event) {
      pid <- event$patient_id
      patient <- private$patient_store[[pid]]

      # Find the resource treating this patient
      treating_resource <- NULL
      for (r in private$all_resources) {
        if (!r$is_available() &&
            !is.null(r$current_patient) &&
            r$current_patient$id == pid) {
          treating_resource <- r
          break
        }
      }
      if (!is.null(treating_resource)) {
        treating_resource$release_patient(event$time)
      }

      # Only serve next patient if simulation has not yet reached its end.
      # At the boundary (event$time == sim_duration) the shift is over —
      # remaining queued patients are left unserved with NA wait/service times.
      if (!private$queue$is_empty() &&
          event$time < private$config$sim_duration &&
          !is.null(treating_resource)) {
        # The freed resource claims the next patient appropriate to its role
        if (treating_resource$role == "doctor") {
          next_patient <- private$queue$dequeue_for_doctor(event$time)
        } else {
          next_patient <- private$queue$dequeue_for_nurse(event$time)
        }
        if (!is.null(next_patient)) {
          private$serve_patient(next_patient, treating_resource, event$time)
        }
      }
    },

    # Handle an ESCALATION event: attempt to hand a long-waiting Standard
    # patient to a free doctor now that their effective priority has crossed
    # the Urgent threshold (eff <= 20 after 20 min wait).
    process_escalation = function(event) {
      pid <- event$patient_id
      # Safety guard: should not normally fire past sim end, but skip if so.
      if (event$time >= private$config$sim_duration) return(invisible(NULL))
      # Patient may already have been served by a nurse — nothing to do.
      if (!private$queue$has_patient(pid)) return(invisible(NULL))
      # Try to assign the highest-priority doctor-eligible patient. If none are
      # available the escalated patient stays in the queue; a future doctor
      # departure event will claim the next eligible patient.
      free_doctor <- private$find_available_doctor()
      if (is.null(free_doctor)) return(invisible(NULL))
      patient <- private$queue$dequeue_for_doctor(event$time)
      if (is.null(patient)) return(invisible(NULL))
      private$serve_patient(patient, free_doctor, event$time)
    },

    # Build the resource utilisation log after simulation completes
    build_resource_log = function() {
      rows <- lapply(private$all_resources, function(r) {
        data.frame(
          id          = r$id,
          role        = r$role,
          utilisation = r$utilisation(private$config$sim_duration),
          busy_time   = r$total_busy_time,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    },

    # Build the patient log data.frame from patient_store
    build_patient_log = function() {
      rows <- lapply(private$patient_store, function(p) p$to_list())
      df   <- do.call(rbind, lapply(rows, as.data.frame,
                                    stringsAsFactors = FALSE))
      # Coerce types
      df$arrival_time       <- as.numeric(df$arrival_time)
      df$service_time       <- as.numeric(df$service_time)
      df$start_service_time <- as.numeric(df$start_service_time)
      df$end_service_time   <- as.numeric(df$end_service_time)
      df$wait_time          <- as.numeric(df$wait_time)
      df$urgency_level      <- as.integer(df$urgency_level)

      # Classify each patient's outcome
      in_queue    <- is.na(df$start_service_time)
      in_progress <- !in_queue &
                     (df$start_service_time + df$service_time) > private$config$sim_duration
      df$status <- ifelse(in_queue, "In Queue",
                   ifelse(in_progress, "In Progress", "Discharged"))

      # Unserved patients: clear the pre-generated (never used) service time.
      df$service_time[in_queue] <- NA_real_

      # ── Escalation tracking ─────────────────────────────────────────────
      # Mirror the same BASE/RATES as PriorityQueue$dequeue() so the tier
      # shown here is consistent with how the queue actually ordered them.
      BASE  <- c("1" = 10.0, "2" = 20.0, "3" = 40.0)
      RATES <- c("1" =  0.0, "2" =  1.0, "3" =  1.0)

      # Reference time: when service started (served) or sim end (unserved)
      ref_time <- ifelse(!is.na(df$start_service_time),
                         df$start_service_time,
                         private$config$sim_duration)

      eff_priority <- mapply(function(ul, at, rt) {
        key  <- as.character(ul)
        wait <- max(0.0, rt - at)
        max(0.0, BASE[[key]] - RATES[[key]] * wait)
      }, df$urgency_level, df$arrival_time, ref_time)

      # Map effective priority back to tier using same thresholds as BASE
      eff_tier <- as.integer(ifelse(eff_priority <= 10, 1L,
                             ifelse(eff_priority <= 20, 2L, 3L)))

      # escalated_urgency: "Original → NewTier" if tier improved, else NA
      df$escalated_urgency <- ifelse(
        eff_tier < df$urgency_level,
        paste0(df$urgency_label, " \u2192 ",
               URGENCY_LABELS[as.character(eff_tier)]),
        NA_character_
      )

      df
    }
  ),

  public = list(

    #' Create a new SimulationEngine
    #'
    #' @param config A \code{SimConfig} object from \code{\link{new_sim_config}}.
    initialize = function(config) {
      validate_sim_config(config)
      private$config        <- config
      private$queue         <- PriorityQueue$new()
      private$doctors       <- make_resources(config$n_doctors, "doctor")
      private$nurses        <- make_resources(config$n_nurses,  "nurse")
      private$all_resources <- c(private$doctors, private$nurses)
      private$patient_store <- list()
      private$event_list    <- data.frame(
        type          = character(0),
        time          = numeric(0),
        patient_id    = character(0),
        urgency_level = integer(0),
        stringsAsFactors = FALSE
      )
      private$patient_counter <- 0L
      private$current_time    <- 0
    },

    #' Run the Simulation
    #'
    #' Generates all arrival events up-front using the Rcpp helper
    #' \code{generate_arrivals_cpp}, then processes them in chronological
    #' order alongside departure events via the main R6 event loop.
    #'
    #' @return A \code{SimResults} S3 object.
    run = function() {
      cfg      <- private$config
      if (!is.null(cfg$seed)) set.seed(cfg$seed)

      start_time <- proc.time()["elapsed"]

      # ── Generate arrival events via Rcpp ─────────────────────────────────
      arrivals_df <- generate_arrivals_cpp(
        arrival_rate  = cfg$arrival_rate,
        sim_duration  = cfg$sim_duration,
        urgency_probs = cfg$urgency_probs
      )

      # Seed initial event list with arrivals
      if (nrow(arrivals_df) > 0L) {
        private$event_list <- rbind(private$event_list, arrivals_df)
      }

      # ── Main event loop ───────────────────────────────────────────────────
      while (nrow(private$event_list) > 0L) {
        # Pop earliest event
        idx   <- which.min(private$event_list$time)
        event <- private$event_list[idx, ]
        private$event_list <- private$event_list[-idx, ]

        private$current_time <- event$time

        if (event$type == "ARRIVAL") {
          private$process_arrival(event)
        } else if (event$type == "DEPARTURE") {
          private$process_departure(event)
        } else if (event$type == "ESCALATION") {
          private$process_escalation(event)
        }
      }

      # ── Build results ─────────────────────────────────────────────────────
      patient_log       <- private$build_patient_log()
      kpis              <- compute_kpis(patient_log)
      queue_over_time   <- compute_queue_over_time(patient_log)
      queue_ts_analysis <- compute_queue_ts(queue_over_time)
      resource_log      <- private$build_resource_log()
      elapsed_sec       <- proc.time()["elapsed"] - start_time

      private$results <- new_sim_results(
        patient_log       = patient_log,
        kpis              = kpis,
        queue_over_time   = queue_over_time,
        queue_ts_analysis = queue_ts_analysis,
        resource_log      = resource_log,
        config            = cfg,
        elapsed_sec       = unname(elapsed_sec)
      )
      private$results
    },

    #' Retrieve the results from the last run
    #'
    #' @return A \code{SimResults} object, or \code{NULL} if \code{run()} has
    #'   not been called yet.
    get_results = function() private$results,

    #' @description Print engine state.
    print = function(...) {
      cfg <- private$config
      cat(sprintf(
        "<SimulationEngine | %.2f arr/min | %d doctors | %d nurses | %.0f min>\n",
        cfg$arrival_rate, cfg$n_doctors, cfg$n_nurses, cfg$sim_duration
      ))
      invisible(self)
    }
  )
)
