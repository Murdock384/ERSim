#' @title PriorityQueue R6 Class
#' @description A priority queue that orders patients by urgency level
#'   (1 = Critical first) and, within the same urgency, by arrival time (FIFO).
#' @importFrom R6 R6Class

#' PriorityQueue
#'
#' An R6 class implementing a priority queue for \code{Patient} objects.
#' Patients are dequeued by effective urgency tier, then by when they entered
#' that tier, then original urgency and arrival time. This preserves FIFO
#' behavior among patients who are currently in the same urgency tier.
#'
#' @export
PriorityQueue <- R6::R6Class(
  classname = "PriorityQueue",
  cloneable  = FALSE,

  private = list(
    .queue = NULL, 
    tier_entry_time = function(patient, eff_tier, base, rates) {
      original_tier <- patient$urgency_level
      if (eff_tier >= original_tier || rates[[as.character(original_tier)]] == 0) {
        return(patient$arrival_time)
      }

      patient$arrival_time +
        (base[[as.character(original_tier)]] - base[[as.character(eff_tier)]]) /
        rates[[as.character(original_tier)]]
    }
  ),

  active = list(
    #' @field size Integer. Number of patients currently in the queue.
    size = function() length(private$.queue)
  ),

  public = list(

    #' Create a new empty PriorityQueue
    initialize = function() {
      private$.queue <- list()
    },

    #' Add a patient to the queue
    #'
    #' The queue is re-sorted after insertion using a stable sort so that
    #' arrival-time ordering is preserved within each urgency band.
    #'
    #' @param patient A \code{Patient} R6 object.
    enqueue = function(patient) {
      if (!inherits(patient, "Patient")) {
        stop("`patient` must be a Patient R6 object.", call. = FALSE)
      }
      private$.queue <- c(private$.queue, list(patient))
      ord <- order(
        vapply(private$.queue, function(p) p$urgency_level, integer(1L)),
        vapply(private$.queue, function(p) p$arrival_time,  double(1L))
      )
      private$.queue <- private$.queue[ord]
      invisible(self)
    },

    #' Remove and return the highest-priority patient
    #'
    #' Dynamic priority: effective priority = base - rate * wait_time (floor 0).
    #' Urgent and Standard patients can age into higher effective tiers. Within
    #' an effective tier, patients are ordered by the time they entered that
    #' tier, then original urgency, then arrival time.
    #'
    #' @param current_time Numeric. The simulation clock time at the moment of
    #'   selection, used to compute how long each patient has been waiting.
    #' @return The next \code{Patient} R6 object, or \code{NULL} if empty.
    dequeue = function(current_time = 0) {
      if (self$is_empty()) return(NULL)

      # Base priorities and decay rates indexed by urgency_level (1/2/3).
      # Critical never decays (rate=0) — always highest priority.
      # Urgent and Standard decay at the same rate (1.0/min):
      #   Urgent   (20): reaches Critical threshold (10) after 10 min waiting
      #   Standard (40): reaches Urgent threshold   (20) after 20 min waiting
      #                  reaches Critical threshold (10) after 30 min waiting
      BASE  <- c("1" = 10.0, "2" = 20.0, "3" = 40.0)
      RATES <- c("1" =  0.0, "2" =  1.0, "3" =  1.0)

      # Effective priority for each queued patient at current_time
      eff <- vapply(private$.queue, function(p) {
        key  <- as.character(p$urgency_level)
        wait <- max(0.0, current_time - p$arrival_time)
        max(0.0, BASE[[key]] - RATES[[key]] * wait)
      }, double(1L))

      eff_tier <- as.integer(ifelse(eff <= 10.0, 1L,
                             ifelse(eff <= 20.0, 2L, 3L)))
      urgency  <- vapply(private$.queue, function(p) p$urgency_level, integer(1L))
      arrivals <- vapply(private$.queue, function(p) p$arrival_time, double(1L))
      tier_entry <- mapply(
        function(p, tier) private$tier_entry_time(p, tier, BASE, RATES),
        private$.queue,
        eff_tier
      )

      idx <- order(eff_tier, tier_entry, urgency, arrivals)[1L]

      patient        <- private$.queue[[idx]]
      private$.queue <- private$.queue[-idx]
      patient
    },

    #' Peek at the highest-priority patient without removing it
    #'
    #' @return The next \code{Patient} R6 object, or \code{NULL} if empty.
    peek = function() {
      if (self$is_empty()) return(NULL)
      private$.queue[[1L]]
    },

    #' Check whether the queue is empty
    #'
    #' @return Logical.
    is_empty = function() {
      length(private$.queue) == 0L
    },

    #' Dequeue the next patient a doctor should serve
    #'
    #' Selects the highest-priority patient whose effective priority has reached
    #' the Urgent/Critical threshold (eff \eqn{\le} 20): this covers Critical,
    #' Urgent, and Standard patients that have escalated through priority decay.
    #' Returns \code{NULL} if no such patient is waiting (doctor stays idle).
    #'
    #' @param current_time Numeric. Simulation clock time for decay computation.
    #' @return A \code{Patient} R6 object, or \code{NULL}.
    dequeue_for_doctor = function(current_time = 0) {
      if (self$is_empty()) return(NULL)

      BASE  <- c("1" = 10.0, "2" = 20.0, "3" = 40.0)
      RATES <- c("1" =  0.0, "2" =  1.0, "3" =  1.0)

      eff <- vapply(private$.queue, function(p) {
        key  <- as.character(p$urgency_level)
        wait <- max(0.0, current_time - p$arrival_time)
        max(0.0, BASE[[key]] - RATES[[key]] * wait)
      }, double(1L))

      # Eligible: Critical + Urgent + Standard that has decayed to eff <= 20
      eligible <- which(eff <= 20.0)
      if (length(eligible) == 0L) return(NULL)

      eff_sub      <- eff[eligible]
      eff_tier_sub <- as.integer(ifelse(eff_sub <= 10.0, 1L, 2L))
      urgency_sub  <- vapply(private$.queue[eligible],
                             function(p) p$urgency_level, integer(1L))
      arrivals_sub <- vapply(private$.queue[eligible],
                             function(p) p$arrival_time, double(1L))
      tier_entry_sub <- mapply(
        function(p, tier) private$tier_entry_time(p, tier, BASE, RATES),
        private$.queue[eligible],
        eff_tier_sub
      )
      idx <- eligible[order(eff_tier_sub, tier_entry_sub, urgency_sub, arrivals_sub)[1L]]

      patient        <- private$.queue[[idx]]
      private$.queue <- private$.queue[-idx]
      patient
    },

    #' Dequeue the next patient a nurse should serve
    #'
    #' Selects the highest-priority Standard (urgency level 3) patient whose
    #' effective priority is still in the Standard range (eff > 20). Once a
    #' Standard patient has waited long enough to escalate (eff \eqn{\le} 20)
    #' they are no longer eligible for nurses and must wait for a doctor.
    #' Returns \code{NULL} if no eligible patient is waiting (nurse stays idle).
    #'
    #' @param current_time Numeric. Simulation clock time for decay computation.
    #' @return A \code{Patient} R6 object, or \code{NULL}.
    dequeue_for_nurse = function(current_time = 0) {
      if (self$is_empty()) return(NULL)

      BASE  <- c("1" = 10.0, "2" = 20.0, "3" = 40.0)
      RATES <- c("1" =  0.0, "2" =  1.0, "3" =  1.0)

      eff <- vapply(private$.queue, function(p) {
        key  <- as.character(p$urgency_level)
        wait <- max(0.0, current_time - p$arrival_time)
        max(0.0, BASE[[key]] - RATES[[key]] * wait)
      }, double(1L))

      ul       <- vapply(private$.queue, function(p) p$urgency_level, integer(1L))
      eligible <- which(ul == 3L & eff > 20.0)
      if (length(eligible) == 0L) return(NULL)

      eff_sub    <- eff[eligible]
      min_eff    <- min(eff_sub)
      candidates <- eligible[eff_sub == min_eff]
      if (length(candidates) > 1L) {
        arr        <- vapply(private$.queue[candidates],
                             function(p) p$arrival_time, double(1L))
        candidates <- candidates[which.min(arr)]
      }

      patient        <- private$.queue[[candidates]]
      private$.queue <- private$.queue[-candidates]
      patient
    },

    #' Check whether a specific patient is still waiting in the queue
    #'
    #' @param patient_id Character. The patient ID to look up.
    #' @return Logical.
    has_patient = function(patient_id) {
      any(vapply(private$.queue, function(p) p$id == patient_id, logical(1L)))
    },

    #' Remove and return a specific patient by ID
    #'
    #' Used by the escalation handler to pull a named patient from the queue
    #' and assign them directly to a doctor.
    #'
    #' @param patient_id Character. The patient ID to dequeue.
    #' @return The \code{Patient} R6 object, or \code{NULL} if not found.
    dequeue_patient = function(patient_id) {
      idx <- which(vapply(private$.queue,
                          function(p) p$id == patient_id, logical(1L)))
      if (length(idx) == 0L) return(NULL)
      patient        <- private$.queue[[idx[1L]]]
      private$.queue <- private$.queue[-idx[1L]]
      patient
    },

    #' Return a snapshot data.frame of all patients currently waiting
    #'
    #' @return A data.frame with columns: position, id, urgency_level,
    #'   urgency_label, arrival_time.
    snapshot = function() {
      if (self$is_empty()) {
        return(data.frame(
          position      = integer(0),
          id            = character(0),
          urgency_level = integer(0),
          urgency_label = character(0),
          arrival_time  = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        position      = seq_along(private$.queue),
        id            = vapply(private$.queue, function(p) p$id, character(1L)),
        urgency_level = vapply(private$.queue, function(p) p$urgency_level, integer(1L)),
        urgency_label = vapply(private$.queue, function(p) urgency_label(p$urgency_level), character(1L)),
        arrival_time  = vapply(private$.queue, function(p) p$arrival_time, double(1L)),
        stringsAsFactors = FALSE
      )
    },

    #' @description Print a summary of the queue state.
    print = function(...) {
      cat(sprintf("<PriorityQueue | %d patient(s) waiting>\n", self$size))
      if (!self$is_empty()) {
        next_p <- self$peek()
        cat(sprintf("  Next: %s (%s, arrived %.1f min)\n",
                    next_p$id,
                    urgency_label(next_p$urgency_level),
                    next_p$arrival_time))
      }
      invisible(self)
    }
  )
)
