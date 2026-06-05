library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
# library(ERsim) # Commented out for dev mode - sources R files directly instead

# Load R files
ersim_r_dir <- 'c:/Users/akhil/Downloads/Advanced R Project (1)/Advanced R Project/ERsim/R'
source(file.path(ersim_r_dir, 'validators.R'))
source(file.path(ersim_r_dir, 'utils.R'))
source(file.path(ersim_r_dir, 'config.R'))
source(file.path(ersim_r_dir, 'patient.R'))
source(file.path(ersim_r_dir, 'queue.R'))
source(file.path(ersim_r_dir, 'resource.R'))
source(file.path(ersim_r_dir, 'results.R'))
source(file.path(ersim_r_dir, 'stats.R'))

# Skip simulation.R and RcppExports.R for frontend dev - they require C++ compilation
# Simulation will not be functional, but UI is fully editable

# Urgency colours shared with the package
URGENCY_COLOURS <- c(Critical = "#e74c3c", Urgent = "#f39c12", Standard = "#2ecc71")

server <- function(input, output, session) {

  # ── Reactive state ──────────────────────────────────────────────────────
  current_config  <- reactiveVal(NULL)
  current_results <- reactiveVal(NULL)
  scenarios       <- reactiveVal(list())   # named list of SimResults

  # ── Tab 1: Configure ─────────────────────────────────────────────────────

  output$urgency_sum_warn <- renderText({
    total <- input$prob_critical + input$prob_urgent + input$prob_standard
    if (abs(total - 100) > 0.5) {
      paste0("Warning: probabilities sum to ", total, "% (must equal 100%)")
    } else {
      ""
    }
  })

  observeEvent(input$btn_apply_config, {
    total <- input$prob_critical + input$prob_urgent + input$prob_standard
    if (abs(total - 100) > 0.5) {
      showNotification("Urgency probabilities must sum to 100%.", type = "error")
      return()
    }

    seed_val <- if (is.na(input$seed) || input$seed == "") NULL else as.integer(input$seed)

    cfg <- tryCatch(
      new_sim_config(
        arrival_rate   = input$arrival_rate,
        n_doctors      = input$n_doctors,
        n_nurses       = input$n_nurses,
        sim_duration   = input$sim_duration,
        urgency_probs  = c(input$prob_critical,
                           input$prob_urgent,
                           input$prob_standard) / 100,
        service_params = list(
          "1" = list(mean = input$svc_crit_mean, sd = input$svc_crit_sd),
          "2" = list(mean = input$svc_urg_mean,  sd = input$svc_urg_sd),
          "3" = list(mean = input$svc_std_mean,  sd = input$svc_std_sd)
        ),
        seed = seed_val
      ),
      error = function(e) {
        showNotification(paste("Config error:", e$message), type = "error")
        NULL
      }
    )

    if (!is.null(cfg)) {
      current_config(cfg)
      showNotification("Configuration applied.", type = "message")
    }
  })

  output$config_preview <- renderPrint({
    cfg <- current_config()
    if (is.null(cfg)) cat("(No configuration applied yet. Click 'Apply Configuration'.)")
    else print(cfg)
  })

  # ── Tab 2: Run & Monitor ──────────────────────────────────────────────────

  sim_status <- reactiveVal("")
  output$sim_status <- renderText(sim_status())

  observeEvent(input$btn_run, {
    cfg <- current_config()
    if (is.null(cfg)) {
      showNotification("Please apply a configuration first.", type = "warning")
      return()
    }

    # Check if SimulationEngine is available
    if (!exists("SimulationEngine")) {
      showNotification(
        "Simulation not available - C++ code not compiled. For frontend dev only.",
        type = "error"
      )
      return()
    }

    sim_status("Running simulation...")

    results <- tryCatch({
      engine <- SimulationEngine$new(cfg)
      res <- engine$run()
      sim_status("Simulation complete.")
      res
    }, error = function(e) {
      sim_status("Simulation failed.")
      showNotification(paste("Simulation error:", e$message), type = "error")
      NULL
    })

    if (!is.null(results)) {
      current_results(results)
      showNotification(
        sprintf("Simulation complete. %d patients served.", results$kpis$n_patients),
        type = "message"
      )
    }
  })

  output$vbox_n_patients <- renderValueBox({
    res <- current_results()
    valueBox(
      value    = if (is.null(res)) "—" else res$kpis$n_patients,
      subtitle = "Patients Served",
      icon     = icon("user-injured"),
      color    = "blue"
    )
  })

  output$vbox_not_served <- renderValueBox({
    res <- current_results()
    n   <- if (is.null(res)) "—" else sum(res$patient_log$status == "In Queue")
    valueBox(
      value    = n,
      subtitle = "Patients Not Served",
      icon     = icon("user-times"),
      color    = "red"
    )
  })

  output$vbox_mean_wait <- renderValueBox({
    res <- current_results()
    valueBox(
      value    = if (is.null(res)) "—" else sprintf("%.1f min", res$kpis$mean_wait),
      subtitle = "Mean Wait Time per Served Patient",
      icon     = icon("clock"),
      color    = "orange"
    )
  })

  output$vbox_throughput <- renderValueBox({
    res <- current_results()
    valueBox(
      value    = if (is.null(res)) "—" else sprintf("%.1f/hr", res$kpis$throughput_per_hour),
      subtitle = "Throughput",
      icon     = icon("tachometer-alt"),
      color    = "green"
    )
  })

  output$run_event_table <- renderDT({
    res <- current_results()
    req(res)
    log <- res$patient_log
    log$urgency_label <- factor(log$urgency_label,
                                levels = c("Critical", "Urgent", "Standard"))
    # Urgency display: show escalation trace if tier changed while waiting
    log$urgency_display <- ifelse(
      !is.na(log$escalated_urgency),
      paste0(log$escalated_urgency, " \u2191"),
      as.character(log$urgency_label)
    )
    # Build a display column for the Discharged column:
    #   "Service In Progress" → treatment was running when sim ended
    #   formatted number    → patient fully discharged
    #   blank (NA)          → patient never reached a resource
    log$discharged_display <- ifelse(
      log$status == "In Progress", "Service In Progress",
      ifelse(is.na(log$end_service_time), NA_character_,
             formatC(log$end_service_time, format = "f", digits = 2))
    )
    datatable(
      log[, c("id", "urgency_display", "arrival_time",
              "wait_time", "service_time", "discharged_display")],
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      colnames = c("Patient", "Urgency", "Arrived (min)",
                   "Wait (min)", "Service (min)", "Discharged (min)")
    ) |>
      formatRound(c("arrival_time", "wait_time", "service_time"), digits = 2)
  })

  # ── Tab 3: Results ────────────────────────────────────────────────────────

  output$res_n_patients <- renderValueBox({
    res <- current_results()
    valueBox(if (is.null(res)) "—" else res$kpis$n_patients,
             "Patients Served", icon("user-injured"), color = "blue")
  })
  output$res_not_served <- renderValueBox({
    res <- current_results()
    n   <- if (is.null(res)) "—" else sum(res$patient_log$status == "In Queue")
    valueBox(n, "Patients Not Served", icon("user-times"), color = "red")
  })
  output$res_mean_wait <- renderValueBox({
    res <- current_results()
    valueBox(if (is.null(res)) "—" else sprintf("%.1f min", res$kpis$mean_wait),
             "Mean Wait (Served Patients)", icon("clock"), color = "orange")
  })
  output$res_p95_wait <- renderValueBox({
    res <- current_results()
    valueBox(if (is.null(res)) "—" else sprintf("%.1f min", res$kpis$p95_wait),
             "95th Pct Wait", icon("exclamation-triangle"), color = "red")
  })
  output$res_throughput <- renderValueBox({
    res <- current_results()
    valueBox(if (is.null(res)) "—" else sprintf("%.1f/hr", res$kpis$throughput_per_hour),
             "Throughput", icon("tachometer-alt"), color = "green")
  })

  output$plot_wait_hist <- renderPlot({
    res <- current_results()
    req(res)
    pl <- res$patient_log
    pl <- pl[!is.na(pl$wait_time), ]
    pl$urgency_label <- factor(pl$urgency_label,
                               levels = c("Critical", "Urgent", "Standard"))
    ggplot(pl, aes(x = wait_time, fill = urgency_label)) +
      geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
      scale_fill_manual(values = URGENCY_COLOURS) +
      labs(x = "Wait Time (min)", y = "Count", fill = "Urgency") +
      theme_minimal(base_size = 13)
  })

  output$plot_queue_time <- renderPlot({
    res <- current_results()
    req(res)
    ts  <- res$queue_ts_analysis
    df  <- ts$rolling_mean
    lbl <- sprintf("Trend: %s  |  slope %.3f pts/min  |  peak %d patients at t=%.1f min",
                   ts$trend_label, ts$trend_slope, ts$peak_length, ts$peak_time)
    ggplot(df, aes(x = time)) +
      geom_line(aes(y = queue_length), colour = "#3498db",
                linewidth = 0.5, alpha = 0.45) +
      geom_line(aes(y = rolling_mean), colour = "#e74c3c",
                linewidth = 1.0, na.rm = TRUE) +
      labs(x = "Time (min)", y = "Patients in Queue",
           caption = paste0("Blue = raw queue length  |  Red = rolling mean  |  ", lbl)) +
      theme_minimal(base_size = 13)
  })

  output$plot_queue_acf <- renderPlot({
    res <- current_results()
    req(res)
    ts       <- res$queue_ts_analysis
    acf_vals <- ts$acf_values
    n_events <- nrow(res$queue_over_time)
    sig_bound <- 1.96 / sqrt(max(n_events, 1L))
    df_acf <- data.frame(
      lag = factor(names(acf_vals), levels = names(acf_vals)),
      acf = as.numeric(acf_vals)
    )
    ggplot(df_acf, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", fill = "#9b59b6", alpha = 0.8) +
      geom_hline(yintercept =  sig_bound, linetype = "dashed",
                 colour = "#e74c3c", linewidth = 0.6) +
      geom_hline(yintercept = -sig_bound, linetype = "dashed",
                 colour = "#e74c3c", linewidth = 0.6) +
      geom_hline(yintercept = 0, colour = "black") +
      labs(x = "Lag", y = "Autocorrelation",
           caption = "Dashed red = 95% significance bounds. Bars above bound indicate persistent queue build-up.") +
      theme_minimal(base_size = 13)
  })

  output$plot_wait_urgency <- renderPlot({
    res <- current_results()
    req(res)
    df <- res$kpis$wait_by_urgency_df
    df$urgency_label <- factor(df$urgency_label,
                               levels = c("Critical", "Urgent", "Standard"))
    ggplot(df, aes(x = urgency_label, y = mean_wait, fill = urgency_label)) +
      geom_bar(stat = "identity", alpha = 0.85) +
      geom_errorbar(aes(ymin = mean_wait, ymax = p95_wait), width = 0.2) +
      scale_fill_manual(values = URGENCY_COLOURS) +
      labs(x = "Urgency", y = "Wait Time (min)",
           caption = "Bar = mean; error bar top = 95th percentile") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$plot_utilisation <- renderPlot({
    res <- current_results()
    req(res)
    rl <- res$resource_log
    ggplot(rl, aes(x = reorder(id, utilisation),
                   y = utilisation * 100, fill = role)) +
      geom_bar(stat = "identity", alpha = 0.85) +
      coord_flip() +
      scale_fill_manual(values = c(doctor = "#2980b9", nurse = "#27ae60")) +
      labs(x = NULL, y = "Utilisation (%)", fill = "Role") +
      theme_minimal(base_size = 13)
  })

  output$results_table <- renderDT({
    res <- current_results()
    req(res)
    datatable(
      res$patient_log,
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) |> formatRound(c("arrival_time", "wait_time", "service_time",
                        "start_service_time", "end_service_time"), digits = 2)
  })

  # ── Tab 4: Compare Scenarios ──────────────────────────────────────────────
  # (Remaining comparison code follows in original...)
}
