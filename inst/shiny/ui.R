library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

dashboardPage(
  skin = "black",

  dashboardHeader(title = "ERsim"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Configure",         tabName = "configure",  icon = icon("sliders")),
      menuItem("Run & Monitor",     tabName = "run",        icon = icon("play-circle")),
      menuItem("Results",           tabName = "results",    icon = icon("chart-bar")),
      menuItem("Compare Scenarios", tabName = "compare",    icon = icon("exchange-alt"))
    )
  ),

  dashboardBody(

    # ── Custom CSS ──────────────────────────────────────────────────────────
    tags$head(tags$style(HTML("
      :root {
        --bg: #111315;
        --surface: #181b1f;
        --surface-2: #20242a;
        --surface-3: #2a3038;
        --border: rgba(226, 232, 240, 0.10);
        --text: #eef2f6;
        --muted: #a5adb8;
        --accent: #5b8def;
        --accent-strong: #7aa2f7;
        --warning: #d49a3a;
        --danger: #d45d5d;
        --shadow: 0 12px 28px rgba(0, 0, 0, 0.22);
      }

      html, body, .wrapper {
        background: var(--bg) !important;
        color: var(--text);
        font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      body {
        color: var(--text);
        letter-spacing: 0;
      }

      .content-wrapper, .right-side {
        background: var(--bg) !important;
        min-height: 100vh;
      }

      .content {
        padding: 22px;
      }

      .main-header .logo,
      .skin-black .main-header .logo,
      .skin-black .main-header .logo:hover,
      .skin-black .main-header .navbar {
        background: var(--surface) !important;
        color: var(--text) !important;
        border: 0 !important;
        box-shadow: none;
      }

      .main-header .logo {
        font-weight: 800;
        letter-spacing: 0;
      }

      .main-header .navbar {
        border-bottom: 1px solid var(--border) !important;
      }

      .skin-black .main-header .navbar .sidebar-toggle {
        color: var(--muted) !important;
      }

      .skin-black .main-header .navbar .sidebar-toggle:hover {
        background: var(--surface-2) !important;
        color: var(--text) !important;
      }

      .main-sidebar,
      .left-side,
      .skin-black .main-sidebar {
        background: var(--surface) !important;
        border-right: 1px solid var(--border);
        box-shadow: none;
      }

      .sidebar-menu {
        padding: 14px 10px;
      }

      .sidebar-menu > li {
        margin: 4px 0;
      }

      .skin-black .sidebar-menu > li > a,
      .skin-black .sidebar-menu > li.header {
        border-left: 0 !important;
        color: var(--muted) !important;
        border-radius: 10px;
        margin: 0 2px;
        font-weight: 650;
      }

      .skin-black .sidebar-menu > li:hover > a,
      .skin-black .sidebar-menu > li.active > a {
        background: #222832 !important;
        color: var(--text) !important;
        box-shadow: inset 3px 0 0 var(--accent);
      }

      .sidebar-menu i {
        color: var(--accent);
        margin-right: 8px;
      }

      .box {
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: 12px;
        box-shadow: var(--shadow);
        color: var(--text);
        margin-bottom: 16px;
        overflow: hidden;
      }

      .box.box-solid,
      .box.box-primary,
      .box.box-warning,
      .box.box-info {
        border: 1px solid var(--border);
      }

      .box-header,
      .box.box-solid > .box-header,
      .box.box-primary > .box-header,
      .box.box-warning > .box-header,
      .box.box-info > .box-header {
        background: var(--surface);
        border-bottom: 1px solid var(--border);
        color: var(--text);
        padding: 15px 18px;
      }

      .box.box-solid.box-primary > .box-header,
      .box.box-solid.box-warning > .box-header,
      .box.box-solid.box-info > .box-header {
        background: var(--surface-2) !important;
        color: var(--text) !important;
      }

      .box.box-solid.box-primary { border-top: 3px solid var(--accent); }
      .box.box-solid.box-warning { border-top: 3px solid var(--warning); }
      .box.box-solid.box-info { border-top: 3px solid #54b7c8; }

      .box-title {
        color: var(--text);
        font-size: 15px;
        font-weight: 750;
      }

      .box-body {
        padding: 18px;
      }

      label,
      .control-label,
      .selectize-control.single .selectize-input,
      .help-block {
        color: var(--muted);
        font-weight: 650;
      }

      .form-control,
      .selectize-input,
      input[type='number'],
      input[type='text'] {
        background: #121417 !important;
        border: 1px solid var(--border) !important;
        border-radius: 12px !important;
        color: var(--text) !important;
        box-shadow: none !important;
        min-height: 40px;
      }

      .form-control:focus,
      .selectize-input.focus,
      input:focus {
        border-color: rgba(91, 141, 239, 0.62) !important;
        box-shadow: 0 0 0 3px rgba(91, 141, 239, 0.14) !important;
      }

      .irs-line {
        background: #2d333b !important;
        border: 1px solid var(--border) !important;
        border-radius: 999px;
      }

      .irs-bar,
      .irs-bar-edge {
        background: var(--accent) !important;
        border-color: transparent !important;
      }

      .irs-slider {
        background: #e8edf4 !important;
        border: 2px solid var(--accent) !important;
        border-radius: 50% !important;
        box-shadow: 0 2px 6px rgba(15, 23, 42, 0.18) !important;
      }

      .irs-grid-text,
      .irs-min,
      .irs-max,
      .irs-from,
      .irs-to,
      .irs-single {
        color: var(--muted) !important;
      }

      .irs-single,
      .irs-from,
      .irs-to {
        background: var(--surface-3) !important;
        border-radius: 999px !important;
        color: var(--text) !important;
      }

      .btn {
        border: 0 !important;
        border-radius: 10px !important;
        font-weight: 750;
        letter-spacing: 0;
        box-shadow: none;
        transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease;
      }

      .btn:hover {
        filter: none;
        box-shadow: none;
      }

      .btn-lg {
        padding: 10px 16px;
        font-size: 15px;
      }

      .configure-action-box {
        box-shadow: none;
      }

      .configure-action-box .box-body {
        min-height: 64px;
        padding: 16px;
      }

      .config-action-row {
        align-items: center;
        display: flex;
        gap: 14px;
        justify-content: space-between;
      }

      .config-action-note {
        color: var(--muted);
        font-size: 13px;
        font-weight: 650;
      }

      .config-summary {
        background: var(--surface-2);
        border: 1px solid var(--border);
        border-radius: 12px;
        margin-top: 14px;
        padding: 14px;
      }

      .config-summary-title {
        color: var(--text);
        font-size: 14px;
        font-weight: 800;
        margin-bottom: 12px;
      }

      .config-summary-grid {
        display: grid;
        gap: 10px;
        grid-template-columns: repeat(4, minmax(150px, 1fr));
      }

      .config-summary-item {
        background: #15181c;
        border: 1px solid var(--border);
        border-radius: 10px;
        padding: 10px 12px;
      }

      .config-summary-label {
        color: var(--muted);
        font-size: 11px;
        font-weight: 800;
        letter-spacing: 0;
        margin-bottom: 3px;
      }

      .config-summary-value {
        color: var(--text);
        font-size: 15px;
        font-weight: 800;
        line-height: 1.25;
      }

      .config-summary.is-empty {
        color: var(--muted);
        font-size: 13px;
        font-weight: 650;
      }

      .btn-primary,
      .btn-success {
        background: var(--accent) !important;
        color: #ffffff !important;
      }

      .btn-primary:hover,
      .btn-success:hover {
        background: var(--accent-strong) !important;
      }

      .btn-warning {
        background: var(--warning) !important;
        color: #17130b !important;
      }

      .btn-warning:hover {
        background: #e0aa52 !important;
      }

      .btn-danger {
        background: var(--danger) !important;
        color: #ffffff !important;
      }

      .btn-danger:hover {
        background: #e07474 !important;
      }

      .small-box {
        border-radius: 12px;
        overflow: hidden;
        border: 0;
        box-shadow: var(--shadow);
        min-height: 104px;
      }

      .small-box > .inner {
        padding: 18px;
      }

      .small-box h3 {
        font-size: 31px;
        font-weight: 800;
        color: #ffffff;
      }

      .small-box p {
        color: rgba(255, 255, 255, 0.86);
        font-weight: 650;
      }

      .small-box .icon {
        top: 12px;
      }

      .bg-blue,
      .bg-aqua,
      .bg-green,
      .bg-yellow,
      .bg-red {
        color: #ffffff !important;
      }

      .bg-blue {
        background: #315fba !important;
      }

      .bg-aqua {
        background: #247c8e !important;
      }

      .bg-green {
        background: #2f8b57 !important;
      }

      .bg-yellow {
        background: #c9892e !important;
      }

      .bg-red {
        background: #b84e4e !important;
      }

      .small-box .icon {
        color: rgba(255, 255, 255, 0.24);
      }

      .bg-blue .icon,
      .bg-aqua .icon,
      .bg-green .icon,
      .bg-yellow .icon,
      .bg-red .icon {
        color: rgba(255, 255, 255, 0.24);
      }

      pre,
      .shiny-text-output,
      #sim_status_text {
        background: var(--surface-2);
        border: 1px solid var(--border);
        border-radius: 14px;
        color: var(--text) !important;
      }

      pre {
        padding: 14px;
      }

      #sim_status_text {
        display: inline-flex;
        min-height: 44px;
        align-items: center;
        padding: 0 14px;
        color: var(--accent) !important;
      }

      #sim_status_text .shiny-text-output {
        background: transparent !important;
        border: 0 !important;
        border-radius: 0 !important;
        color: inherit !important;
        padding: 0 !important;
      }

      .table,
      table.dataTable,
      .dataTables_wrapper {
        color: var(--text) !important;
      }

      table.dataTable,
      .table {
        background: var(--surface);
        border-radius: 14px;
        overflow: hidden;
      }

      table.dataTable thead th,
      .table > thead > tr > th {
        background: var(--surface-2) !important;
        border-bottom: 1px solid var(--border) !important;
        color: var(--text) !important;
      }

      table.dataTable tbody tr,
      .table > tbody > tr {
        background: var(--surface) !important;
      }

      table.dataTable tbody tr:hover,
      .table > tbody > tr:hover {
        background: var(--surface-2) !important;
      }

      table.dataTable tbody td,
      .table > tbody > tr > td {
        border-top: 1px solid var(--border) !important;
      }

      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        color: var(--muted) !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button {
        border-radius: 10px !important;
        color: var(--muted) !important;
        border: 1px solid transparent !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: var(--surface-3) !important;
        border: 1px solid var(--border) !important;
        color: var(--text) !important;
      }

      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        background: #121417 !important;
        border: 1px solid var(--border) !important;
        border-radius: 10px;
        color: var(--text) !important;
        min-height: 34px;
      }

      .shiny-plot-output {
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: 14px;
        overflow: hidden;
      }

      .shiny-plot-output img {
        border-radius: 14px;
      }

      .shiny-notification {
        background: var(--surface-2) !important;
        border: 1px solid var(--border) !important;
        border-radius: 14px !important;
        color: var(--text) !important;
        box-shadow: var(--shadow) !important;
      }

      .main-footer {
        background: var(--surface);
        border-top: 1px solid var(--border);
        color: var(--muted);
      }

      .kpi-box { text-align: center; padding: 10px; }
      .kpi-value { font-size: 2em; font-weight: bold; color: var(--text); }
      .kpi-label { font-size: 0.9em; color: var(--muted); }

      @media (max-width: 767px) {
        .content {
          padding: 14px;
        }

        .box {
          border-radius: 14px;
        }

        .btn-lg {
          width: auto;
          margin-bottom: 8px;
        }

        .config-action-row {
          align-items: stretch;
          flex-direction: column;
        }

        .configure-action-box .btn-lg {
          width: 100%;
        }

        .config-summary-grid {
          grid-template-columns: 1fr;
        }
      }
    "))),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('configApplied', function(config) {
        function item(label, value) {
          return '<div class=\"config-summary-item\">' +
                 '<div class=\"config-summary-label\">' + label + '</div>' +
                 '<div class=\"config-summary-value\">' + value + '</div>' +
                 '</div>';
        }

        var urgency = [
          'Critical ' + config.prob_critical + '%',
          'Urgent ' + config.prob_urgent + '%',
          'Standard ' + config.prob_standard + '%'
        ].join(' / ');

        var serviceTimes = [
          'Critical ' + config.svc_crit_mean + '+/-' + config.svc_crit_sd,
          'Urgent ' + config.svc_urg_mean + '+/-' + config.svc_urg_sd,
          'Standard ' + config.svc_std_mean + '+/-' + config.svc_std_sd
        ].join(' min, ');

        $('#applied_config_summary')
          .removeClass('is-empty')
          .html(
            '<div class=\"config-summary-title\">Applied Configuration</div>' +
            '<div class=\"config-summary-grid\">' +
              item('Arrival rate', config.arrival_rate + ' patients/min') +
              item('Duration', config.sim_duration + ' min') +
              item('Staff', config.n_doctors + ' doctors, ' + config.n_nurses + ' nurses') +
              item('Seed', config.seed) +
              item('Urgency mix', urgency) +
              item('Service times', serviceTimes) +
            '</div>'
          );
      });
    ")),

    tabItems(

      # ── Tab 1: Configure ─────────────────────────────────────────────────
      tabItem(tabName = "configure",
        fluidRow(
          box(title = "Arrival & Duration", width = 6, solidHeader = TRUE,
              status = "primary",
              sliderInput("arrival_rate",  "Arrival Rate (patients/min)",
                          min = 0.1, max = 5, value = 0.5, step = 0.1),
              sliderInput("sim_duration",  "Simulation Duration (min)",
                          min = 60, max = 1440, value = 480, step = 60),
              numericInput("seed", "Random Seed (blank = random)",
                           value = 42, min = 1, max = .Machine$integer.max)
          ),
          box(title = "Staff Resources", width = 6, solidHeader = TRUE,
              status = "primary",
              sliderInput("n_doctors", "Number of Doctors",
                          min = 1, max = 20, value = 3, step = 1),
              sliderInput("n_nurses",  "Number of Nurses",
                          min = 1, max = 20, value = 5, step = 1)
          )
        ),
        fluidRow(
          box(title = "Urgency Distribution", width = 6, solidHeader = TRUE,
              status = "warning",
              sliderInput("prob_critical", "Critical (%)",
                          min = 0, max = 100, value = 20, step = 1),
              sliderInput("prob_urgent",   "Urgent (%)",
                          min = 0, max = 100, value = 30, step = 1),
              sliderInput("prob_standard", "Standard (%)",
                          min = 0, max = 100, value = 50, step = 1),
              textOutput("urgency_sum_warn")
          ),
          box(title = "Service Times (mean / sd in minutes)", width = 6,
              solidHeader = TRUE, status = "warning",
              fluidRow(
                column(6,
                  numericInput("svc_crit_mean",  "Critical mean",  30, min = 1),
                  numericInput("svc_urg_mean",   "Urgent mean",    20, min = 1),
                  numericInput("svc_std_mean",   "Standard mean",  15, min = 1)
                ),
                column(6,
                  numericInput("svc_crit_sd",    "Critical sd",    10, min = 0),
                  numericInput("svc_urg_sd",     "Urgent sd",       7, min = 0),
                  numericInput("svc_std_sd",     "Standard sd",     5, min = 0)
                )
              )
          )
        ),
        fluidRow(
          box(width = 12, class = "configure-action-box",
              tags$div(class = "config-action-row",
                tags$div(class = "config-action-note", "Apply the current inputs to lock in the simulation setup."),
                actionButton("btn_apply_config", "Apply Configuration",
                             class = "btn-primary btn-lg",
                             icon  = icon("check"))
              ),
              tags$div(
                id = "applied_config_summary",
                class = "config-summary is-empty",
                "No configuration applied yet."
              )
          )
        )
      ),

      # ── Tab 2: Run & Monitor ─────────────────────────────────────────────
      tabItem(tabName = "run",
        fluidRow(
          box(width = 12,
              actionButton("btn_run", "Run Simulation",
                           class = "btn-primary btn-lg",
                           icon  = icon("play")),
              tags$br(), tags$br(),
              tags$div(id = "sim_status_text",
                       style = "font-size: 1.1em; color: #2980b9;",
                       textOutput("sim_status"))
          )
        ),
        fluidRow(
          valueBoxOutput("vbox_n_patients",  width = 3),
          valueBoxOutput("vbox_not_served",  width = 3),
          valueBoxOutput("vbox_mean_wait",   width = 3),
          valueBoxOutput("vbox_throughput",  width = 3)
        ),
        fluidRow(
          box(title = "Event Log", width = 12,
              solidHeader = TRUE, status = "info",
              DTOutput("run_event_table")
          )
        )
      ),

      # ── Tab 3: Results ───────────────────────────────────────────────────
      tabItem(tabName = "results",
        fluidRow(
          valueBoxOutput("res_n_patients",  width = 3),
          valueBoxOutput("res_not_served",  width = 3),
          valueBoxOutput("res_mean_wait",   width = 3),
          valueBoxOutput("res_throughput",  width = 3)
        ),
        fluidRow(
          valueBoxOutput("res_p95_wait",    width = 3)
        ),
        fluidRow(
          box(title = "Wait Time Distribution by Urgency", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("plot_wait_hist", height = "300px")),
          box(title = "Queue Length Over Time (with Rolling Mean)", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("plot_queue_time", height = "300px"))
        ),
        fluidRow(
          box(title = "Mean Wait by Urgency Level", width = 6,
              solidHeader = TRUE, status = "warning",
              plotOutput("plot_wait_urgency", height = "300px")),
          box(title = "Resource Utilisation", width = 6,
              solidHeader = TRUE, status = "warning",
              plotOutput("plot_utilisation", height = "300px"))
        ),
        fluidRow(
          box(title = "Patient Log", width = 12, solidHeader = TRUE,
              status = "info",
              DTOutput("results_table"))
        )
      ),

      # ── Tab 4: Compare Scenarios ─────────────────────────────────────────
      tabItem(tabName = "compare",
        fluidRow(
          box(width = 12,
              textInput("scenario_name", "Scenario Name",
                        placeholder = "e.g. Baseline, +2 Doctors"),
              actionButton("btn_save_scenario", "Save Current Results as Scenario",
                           class = "btn-warning",
                           icon  = icon("bookmark")),
              actionButton("btn_clear_scenarios", "Clear All Scenarios",
                           class = "btn-danger",
                           icon  = icon("trash"))
          )
        ),
        fluidRow(
          box(title = "Saved Scenarios", width = 12,
              tableOutput("scenarios_summary_table"))
        ),
        fluidRow(
          box(title = "Mean Wait Time Comparison", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("compare_wait_plot", height = "300px")),
          box(title = "Throughput Comparison", width = 6,
              solidHeader = TRUE, status = "primary",
              plotOutput("compare_throughput_plot", height = "300px"))
        ),
        fluidRow(
          box(title = "Resource Utilisation Comparison", width = 12,
              solidHeader = TRUE, status = "warning",
              plotOutput("compare_utilisation_plot", height = "350px"))
        )
      )
    )
  )
)
