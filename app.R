library(shiny)
library(shinyjs)
library(openxlsx)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(parallel)

# Security parameters (will be set via environment variables in production)
unlock_date <- Sys.getenv("unlock_date", "2026-04-25")
correct_password <- Sys.getenv("correct_password", "1020304050")

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$script(HTML("
    setInterval(function(){
      Shiny.setInputValue('keepalive', Date.now());
    }, 60000); // Ping every minute
  "))),
  titlePanel("Production Scheduling Optimizer (Heuristic)"),
  p("This tool creates production schedules using heuristic methods for large-scale problems."),
  p(em("Upload your Excel file with the required sheets and click 'Generate Schedule'.")), 
  
  # Authentication UI
  hidden(
    div(
      id = "auth_panel",
      passwordInput("password", "Enter Access Password:"),
      actionButton("login_btn", "Login"),
      textOutput("auth_message")
    )
  ),
  
  # Main app UI
  hidden(
    div(
      id = "main_app",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Choose Excel File", accept = c(".xlsx")),
          numericInput("num_cores", "Number of Cores", 
                       value = max(1, detectCores() - 1), 
                       min = 1, max = detectCores()),
          actionButton("generate", "Generate Schedule", class = "btn-lg"),
          hr(),
          verbatimTextOutput("console_output"),
          downloadButton("download_excel", "Download Schedule xlsx", class = "btn-sm")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Schedule", DTOutput("schedule_table")),
            tabPanel("Gantt Chart", 
                     div(style = "overflow-x: scroll; width: 100%", 
                         plotlyOutput("gantt_plot", height = "600px"))),
            tabPanel("Schedule Info", 
                     verbatimTextOutput("schedule_info"),
                     textOutput("elapsed_time"))
          )
        )
      )
    )
  )
)

# [Include all your server logic here - identical to your existing server code]

server <- function(input, output, session) {
  # Reactive value for authentication status
  auth_status <- reactiveValues(valid = FALSE)
  
  # Set clientData inactivity timeout to 15 minutes (900000 ms)
  shinyOptions(clientData = list(inactivityTimeout = 900000))
  
  # Check if we're past the unlock date
  past_unlock_date <- reactive({
    Sys.Date() > as.Date(unlock_date)
  })
  
  # Show appropriate UI based on date and auth status
  observe({
    if (past_unlock_date()) {
      # After unlock date - require password
      show("auth_panel")
      hide("main_app")
    } else {
      # Before unlock date - show app directly
      hide("auth_panel")
      show("main_app")
      auth_status$valid <- TRUE
    }
  })
  
  # Handle login attempts
  observeEvent(input$login_btn, {
    if (input$password == correct_password) {
      auth_status$valid <- TRUE
      hide("auth_panel")
      show("main_app")
      output$auth_message <- renderText("")
    } else {
      output$auth_message <- renderText("Incorrect password. Please try again.")
    }
  })
  
  # Only proceed with the app if authenticated or before unlock date
  observe({
    req(auth_status$valid || !past_unlock_date())
    
    # Reactive values to store data
    rv <- reactiveValues(
      schedule = NULL,
      console_text = "",
      start_time = NULL,
      input_data = NULL,
      makespan = NULL,
      status = NULL
    )
    
    # Capture console output
    capture_console <- function(expr) {
      console <- capture.output(expr, type = "message")
      rv$console_text <- paste(rv$console_text, paste(console, collapse = "\n"), sep = "\n")
    }
    
    # Read input file
    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        rv$input_data <- list(
          df_machines = read_excel(input$file$datapath, sheet = "Machines"),
          df_jobs = read_excel(input$file$datapath, sheet = "Jobs"),
          df_processing_time = read_excel(input$file$datapath, sheet = "ProcessingTimes"),
          df_setup_time = read_excel(input$file$datapath, sheet = "SetupTimes"),
          df_deadlines = read_excel(input$file$datapath, sheet = "Deadlines")
        )
        showNotification("File loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Priority rule-based scheduling function with independent setup times
    priority_scheduler <- function(jobs, machines, processing_time, setup_time, deadlines, rule) {
      # Initialize machine schedules
      machine_schedules <- lapply(machines, function(m) {
        data.frame(Job = character(), Start = numeric(), End = numeric())
      })
      names(machine_schedules) <- machines
      
      # Order jobs based on selected priority rule
      if (rule == "EDD") {
        job_order <- order(deadlines[jobs])
      } else if (rule == "SPT") {
        avg_processing <- sapply(jobs, function(j) mean(processing_time[paste(j, machines)]))
        job_order <- order(avg_processing)
      } else if (rule == "LFJ") {
        machine_options <- sapply(jobs, function(j) sum(!is.na(processing_time[paste(j, machines)])))
        job_order <- order(machine_options)
      } else if (rule == "CR") {
        current_time <- 0
        time_remaining <- deadlines[jobs] - current_time
        avg_processing <- sapply(jobs, function(j) mean(processing_time[paste(j, machines)]))
        critical_ratio <- time_remaining / avg_processing
        job_order <- order(critical_ratio)
      }
      
      ordered_jobs <- jobs[job_order]
      
      # Assign jobs to machines
      for (job in ordered_jobs) {
        # Find eligible machines for this job
        eligible_machines <- machines[!is.na(processing_time[paste(job, machines)])]
        
        # Find machine with earliest availability considering job-specific setup time
        best_machine <- NULL
        best_start <- Inf
        best_end <- Inf
        
        for (machine in eligible_machines) {
          machine_schedule <- machine_schedules[[machine]]
          
          if (nrow(machine_schedule) == 0) {
            # Machine is empty - just add setup time
            setup <- ifelse(is.null(setup_time[[paste(job, machine)]]),
                            0, setup_time[[paste(job, machine)]])
            start_time <- setup
          } else {
            # Machine has jobs - schedule after last job + setup time
            setup <- ifelse(is.null(setup_time[[paste(job, machine)]]),
                            0, setup_time[[paste(job, machine)]])
            start_time <- max(tail(machine_schedule$End, 1)) + setup
          }
          
          end_time <- start_time + processing_time[paste(job, machine)]
          
          if (end_time < best_end) {
            best_machine <- machine
            best_start <- start_time
            best_end <- end_time
          }
        }
        
        if (!is.null(best_machine)) {
          # Add job to machine schedule
          machine_schedules[[best_machine]] <- rbind(
            machine_schedules[[best_machine]],
            data.frame(Job = job, Start = best_start, End = best_end)
          )
        }
      }
      
      # Combine all machine schedules
      schedule <- do.call(rbind, lapply(names(machine_schedules), function(m) {
        df <- machine_schedules[[m]]
        if (nrow(df) > 0) {
          df$Machine <- m
          df$Deadline <- deadlines[df$Job]
          df$Status <- ifelse(df$End > df$Deadline, "Late", "On Time")
          df[, c("Machine", "Job", "Start", "End", "Deadline", "Status")]
        }
      }))
      
      makespan <- max(schedule$End, na.rm = TRUE)
      
      return(list(
        schedule = schedule,
        makespan = makespan,
        status = paste("Priority rule solution (", rule, ")", sep = "")
      ))
    }
    
    # Function to try all priority rules and select the best one
    try_all_rules <- function(jobs, machines, processing_time, setup_time, deadlines) {
      rules <- c("EDD", "SPT", "LFJ", "CR")
      
      # Run each rule in parallel
      cl <- makeCluster(min(input$num_cores, length(rules)))
      clusterExport(cl, c("priority_scheduler"), envir = environment())
      
      results <- parLapply(cl, rules, function(rule) {
        priority_scheduler(jobs, machines, processing_time, setup_time, deadlines, rule)
      })
      
      stopCluster(cl)
      
      # Find the result with the smallest makespan
      best_idx <- which.min(sapply(results, function(x) x$makespan))
      return(results[[best_idx]])
    }
    
    # Schedule generation process
    observeEvent(input$generate, {
      req(input$file)
      req(rv$input_data)
      
      rv$start_time <- Sys.time()
      rv$console_text <- ""
      
      # Disable button during generation
      shinyjs::disable("generate")
      on.exit(shinyjs::enable("generate"))
      
      showNotification("Schedule generation started...", duration = NULL)
      
      tryCatch({
        capture_console({
          # Extract data from input
          df_machines <- rv$input_data$df_machines
          df_jobs <- rv$input_data$df_jobs
          df_processing_time <- rv$input_data$df_processing_time
          df_setup_time <- rv$input_data$df_setup_time
          df_deadlines <- rv$input_data$df_deadlines
          
          machines <- df_machines$Machines
          jobs <- df_jobs$Jobs
          
          # Create named lists
          processing_time <- setNames(df_processing_time[[3]],
                                      paste(df_processing_time[[1]], df_processing_time[[2]]))
          
          # Modified setup time to be job-machine specific (not sequence dependent)
          setup_time <- setNames(df_setup_time[[3]],
                                 paste(df_setup_time[[1]], df_setup_time[[2]]))
          
          deadlines <- setNames(df_deadlines[[2]], df_deadlines[[1]])
          
          # Try all priority rules and select the best one
          result <- try_all_rules(jobs, machines, processing_time, setup_time, deadlines)
          
          rv$schedule <- result$schedule
          rv$makespan <- result$makespan
          rv$status <- result$status
        })
        
        showNotification("Schedule generated successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        rv$console_text <- paste(rv$console_text, paste("Error:", e$message), sep = "\n")
      })
    })
    
    # Outputs
    output$schedule_table <- renderDT({
      req(rv$schedule)
      datatable(rv$schedule, options = list(scrollX = TRUE))
    })
    
    output$gantt_plot <- renderPlotly({
      req(rv$schedule)
      
      max_end_time <- max(rv$schedule$End, na.rm = TRUE)
      deadline_buffer <- max(rv$schedule$Deadline, na.rm = TRUE) * 0.1
      
      # Create the plot
      p <- ggplot(rv$schedule, aes(x = Start, xend = End,
                                   y = Machine, yend = Machine,
                                   color = Job, 
                                   text = paste("Job:", Job, 
                                                "<br>Start:", Start, 
                                                "<br>End:", End,
                                                "<br>Deadline:", Deadline,
                                                "<br>Status:", Status))) +
        geom_segment(size = 8) +
        geom_vline(aes(xintercept = Deadline), linetype = "dashed", color = "red") +
        labs(title = paste("Schedule Gantt Chart (", rv$status, ")", sep = ""),
             subtitle = paste("Makespan:", round(rv$makespan, 1)),
             x = "Time", y = "Machine") +
        theme_minimal() +
        scale_x_continuous(limits = c(0, max(max_end_time + deadline_buffer, 
                                             max(rv$schedule$Deadline, na.rm = TRUE)))) +
        geom_text(aes(x = Start + (End-Start)/2, y = Machine, 
                      label = Job), color = "white", size = 3) +
        theme(legend.position = "none")
      
      # Convert to plotly with zoom and scroll
      ggplotly(p, tooltip = "text") %>%
        layout(xaxis = list(fixedrange = FALSE),
               yaxis = list(fixedrange = TRUE),
               dragmode = "pan") %>%
        config(displayModeBar = TRUE,
               scrollZoom = TRUE,
               displaylogo = FALSE)
    })
    
    output$console_output <- renderText({
      rv$console_text
    })
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("production_schedule_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
      },
      content = function(file) {
        # Create a workbook with multiple sheets
        wb <- openxlsx::createWorkbook()
        
        # Sheet 1: Schedule Data
        openxlsx::addWorksheet(wb, "Schedule")
        openxlsx::writeData(wb, "Schedule", rv$schedule)
        
        # Sheet 2: Schedule Summary
        openxlsx::addWorksheet(wb, "Summary")
        summary_data <- data.frame(
          Metric = c("Scheduling Method", "Makespan", "Total Jobs", "On Time Jobs", "Late Jobs"),
          Value = c(
            rv$status,
            round(rv$makespan, 2),
            nrow(rv$schedule),
            sum(rv$schedule$Status == "On Time"),
            sum(rv$schedule$Status == "Late")
          )
        )
        openxlsx::writeData(wb, "Summary", summary_data)
        
        # Sheet 3: Console Output
        openxlsx::addWorksheet(wb, "Console_Log")
        openxlsx::writeData(wb, "Console_Log", data.frame(Log = rv$console_text))
        
        # Save the workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    output$schedule_info <- renderPrint({
      req(rv$schedule)
      cat("Scheduling Method:", rv$status, "\n")
      cat("Makespan:", rv$makespan, "\n")
      cat("\nSchedule Summary:\n")
      cat("Total Jobs:", nrow(rv$schedule), "\n")
      cat("On Time Jobs:", sum(rv$schedule$Status == "On Time"), "\n")
      cat("Late Jobs:", sum(rv$schedule$Status == "Late"), "\n")
    })
    
    output$elapsed_time <- renderText({
      req(rv$start_time) 
      paste("Elapsed Time:", 
            round(as.numeric(difftime(Sys.time(), rv$start_time, units = "secs")), 1), 
            "seconds") 
    })
  })
}

tryCatch({
  shinyApp(ui, server)
}, error = function(e) {
  cat("🚨 Startup Error:\n")
  print(e)
  stop(e)
})
