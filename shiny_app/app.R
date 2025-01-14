# Load required libraries
library(shiny)
library(DT)
library(bslib)
library(HDAnalyzeR)
library(dplyr)
library(plotly)

# Define custom themes
light_theme <- bs_theme(
  primary = "#883268",
)

# Define UI
ui <- navbarPage(
  title = "HDAnalyzeR",
  theme = light_theme,
  id = "main_navbar", # Identifier for navigation

  # Page 1: Welcome & Data Upload
  tabPanel(
    "Welcome & Data Upload",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h3("Welcome!"),
          p("This app simplifies proteomics data analysis and biomarker discovery."),
          hr(),
          h4("Data"),
          fileInput("data_file",
                    "Upload Data File (.csv, .tsv, .txt, .xlsx, .rds, .rda, .parquet):",
                    accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds", ".rda", ".parquet")),
          uiOutput("sample_id_ui"),
          uiOutput("variable_value_ui"),
          hr(),
          h4("Metadata"),
          fileInput("metadata_file",
                    "Upload Metadata File (.csv, .tsv, .txt, .xlsx, .rds, .rda, .parquet):",
                    accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds", ".rda", ".parquet")),
          textOutput("validation_msg"),

          # Filtering Section
          hr(),
          h4("Filtering"),
          textInput("columns_to_keep",
                    "Enter Column Names to Keep (comma-separated):",
                    value = ""),
          textInput("rows_to_keep",
                    "Enter Row Numbers to Keep (comma-separated):",
                    value = ""),

          # Button Grid with Consistent Alignment
          fluidRow(
            column(6, actionButton("rows_keep", "Keep Rows", class = "btn-block")),
            column(6, actionButton("rows_remove", "Remove Rows", class = "btn-block btn-primary"))
          ),
          fluidRow(
            column(6, actionButton("cols_keep", "Keep Cols", class = "btn-block")),
            column(6, actionButton("cols_remove", "Remove Cols", class = "btn-block btn-primary"))
          ),
          actionButton("reset_data", "Reset", style = "margin-top: 10px;"),
          tags$style(".btn-block { width: 100%; margin-top: 10px; }")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Data Preview",
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "overflow-x: auto; white-space: nowrap;",
                    DTOutput("data_preview") # Render the Data Preview table
                  )
                )
              )
            ),
            tabPanel(
              "Metadata Preview",
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "overflow-x: auto; white-space: nowrap;",
                    DTOutput("metadata_preview") # Render the Metadata Preview table
                  )
                )
              )
            ),
            tabPanel(
              "Processed Data",
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "overflow-x: auto; white-space: nowrap;",
                    DTOutput("processed_data_preview") # Render the Processed Data table
                  )
                )
              )
            )
          ),
          # Plot Section - Place below the data preview section
          hr(),
          h4("Exploratory Data Analysis"),
          fluidRow(
            column(4,
                   textInput("x_variable", "X Variable", value = "")
            ),
            column(4,
                   textInput("y_variable", "Y Variable", value = "")
            ),
            column(4,
                   textInput("color_variable", "Color Variable", value = "")
            ),
          ),
          checkboxInput("equal_axis", "Equal Axes", value = TRUE),
          fluidRow(
            column(6,
                   actionButton("plot_button", "Plot", class = "btn-primary btn-block", style = "height: 50px;") # Align plot button below
            )
          ),
          plotlyOutput("scatter_plot")  # Plot display here
        )
      )
    )
  ),

  # Page 2: Dimensionality Reduction
  tabPanel(
    "Dimensionality Reduction",
    fluidPage(
      h3("Dimensionality Reduction"),
      p("This page is under construction."),
      p("In the future, you will be able to perform PCA and other dimensionality reduction techniques here.")
    )
  )
)

# Define server
server <- function(input, output, session) {

  # Reactive values for data and metadata
  data <- reactive({
    req(input$data_file)
    file <- input$data_file$datapath
    hd_import_data(file)
  })

  metadata <- reactive({
    req(input$metadata_file)
    file <- input$metadata_file$datapath
    hd_import_data(file)
  })

  output$sample_id_ui <- renderUI({
    req(data())
    selectInput("sample_id",
                "Select Sample ID Column:",
                choices = names(data()))
  })

  # Observe Sample ID selection and check for duplicates
  observeEvent(input$sample_id, {
    req(data())
    sample_id_col <- input$sample_id
    duplicates_exist <- any(duplicated(data()[[sample_id_col]]))

    if (duplicates_exist) {
      showNotification("Sample ID column has duplicates. Please provide Variable and Value columns to widen the data.", type = "warning")

      output$variable_value_ui <- renderUI({
        tagList(
          selectInput("var_column",
                      "Select Variable Column (e.g., Protein Names):",
                      choices = names(data())),
          selectInput("value_column",
                      "Select Value Column (e.g., Protein Expression Levels):",
                      choices = names(data())),
          actionButton("widen_data", "Widen Data")
        )
      })
    } else {
      showNotification("Sample ID column has no duplicates. Data is already in wide format.", type = "message")
      output$variable_value_ui <- renderUI(NULL)
    }
  })

  observeEvent(input$sample_id, {
    req(metadata())
    sample_id_col <- input$sample_id

    # Check if the selected Sample ID exists in metadata
    if (!sample_id_col %in% names(metadata())) {
      showNotification(
        "The selected Sample ID column is not present in the metadata file. Please select a different column.",
        type = "error"
      )
    }
  })

  # Handle data widening
  processed_data <- reactiveVal(NULL)
  original_data <- reactiveVal(NULL)
  observeEvent(input$widen_data, {
    req(data(), input$sample_id, input$var_column, input$value_column)

    # Attempt to widen data with error handling
    tryCatch(
      {
        wide_data <- hd_initialize(
          dat = data(),
          metadata = NULL,
          is_wide = FALSE,
          sample_id = input$sample_id,
          var_name = input$var_column,
          value_name = input$value_column
        )[["data"]]

        # If successful, store the widened data
        processed_data(wide_data)
        original_data(wide_data)
        showNotification("Data successfully widened and ready for analysis!", type = "message")
      },
      error = function(e) {
        # Handle errors gracefully
        showNotification(
          paste("Error in data widening, please use different columns."),
          type = "error"
        )

        # Do not update processed_data on error
        processed_data()
      }
    )
  })

  observe({
    req(data(), input$sample_id)

    # Check if data is already in wide format
    duplicates_exist <- any(duplicated(data()[[input$sample_id]]))
    if (!duplicates_exist) {
      processed_data(data())
      original_data(data())
    }
  })

  # Display processed (widened) data if available
  output$processed_data_preview <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 5))
  })

  # Preview uploaded data
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 5))
  })

  # Preview uploaded metadata
  output$metadata_preview <- renderDT({
    req(metadata())
    datatable(metadata(), options = list(pageLength = 5))
  })

  # Validation message
  output$validation_msg <- renderText({
    if (is.null(input$data_file) || is.null(input$metadata_file)) {
      return("Please upload both data and metadata files to proceed.")
    }
    "Files uploaded successfully!"
  })

  # Perform Row Filtering
  observeEvent(c(input$rows_keep, input$rows_remove), {
    req(processed_data())

    data <- processed_data()
    row_indices <- as.numeric(unlist(strsplit(input$rows_to_keep, ",")))
    if (any(is.na(row_indices)) && input$rows_to_keep != "") {
      showNotification("Please enter valid row numbers.", type = "error")
      return()
    }

    if (input$rows_keep > input$rows_remove) {
      # Keep selected rows
      filtered <- data[row_indices, , drop = FALSE]
    } else {
      # Remove selected rows
      filtered <- data[-row_indices, , drop = FALSE]
    }

    # Update processed_data with the filtered data
    processed_data(filtered)
    showNotification("Row filtering applied successfully! Processed data updated.", type = "message")
  })

  # Perform Column Filtering
  observeEvent(c(input$cols_keep, input$cols_remove), {
    req(processed_data())

    data <- processed_data()
    selected_columns <- unlist(strsplit(input$columns_to_keep, ","))
    selected_columns <- trimws(selected_columns) # Remove extra whitespace
    if (any(!selected_columns %in% names(data))) {
      showNotification("Some column names are invalid. Please check your input.", type = "error")
      return()
    }

    if (input$cols_keep > input$cols_remove) {
      # Keep selected columns
      filtered <- data[, selected_columns, drop = FALSE]
    } else {
      # Remove selected columns
      filtered <- data[, !(names(data) %in% selected_columns), drop = FALSE]
    }

    # Update processed_data with the filtered data
    processed_data(filtered)
    showNotification("Column filtering applied successfully! Processed data updated.", type = "message")
  })

  merged_data <- reactive({
    req(processed_data(), metadata())  # Make sure both processed data and metadata are available
    # Merging processed data and metadata
    left_join(processed_data(), metadata(), by = input$sample_id)
  })

  # Reset button logic
  observeEvent(input$reset_data, {
    req(original_data())
    processed_data(original_data()) # Restore the original wide data
    showNotification("Data has been reset to the original wide format.", type = "message")
  })

  # Display Processed Data
  output$processed_data_preview <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 5))
  })

  observeEvent(input$plot_button, {
    req(processed_data())  # Ensure processed data is available

    # Create a merged data set (processed data + metadata)
    merged <- merged_data()

    # Get the x and y variable input from the user
    x_var <- input$x_variable
    y_var <- input$y_variable
    sample_id <- input$sample_id

    # Check if x and y variables exist in the merged data
    if (x_var %in% colnames(merged) && y_var %in% colnames(merged)) {
      if (input$color_variable != "" && input$color_variable %in% colnames(merged)) {
        color_var <- input$color_variable
        plot_data <- merged %>% select(all_of(c(sample_id, x_var, y_var, color_var)))
        x_type <- hd_detect_vartype(plot_data[[x_var]], unique_threshold = 5)
        y_type <- hd_detect_vartype(plot_data[[y_var]], unique_threshold = 5)
        c_type <- hd_detect_vartype(plot_data[[color_var]], unique_threshold = 5)

        if (x_type == "continuous" && y_type == "continuous") {
          plot <- plot_ly(
            data = plot_data,
            x = ~get(x_var),
            y = ~get(y_var),
            color = ~get(color_var),
            type = "scatter",
            mode = "markers",
            text = ~get(sample_id),
            hoverinfo = "text"
          )

          if (input$equal_axis) {
            plot <- plot %>%
              layout(
                xaxis = list(title = paste("<b>", x_var, "</b>"), scaleanchor = "y"),
                yaxis = list(title = paste("<b>", y_var, "</b>"), scaleanchor = "x"))
          } else {
            plot <- plot %>%
              layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                     yaxis = list(title = paste("<b>", y_var, "</b>")))
          }
          plot <- plot %>%
            layout(legend = list(title = list(text = paste("<b>", color_var, "</b>")))) %>%
            colorbar(title = paste("<b>", color_var, "</b>"))
        } else if ((x_type == "continuous" && y_type == "categorical") || (x_type == "categorical" && y_type == "continuous")) {
          if (c_type != "categorical") {
            showNotification("Color variable must be categorical for categorical x or y variable.", type = "error")
            if (c_type == "categorical") {
              color_var <- x_var
            } else {
              color_var <- y_var
            }
          }
          plot <- plot_ly(
            data = plot_data,
            x = ~get(x_var),
            y = ~get(y_var),
            color = ~get(color_var),
            type = "box"
          )
          if (x_var == color_var || y_var == color_var) {
            plot <- plot %>%
              layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                     yaxis = list(title = paste("<b>", y_var, "</b>")),
                     legend = list(title = list(text = paste("<b>", color_var, "</b>"))))
          } else {
            plot <- plot %>%
              layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                     yaxis = list(title = paste("<b>", y_var, "</b>")),
                     legend = list(title = list(text = paste("<b>", color_var, "</b>"))),
                     boxmode = "group")
          }
        }
      } else {
        plot_data <- merged %>% select(all_of(c(sample_id, x_var, y_var)))
        x_type <- hd_detect_vartype(plot_data[[x_var]], unique_threshold = 5)
        y_type <- hd_detect_vartype(plot_data[[y_var]], unique_threshold = 5)

        if (x_type == "continuous" && y_type == "continuous") {
          plot <- plot_ly(
            data = plot_data,
            x = ~get(x_var),
            y = ~get(y_var),
            type = "scatter",
            mode = "markers",
            text = ~get(sample_id),
            hoverinfo = "text"
          )

          if (input$equal_axis) {
            plot <- plot %>%
              layout(
                xaxis = list(title = paste("<b>", x_var, "</b>"), scaleanchor = "y"),
                yaxis = list(title = paste("<b>", y_var, "</b>"), scaleanchor = "x"))
          } else {
            plot <- plot %>%
              layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                     yaxis = list(title = paste("<b>", y_var, "</b>")))
          }
        } else if ((x_type == "continuous" && y_type == "categorical") || (x_type == "categorical" && y_type == "continuous")) {
          plot <- plot_ly(
            data = plot_data,
            x = ~get(x_var),
            y = ~get(y_var),
            type = "box"
          ) %>% layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                       yaxis = list(title = paste("<b>", y_var, "</b>")))
        }
      }

      output$scatter_plot <- renderPlotly({
        plot
      })
    } else {
      showNotification("Selected variables not found in the data.", type = "error")
    }
  })

  # Sample ID detection when clicked on scatter plot points
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      showNotification(paste("Sample ID clicked: ", click_data$text), type = "message")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
