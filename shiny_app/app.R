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

# Define UI --------------------------------------------------------------------
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
          tags$style(".btn-block { width: 100%; margin-top: 10px; }"),
          hr(),
          h4("Missing Data Removal"),
          p("This button will remove rows with missing values (NAs) from the dataset. If the user does not want to remove them, this step can be skipped and the NAs will be imputed before the analysis."),
          p("If not the desired behavior, please handle NAs manually before importing the dataset."),
          fluidRow(
            column(6, actionButton("remove_na_button", "Remove NAs", class = "btn-block btn-primary"))
          ),
          hr(),
          h4("Reset Data"),
          fluidRow(
            column(6, actionButton("reset_data", "Reset", class = "btn-block"))
          )
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
          uiOutput("dynamic_palette_ui"),
          checkboxInput("equal_axis", "Equal Axes", value = TRUE),
          fluidRow(
            column(6,
                   actionButton("plot_button", "Plot", class = "btn-block", style = "height: 50px;")
            )
          ),
          plotlyOutput("scatter_plot")
        )
      )
    )
  ),

  # Page 2: Dimensionality Reduction
  tabPanel(
    "Dimensionality Reduction",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("method", "Choose Method:", choices = c("PCA", "UMAP")),
          numericInput("components", "Number of Components:", value = 10, min = 2, step = 1),
          checkboxInput("by_sample", "By Sample", value = TRUE),
          selectInput("plot_x", "X-axis:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC1"),
          selectInput("plot_y", "Y-axis:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC2"),
          checkboxInput("equal_axis_dim", "Equal Axes", value = TRUE),
          uiOutput("plot_color_ui"),  # Color by options
          uiOutput("dynamic_pca_palette_ui"),  # Palette options (if any)
          actionButton("run", "Run Analysis", class = "btn-primary btn-block"),
          actionButton("plot_update", "Plot", class = "btn-block")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Results",
                     fluidRow(
                       column(
                         width = 12,
                         div(
                           style = "overflow-x: auto; white-space: nowrap;",
                           DTOutput("dim_table")
                         )
                       )
                     ),
                     plotlyOutput("dim_plot"),
                     uiOutput("additional_plots_ui")
            )
          )
        )
      )
    )
  )
)


# Define server ----------------------------------------------------------------
server <- function(input, output, session) {

  # Input data -----------------------------------------------------------------
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
  observeEvent(c(input$sample_id, input$data_file), {
    req(data(), input$sample_id)
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


  # Preprocessing Data ---------------------------------------------------------
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
    selected_columns <- trimws(selected_columns)
    if (any(!selected_columns %in% names(data))) {
      showNotification("Some column names are invalid. Please check your input.", type = "error")
      return()
    }

    if (input$cols_keep > input$cols_remove) {
      # Keep selected columns
      showNotification("Selected columns: ", type = "message")
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

  observeEvent(input$remove_na_button, {
    req(processed_data())

    cleaned_data <- hd_omit_na(processed_data())

    processed_data(cleaned_data)

    showNotification("Rows with NAs have been removed from the dataset.", type = "message")
  })

  # Plot Section --------------------------------------------------------------
  # Placeholder for dynamic palette configuration
  palette_data <- reactiveVal(NULL)

  observeEvent(input$color_variable, {
    req(input$color_variable)

    # if color variable not in merged data
    if (!input$color_variable %in% colnames(merged_data())) {
      showNotification("Color variable not found in the merged data.", type = "error")
      return(NULL)
    }

    merged <- merged_data()
    plot_data <- merged %>% select(all_of(c(input$sample_id, input$color_variable)))
    c_type <- hd_detect_vartype(plot_data[[input$color_variable]], unique_threshold = 5)

    # Dynamically render UI elements based on variable type
    if (c_type == "continuous") {
      output$dynamic_palette_ui <- renderUI({
        tagList(
          fluidRow(
            column(4, textInput("low_color", "Low Color", value = "#5e4fa2")),
            column(4, textInput("middle_color", "Middle Color", value = "#ffffbf")),
            column(4, textInput("high_color", "High Color", value = "#9e0142"))
          )
        )
      })
    } else if (c_type == "categorical") {
      output$dynamic_palette_ui <- renderUI({
        tagList(
          fluidRow(
            column(4, textInput("categories", "Categories", value = "")),
            column(4, textInput("category_colors", "Colors", value = ""))
          ),
        )
      })
    }
  })

  # Process the user input to generate the palette
  observeEvent(input$plot_button, {
    req(input$color_variable)
    merged <- merged_data()
    plot_data <- merged %>% select(all_of(c(input$sample_id, input$color_variable)))
    c_type <- hd_detect_vartype(plot_data[[input$color_variable]], unique_threshold = 5)

    if (c_type == "continuous") {
      # Collect continuous palette input
      low_color <- input$low_color
      middle_color <- input$middle_color
      high_color <- input$high_color
      low_limit <- 0
      middle_limit <- 0.5
      high_limit <- 1

      if (!is.null(low_color) && !is.null(middle_color) && !is.null(high_color)) {
        palette <- list(
          list(low_limit, low_color),
          list(middle_limit, middle_color),
          list(high_limit, high_color)
        )
        palette_data(palette)
      } else {
        palette_data(NULL)
      }
    } else if (c_type == "categorical") {
      # Collect categorical palette input
      categories <- strsplit(input$categories, ",\\s*")[[1]]
      colors <- strsplit(input$category_colors, ",\\s*")[[1]]

      if (length(categories) == length(colors) && length(categories) > 0) {
        palette <- setNames(colors, categories)
        palette_data(palette)  # Update the palette
      } else if (length(categories) == 0) {
        palette_data(NULL)
      } else {
        palette_data(NULL)  # Reset palette if mismatched inputs
        showNotification("Number of categories and colors must match!", type = "error")
      }
    }
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
    if ((x_var %in% colnames(merged) && y_var %in% colnames(merged)) || (x_var %in% colnames(merged) && y_var == "") || (x_var == "" && y_var %in% colnames(merged))) {
      if (input$color_variable != "" && input$color_variable %in% colnames(merged)) {
        color_var <- input$color_variable
        columns <- c(sample_id, x_var, y_var, color_var)
        columns <- columns[columns != ""]
        plot_data <- merged %>% select(all_of(columns))

        if (x_var != "") {
          x_type <- hd_detect_vartype(plot_data[[x_var]], unique_threshold = 5)
        } else {
          x_type <- ""
        }
        if (y_var != "") {
          y_type <- hd_detect_vartype(plot_data[[y_var]], unique_threshold = 5)
        } else {
          y_type <- ""
        }
        c_type <- hd_detect_vartype(plot_data[[color_var]], unique_threshold = 5)

        custom_palette <- palette_data()

        if (x_type == "continuous" && y_type == "continuous") {
          if (c_type == "categorical") {
            plot <- plot_ly(
              data = plot_data,
              x = ~get(x_var),
              y = ~get(y_var),
              color = ~get(color_var),
              type = "scatter",
              mode = "markers",
              text = ~get(sample_id),
              colors = if (!is.null(custom_palette)) custom_palette else NULL,
              hoverinfo = "text"
            )
          } else {
            custom_scale <- if (!is.null(custom_palette)) {
              lapply(custom_palette, function(item) list(item[[1]], item[[2]]))
            } else NULL

            plot <- plot_ly(
              data = plot_data,
              x = ~get(x_var),
              y = ~get(y_var),
              type = "scatter",
              mode = "markers",
              marker = list(
                color = ~get(color_var),
                colorscale = custom_scale,
                colorbar = list(title = paste("<b>", color_var, "</b>"))
              ),
              text = ~get(sample_id),
              hoverinfo = "text"
            )
          }

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
            return(NULL)
          }
          plot <- plot_ly(
            data = plot_data,
            x = ~get(x_var),
            y = ~get(y_var),
            color = ~get(color_var),
            colors = if (!is.null(custom_palette)) custom_palette else NULL,
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
        } else if (x_type == "categorical" && y_type == "categorical") {
          showNotification("Both x and y variables cannot be categorical.", type = "error")
        } else {
          if (c_type != "categorical") {
            showNotification("Color variable must be categorical for categorical x or y variable.", type = "error")
            return(NULL)
          }
          if (y_var == "") {

            plot <- plot_ly(
              data = plot_data,
              x = ~get(x_var),
              color = ~get(color_var),
              colors = if (!is.null(custom_palette)) custom_palette else NULL,
              type = "histogram"
            )
          } else {
            plot <- plot_ly(
              data = plot_data,
              y = ~get(y_var),
              color = ~get(color_var),
              colors = if (!is.null(custom_palette)) custom_palette else NULL,
              type = "histogram"
            )
          }
          plot <- plot %>%
            layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                   yaxis = list(title = paste("<b>", y_var, "</b>")),
                   legend = list(title = list(text = paste("<b>", color_var, "</b>"))))
        }
      } else {
        columns <- c(sample_id, x_var, y_var)
        columns <- columns[columns != ""]
        plot_data <- merged %>% select(all_of(columns))
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
        } else if (x_type == "categorical" && y_type == "categorical") {
          showNotification("Both x and y variables cannot be categorical.", type = "error")
        } else {
          if (y_var == "") {
            plot <- plot_ly(
              data = plot_data,
              x = ~get(x_var),
              type = "histogram"
            )
          } else {
            plot <- plot_ly(
              data = plot_data,
              y = ~get(y_var),
              type = "histogram"
            )
          }
          plot <- plot %>%
            layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
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


  # Dim Reduction Section ------------------------------------------------------
  dim_result <- reactiveVal(NULL)
  plot_settings <- reactiveValues(x_var = "PC1", y_var = "PC2", color_var = NULL)
  plot_variance <- reactiveVal(NULL)
  plot_loadings <- reactiveVal(NULL)

  # Dynamically render plot_color input
  observe({
    if (input$by_sample == FALSE) {
      updateSelectInput(session, "plot_color", choices = c(""), selected = "")
    } else {
      output$plot_color_ui <- renderUI({
        if (is.null(metadata())) {
          selectInput("plot_color", "Color By:", choices = c(""), selected = NULL, disabled = TRUE)
        } else {
          selectInput("plot_color", "Color By:", choices = c("", colnames(metadata())), selected = NULL)
        }
      })
    }
  })

  observeEvent(input$run, {
    req(processed_data(), metadata())

    if (input$method == "PCA") {
      res <- hd_auto_pca(
        dat = processed_data(),
        metadata = metadata(),
        by_sample = input$by_sample,
        components = input$components,
        plot_x = input$plot_x,
        plot_y = input$plot_y,
        plot_color = if (input$plot_color != "") input$plot_color else NULL
      )
      dim_result(res$pca_res) # Save PCA results for rendering
      plot_variance(res$pca_variance_plot)
      plot_loadings(res$pca_loadings_plot)
      plot_settings$x_var <- input$plot_x
      plot_settings$y_var <- input$plot_y
      plot_settings$color_var <- if (input$plot_color != "") input$plot_color else NULL
    } else {
      res <- hd_auto_umap(
        dat = processed_data(),
        metadata = metadata(),
        by_sample = input$by_sample,
        plot_x = "UMAP1",
        plot_y = "UMAP2",
        plot_color = if (input$plot_color != "") input$plot_color else NULL
      )
      dim_result(res$umap_res) # Save UMAP results for rendering
      plot_settings$x_var <- "UMAP1"
      plot_settings$y_var <- "UMAP2"
      plot_settings$color_var <- if (input$plot_color != "") input$plot_color else NULL
    }
  })

  # Update Plot Settings
  observeEvent(input$plot_update, {
    plot_settings$x_var <- input$plot_x
    plot_settings$y_var <- input$plot_y
    plot_settings$color_var <- if (input$plot_color != "") input$plot_color else NULL
  })

  # Render results table
  output$dim_table <- renderDT({
    req(dim_result())
    datatable(dim_result(), options = list(pageLength = 5))
  })

  observeEvent(input$plot_update, {
    req(plot_loadings(), plot_variance())

    # Dynamically update the UI to show the additional plots (variance and loadings)
    output$additional_plots_ui <- renderUI({
      tagList(
        plotOutput("pca_variance_plot"),
        plotOutput("pca_loadings_plot")
      )
    })
  })

  # Render PCA variance plot (only when "Plot" button is clicked)
  output$pca_variance_plot <- renderPlot({
    req(plot_variance())
    plot(plot_variance())
  })

  # Render PCA loadings plot (only when "Plot" button is clicked)
  output$pca_loadings_plot <- renderPlot({
    req(plot_loadings())
    plot(plot_loadings())
  })

  # Placeholder for dynamic palette configuration
  palette_data_dim <- reactiveVal(NULL)

  observeEvent(input$plot_color, {
    req(input$plot_color, metadata())

    # if color variable not in merged data
    if (!input$plot_color %in% colnames(metadata())) {
      showNotification("Color variable not found in the merged data.", type = "error")
      return(NULL)
    }

    metadata <- metadata() %>% select(all_of(c(input$sample_id, input$plot_color)))
    c_type <- hd_detect_vartype(metadata[[input$plot_color]], unique_threshold = 5)

    # Dynamically render UI elements based on variable type
    if (c_type == "continuous") {
      output$dynamic_pca_palette_ui <- renderUI({
        tagList(
          fluidRow(
            column(4, textInput("low_color_pca", "Low Color", value = "#5e4fa2")),
            column(4, textInput("middle_color_pca", "Middle Color", value = "#ffffbf")),
            column(4, textInput("high_color_pca", "High Color", value = "#9e0142"))
          )
        )
      })
    } else if (c_type == "categorical") {
      output$dynamic_pca_palette_ui <- renderUI({
        tagList(
          fluidRow(
            column(6, textInput("categories_pca", "Categories", value = "")),
            column(6, textInput("category_colors_pca", "Colors", value = ""))
          ),
        )
      })
    }
  })

  # Process the user input to generate the palette
  observeEvent(input$plot_update, {
    req(input$plot_color, metadata())
    metadata <- metadata() %>% select(all_of(c(input$sample_id, input$plot_color)))
    c_type <- hd_detect_vartype(metadata[[input$plot_color]], unique_threshold = 5)

    if (c_type == "continuous") {
      # Collect continuous palette input
      low_color <- input$low_color_pca
      middle_color <- input$middle_color_pca
      high_color <- input$high_color_pca
      low_limit <- 0
      middle_limit <- 0.5
      high_limit <- 1

      if (!is.null(low_color) && !is.null(middle_color) && !is.null(high_color)) {
        palette <- list(
          list(low_limit, low_color),
          list(middle_limit, middle_color),
          list(high_limit, high_color)
        )
        palette_data_dim(palette)
      } else {
        palette_data_dim(NULL)
      }
    } else if (c_type == "categorical") {
      # Collect categorical palette input
      categories <- strsplit(input$categories_pca, ",\\s*")[[1]]
      colors <- strsplit(input$category_colors_pca, ",\\s*")[[1]]

      if (length(categories) == length(colors) && length(categories) > 0) {
        palette <- setNames(colors, categories)
        palette_data_dim(palette)  # Update the palette
      } else if (length(categories) == 0) {
        palette_data_dim(NULL)
      } else {
        palette_data_dim(NULL)  # Reset palette if mismatched inputs
        showNotification("Number of categories and colors must match!", type = "error")
      }
    }
  })

  # Render plot
  observeEvent(c(input$plot_update && input$method == "PCA"), {
    req(dim_result(), metadata())
    plot_data <- dim_result()
    x_var <- plot_settings$x_var
    y_var <- plot_settings$y_var
    sample_id <- input$sample_id

    # Extract plot data and variables
    if (!x_var %in% colnames(plot_data) || !y_var %in% colnames(plot_data)) {
      showNotification("Selected variables not found in the data.", type = "error")
      return(NULL)
    }

    if (input$by_sample == TRUE && !sample_id %in% colnames(plot_data)) {
      showNotification("Features column not found in the data. Rerun the analysis with `By sample` checked.", type = "error")
      return(NULL)
    }

    if (input$by_sample == FALSE) {
      text_var <- "Features"
    } else {
      text_var <- sample_id
    }
    if (!is.null(plot_settings$color_var)) {
      color_var <- plot_settings$color_var

      plot_data <- plot_data |>
        left_join(metadata() |> select(all_of(c(input$sample_id, color_var))),
                  by = input$sample_id)

      c_type <- hd_detect_vartype(plot_data[[color_var]], unique_threshold = 5)

      custom_palette <- palette_data_dim()

      if (c_type == "categorical") {
        plot <- plot_ly(
          data = plot_data,
          x = ~get(x_var),
          y = ~get(y_var),
          color = ~get(color_var),
          type = "scatter",
          mode = "markers",
          text = ~get(text_var),
          colors = if (!is.null(custom_palette)) custom_palette else NULL,
          hoverinfo = "text"
        )
      } else {
        custom_scale <- if (!is.null(custom_palette)) {
          lapply(custom_palette, function(item) list(item[[1]], item[[2]]))
        } else NULL

        plot <- plot_ly(
          data = plot_data,
          x = ~get(x_var),
          y = ~get(y_var),
          type = "scatter",
          mode = "markers",
          marker = list(
            color = ~get(color_var),
            colorscale = custom_scale,
            colorbar = list(title = paste("<b>", color_var, "</b>"))
          ),
          text = ~get(text_var),
          hoverinfo = "text"
        )
      }

      if (input$equal_axis_dim) {
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
        layout(legend = list(title = list(text = paste("<b>", color_var, "</b>")))) |>
        colorbar(title = paste("<b>", color_var, "</b>"))

    } else {
      plot <- plot_ly(
        data = plot_data,
        x = ~get(x_var),
        y = ~get(y_var),
        type = "scatter",
        mode = "markers",
        text = ~get(text_var),
        hoverinfo = "text"
      )

      if (input$equal_axis_dim) {
        plot <- plot %>%
          layout(
            xaxis = list(title = paste("<b>", x_var, "</b>"), scaleanchor = "y"),
            yaxis = list(title = paste("<b>", y_var, "</b>"), scaleanchor = "x"))
      } else {
        plot <- plot %>%
          layout(xaxis = list(title = paste("<b>", x_var, "</b>")),
                 yaxis = list(title = paste("<b>", y_var, "</b>")))
      }
    }

    output$dim_plot <- renderPlotly({
      plot
    })
  })

  previous_method <- reactiveVal(NULL)

  # Observe changes in the method selection
  observeEvent(input$method, {
    current_method <- input$method
    prev_method <- previous_method()
    if (!is.null(prev_method) && current_method != prev_method) {
      # Clear all dimensionality reduction-specific outputs when the method changes
      output$additional_plots_ui <- renderUI({ NULL })  # Clear additional UI if any
      plot_variance(NULL)
      plot_loadings(NULL)

      if (input$method == "PCA") {
        # If PCA is selected, enable all components from PC1 to PC5
        updateNumericInput(session, "components", value = 10, min = 2, max = 10)

        # Update X and Y axes to PCA components
        updateSelectInput(session, "plot_x", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC1")
        updateSelectInput(session, "plot_y", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC2")

      } else if (input$method == "UMAP") {
        # If UMAP is selected, set the components strictly to 2 and update axes accordingly
        updateNumericInput(session, "components", value = 2, min = 2, max = 2)

        # Update X and Y axes to UMAP1 and UMAP2
        updateSelectInput(session, "plot_x", choices = c("UMAP1", "UMAP2"), selected = "UMAP1")
        updateSelectInput(session, "plot_y", choices = c("UMAP1", "UMAP2"), selected = "UMAP2")
      }

      # Print to console or log (optional, for debugging)
      print(paste("Method switched to:", input$method, "- Outputs cleared."))
    }

    previous_method(current_method)

  })
}


# Run the app
shinyApp(ui = ui, server = server)
