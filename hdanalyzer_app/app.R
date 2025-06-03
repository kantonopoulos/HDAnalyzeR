# Load required libraries
library(shiny)
library(shinybusy)
library(DT)
library(bslib)
library(HDAnalyzeR)
library(dplyr)
library(plotly)
library(ggplot2)

# Define custom themes
light_theme <- bs_theme(
  primary = "#883268",
)

# Define UI --------------------------------------------------------------------
ui <- tagList(
  add_busy_spinner(                    # This goes first, before the navbarPage
    spin = "fading-circle",
    color = "#883268",
    position = "bottom-right",
    timeout = 300
  ),
  navbarPage(
    title = "HDAnalyzeR",
    theme = light_theme,
    id = "main_navbar", # Identifier for navigation

    ## Page 1: Welcome & Data Upload ---------------------------------------------
    tabPanel(
      "Welcome & Data Upload",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            h3("Welcome!"),
            p("This app simplifies proteomics data analysis and biomarker discovery."),
            hr(),
            h4("Data"),
            p("The data file should have the sample identifiers in the first column. For more infomation check the Help tab."),
            fileInput("data_file",
                      "Upload Data File (.csv, .tsv, .txt, .xlsx, .rds, .rda, .parquet):",
                      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds", ".rda", ".parquet")),
            uiOutput("sample_id_ui"),
            uiOutput("variable_value_ui"),
            hr(),
            h4("Metadata"),
            p("The metadata should contain the same sample identifiers as the data file. For more infomation check the Help tab."),
            fileInput("metadata_file",
                      "Upload Metadata File (.csv, .tsv, .txt, .xlsx, .rds, .rda, .parquet):",
                      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds", ".rda", ".parquet")),
            textOutput("validation_msg"),
            hr(),

            # Filtering Section
            h4("Sample Filtering"),
            p("Filter samples based on metadata variables. Select a metadata column and choose one or more categories."),
            selectInput("metadata_filter_column", "Metadata Filtering Column:", choices = NULL),
            uiOutput("metadata_filter_choices_ui"),
            fluidRow(
              column(6, actionButton("metadata_keep", "Keep", class = "btn-block")),
              column(6, actionButton("metadata_remove", "Remove", class = "btn-block btn-primary"))
            ),
            tags$style(".btn-block { width: 100%; margin-top: 10px; }"),
            hr(),
            h4("Feature Filtering"),
            uiOutput("columns_to_keep_ui"),
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
                ),
                downloadButton("download_data", "Download Data CSV", class = "btn-block")
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
                ),
                downloadButton("download_metadata", "Download Metadata CSV", class = "btn-block")
              )
            ),
            # Plot Section - Place below the data preview section
            hr(),
            h4("Exploratory Data Analysis"),
            fluidRow(
              column(4,
                    selectizeInput("x_variable", "X Variable", choices = NULL, selected = NULL)
              ),
              column(4,
                    selectizeInput("y_variable", "Y Variable", choices = NULL, selected = NULL)
              ),
              column(4,
                    selectizeInput("color_variable", "Color Variable", choices = NULL, selected = NULL)
              ),
            ),
            uiOutput("dynamic_palette_ui"),
            checkboxInput("equal_axis", "Equal Axes", value = TRUE),
            fluidRow(
              column(6,
                    actionButton("plot_button", "Plot", class = "btn-block", style = "height: 50px;")
              )
            ),
            plotlyOutput("scatter_plot", height = 500)
          )
        )
      )
    ),

    ## Page 2: Dimensionality Reduction ------------------------------------------
    tabPanel(
      "Dimensionality Reduction",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            h4("Analysis Settings"),
            selectInput("method", "Choose Method:", choices = c("PCA", "UMAP")),
            numericInput("components", "Number of Components:", value = 10, min = 2, step = 1),
            checkboxInput("by_sample", "By Sample", value = TRUE),
            actionButton("run", "Run Analysis", class = "btn-primary btn-block"),
            hr(),
            h4("Plot Settings"),
            selectInput("plot_x", "X-axis:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC1"),
            selectInput("plot_y", "Y-axis:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC2"),
            checkboxInput("equal_axis_dim", "Equal Axes", value = TRUE),
            uiOutput("plot_color_ui"),  # Color by options
            uiOutput("dynamic_pca_palette_ui"),  # Palette options (if any)
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
                      downloadButton("download_dim", "Download Results CSV", class = "btn-block"),
                      hr(),
                      plotlyOutput("dim_plot", height = 500),
                      uiOutput("additional_plots_ui")
              )
            )
          )
        )
      )
    ),

    ## Page 3: Differential Expression -------------------------------------------
    tabPanel(
      "Differential Expression",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            h4("Analysis Settings"),
            p("Differential expression analysis using the limma package."),
            uiOutput("de_variable_ui"),
            uiOutput("de_case_ui"),
            checkboxInput("de_log_transform", "Data Log Transform", value = FALSE),
            hr(),
            h4("Plot Settings"),
            numericInput("logfc_lim", "Log Fold Change limit:", value = 1, min = 0, step = 0.5),
            numericInput("pval_lim", "P-Value limit:", value = 0.05, min = 0, step = 0.001),
            textInput("non_c", "Non-signifficant Color:", value = "#DCDCDC"),
            textInput("down_c", "Down-regulated Color:", value = "#317EC2"),
            textInput("up_c", "Up-regulated Color:", value = "#C03830"),
            actionButton("de_run", "Run Analysis", class = "btn-primary btn-block")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Results",
                      fluidRow(
                        column(
                          width = 12,
                          div(
                            style = "overflow-x: auto; white-space: nowrap;",
                            DTOutput("de_table")
                          )
                        )
                      ),
                      downloadButton("download_de", "Download Results CSV", class = "btn-block"),
                      hr(),
                      plotlyOutput("de_plot", height = 500)
              )
            )
          )
        )
      )
    ),

    ## Page 4: Machine Learning --------------------------------------------------
    tabPanel(
      "Classification Model",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            h4("Analysis Settings"),
            p("Lasso regression will be used."),
            uiOutput("ml_variable_ui"),
            uiOutput("ml_case_ui"),
            uiOutput("multiclass_ui"),
            numericInput("ratio", "Train/Test Ratio:", value = 0.8, min = 0.1, max = 0.9, step = 0.1),
            numericInput("cv_sets", "Cross-validation sets:", value = 5, min = 2, max = 10, step = 1),
            uiOutput("ml_balance_ui"),
            uiOutput("ml_palette_ui"),
            actionButton("ml_run", "Run Analysis", class = "btn-primary btn-block")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Results",
                      dataTableOutput("ml_metrics"),
                      hr(),
                      dataTableOutput("ml_features"),
                      downloadButton("download_imp", "Download Feature Importance CSV", class = "btn-block"),
                      uiOutput("ml_plots_ui")
              )
            )
          )
        )
      )
    ),

    ## Page 5: Help --------------------------------------------------------------
    tabPanel(
      "Help",
      fluidPage(
        h2("📘 HDAnalyzeR User Guide"),
        br(),
        
        wellPanel(
          h4("1. Welcome & Data Upload Tab"),
          tags$ul(
            tags$li("Upload your data and metadata using the file inputs."),
            tags$li("Supported formats: .csv, .tsv, .txt, .xlsx, .rds, .rda, .parquet."),
            tags$li("For wide-format data: sample IDs in the first column, followed by expression columns."),
            tags$li("Include metadata (e.g., group, condition) in a separate file."),
            tags$li("Use RDS files for large datasets to reduce size (50MB limit)."),
            tags$li("Select sample ID, variable, and value columns as needed."),
            tags$li("Use filters to clean or reduce the dataset before analysis."),
            tags$li("Missing values are handled automatically (KNN imputation if not removed).")
          )
        ),

        wellPanel(
          h4("2. Exploratory Data Analysis"),
          tags$ul(
            tags$li("Choose X and/or Y variables to visualize data."),
            tags$li("Specify a Color variable for grouped or gradient plots."),
            tags$li("Plot type depends on data types (e.g., histogram, boxplot, scatterplot)."),
            tags$li("Use custom color palettes for more control."),
            tags$li("Click 'Plot' to generate interactive plots.")
          )
        ),

        wellPanel(
          h4("3. Dimensionality Reduction"),
          tags$ul(
            tags$li("Choose PCA or UMAP for analysis."),
            tags$li("Select number of components and analysis mode (by sample or feature)."),
            tags$li("Visualize results with customizable plots."),
            tags$li("PCA shows explained variance and loadings.")
          )
        ),

        wellPanel(
          h4("4. Differential Expression"),
          tags$ul(
            tags$li("Set case/control groups for comparison."),
            tags$li("Optionally correct for confounders via metadata variables."),
            tags$li("Configure logFC and p-value thresholds."),
            tags$li("Generate interactive volcano plots and tables.")
          )
        ),

        wellPanel(
          h4("5. Classification Model"),
          tags$ul(
            tags$li("Run binary or multi-class classification with lasso regression."),
            tags$li("Adjust train/test split and cross-validation settings."),
            tags$li("Results include accuracy metrics and visualizations (ROC, confusion matrix).")
          )
        ),

        wellPanel(
          h4("General Tips"),
          tags$ul(
            tags$li("Double-check column names in your inputs."),
            tags$li("Error messages and alerts will appear if something’s off."),
            tags$li("Use the Download buttons to save outputs."),
            tags$li("Some steps may take time — a spinner will indicate progress."),
            tags$li("For support, reach out to the development team.")
          )
        )
      )
    )
  )
)


# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2)
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
  filtered_metadata <- reactiveVal(NULL)
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
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 5))
  })

  # Preview of filtered metadata
  observeEvent(metadata(), {
    filtered_metadata(metadata())  # initialize filtered_metadata as metadata on load
  })
  output$metadata_preview <- DT::renderDT({
    req(filtered_metadata())
    datatable(filtered_metadata())
  })
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )

  output$download_metadata <- downloadHandler(
    filename = function() {
      paste0("metadata_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_metadata(), file, row.names = FALSE)
    }
  )
  # Validation message
  output$validation_msg <- renderText({
    if (is.null(input$data_file) || is.null(input$metadata_file)) {
      return("Please upload both data and metadata files to proceed.")
    }
    "Files uploaded successfully!"
  })


  # Preprocessing Data Section -------------------------------------------------
  # Perform Column Filtering
  observeEvent(input$cols_keep, {
    req(processed_data(), input$columns_to_keep)
    data <- processed_data()
  
    valid_cols <- input$columns_to_keep[input$columns_to_keep %in% names(data)]
    if (length(valid_cols) == 0) {
      showNotification("No valid columns selected.", type = "error")
      return()
    }
  
    updated <- data[, valid_cols, drop = FALSE]
    processed_data(updated)
    showNotification("Selected columns retained.", type = "message")
  })
  
  observeEvent(input$cols_remove, {
    req(processed_data(), input$columns_to_keep)
    data <- processed_data()
  
    valid_cols <- input$columns_to_keep[input$columns_to_keep %in% names(data)]
    updated <- data[, !(names(data) %in% valid_cols), drop = FALSE]
    processed_data(updated)
    showNotification("Selected columns removed.", type = "message")
  })
  

  output$columns_to_keep_ui <- renderUI({
    req(processed_data(), input$sample_id)
    choices <- setdiff(names(processed_data()), input$sample_id)
    selectInput("columns_to_keep", "Select Features:", choices = choices, multiple = TRUE)
  })

  # Perform category filtering
  observe({
    req(metadata())
    updateSelectInput(session, "metadata_filter_column", choices = colnames(metadata()))
  })

  output$metadata_filter_choices_ui <- renderUI({
    req(metadata(), input$metadata_filter_column)
    if (input$metadata_filter_column %in% colnames(metadata())) {
      choices <- sort(unique(metadata()[[input$metadata_filter_column]]))
      selectInput("metadata_filter_choices", "Select Categories:",
                  choices = choices, multiple = TRUE)
    }
  })

  observeEvent(input$metadata_keep, {
    req(metadata(), processed_data(), input$metadata_filter_column, input$metadata_filter_choices, input$sample_id)
    meta <- metadata()

    filtered_meta <- meta[ meta[[input$metadata_filter_column]] %in% input$metadata_filter_choices, ]
    filtered_ids <- filtered_meta[[input$sample_id]]

    # Subset the processed data: keep only rows with sample IDs in filtered_ids
    current_data <- processed_data()
    new_data <- current_data[ current_data[[input$sample_id]] %in% filtered_ids, ]

    processed_data(new_data)
    filtered_metadata(filtered_meta)
    showNotification("Processed data updated: Only samples with the selected metadata were kept.", type = "message")
  })

  observeEvent(input$metadata_remove, {
    req(metadata(), processed_data(), input$metadata_filter_column, input$metadata_filter_choices, input$sample_id)
    meta <- metadata()

    filtered_meta <- meta[ meta[[input$metadata_filter_column]] %in% input$metadata_filter_choices, ]
    filtered_ids <- filtered_meta[[input$sample_id]]

    # Subset the processed data: remove rows with sample IDs in filtered_ids
    current_data <- processed_data()
    new_data <- current_data[ !(current_data[[input$sample_id]] %in% filtered_ids), ]

    processed_data(new_data)
    filtered_metadata(filtered_meta)
    showNotification("Processed data updated: Samples with the selected metadata were removed.", type = "message")
  })

  merged_data <- reactive({
    req(processed_data(), metadata())  # Make sure both processed data and metadata are available
    # Merging processed data and metadata
    left_join(processed_data(), metadata(), by = input$sample_id)
  })

  # Reset button logic
  observeEvent(input$reset_data, {
    req(original_data(), metadata())
  
    processed_data(original_data())
    filtered_metadata(metadata())  # Reset metadata too
    showNotification("Data and metadata have been reset to original.", type = "message")
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

  ## Plot Section --------------------------------------------------------------
  # Placeholder for dynamic palette configuration
  palette_data <- reactiveVal(NULL)

  observeEvent(input$color_variable, {
    req(input$color_variable, processed_data(), metadata())
  
    merged <- full_join(processed_data(), metadata(), by = input$sample_id)

    if (!input$color_variable %in% names(merged)) {
      showNotification("Color variable not found in the merged data.", type = "error")
      return()
    }
  
    plot_data <- merged %>% select(all_of(c(input$sample_id, input$color_variable)))
    c_type <- hd_detect_vartype(plot_data[[input$color_variable]], unique_threshold = 5)
    
    if (c_type == "continuous") {
      output$dynamic_palette_ui <- renderUI({
        tagList(
          p("Enter hex codes for Low, Middle, and High values."),
          fluidRow(
            column(4, textInput("low_color", "Low Color", value = "#5e4fa2")),
            column(4, textInput("middle_color", "Middle Color", value = "#ffffbf")),
            column(4, textInput("high_color", "High Color", value = "#9e0142"))
          )
        )
      })
    } else if (c_type == "categorical") {
        merged[[input$color_variable]] <- as.character(merged[[input$color_variable]])
        merged[[input$color_variable]][is.na(merged[[input$color_variable]]) | merged[[input$color_variable]] == ""] <- "Missing"
        unique_categories <- unique(na.omit(plot_data[[input$color_variable]]))
    
        output$dynamic_palette_ui <- renderUI({
          unique_categories <- unique(plot_data[[input$color_variable]])

          tagList(
            p("Select categories and specify corresponding colors (comma-separated hex codes):"),
            fluidRow(
              column(6, selectInput("categories", "Categories",
                                    choices = unique_categories,
                                    selected = NULL,  # Don't preselect
                                    multiple = TRUE)),
              column(6, textInput("category_colors", "Colors",
                                  value = ""))  # Leave blank
            )
          )
        })
      }
    })
  

  # Process the user input to generate the palette
  observeEvent(input$plot_button, {
    req(input$color_variable)
    merged <- full_join(processed_data(), metadata(), by = input$sample_id)
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
      categories <- input$categories
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

  observe({
    req(processed_data(), metadata())
    all_vars <- union(colnames(processed_data()), colnames(metadata()))
  
    updateSelectizeInput(session, "x_variable", choices = c("", all_vars), server = TRUE)
    updateSelectizeInput(session, "y_variable", choices = c("", all_vars), server = TRUE)
    updateSelectizeInput(session, "color_variable", choices = c("", all_vars), server = TRUE)
  })

  observeEvent(input$plot_button, {
    req(processed_data())  # Ensure processed data is available

    # Create a merged data set (processed data + metadata)
    merged <- full_join(processed_data(), metadata(), by = input$sample_id)
    
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
        if (c_type == "categorical") {
          plot_data[[input$color_variable]] <- as.character(plot_data[[input$color_variable]])
          plot_data[[input$color_variable]][is.na(plot_data[[input$color_variable]]) | plot_data[[input$color_variable]] == ""] <- "Missing"
        }
        
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
      showNotification("Selected variables not found in the data or metadata.", type = "error")
    }
  })


  # Dim Reduction Section ------------------------------------------------------
  dim_result <- reactiveVal(NULL)
  plot_settings <- reactiveValues(x_var = "PC1", y_var = "PC2", color_var = NULL)
  plot_variance <- reactiveVal(NULL)
  plot_loadings <- reactiveVal(NULL)

  output$download_dim <- downloadHandler(
    filename = function() {
      paste0("dim_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dim_result(), file, row.names = FALSE)
    }
  )

  # Dynamically render plot_color input
  observe({
    if (input$by_sample == FALSE) {
      updateSelectInput(session, "plot_color", choices = c(""), selected = "")
    } else {
      output$plot_color_ui <- renderUI({
        if (is.null(metadata())) {
          selectInput("plot_color", "Color By:", choices = c(""), selected = NULL, disabled = TRUE)
        } else if ((input$by_sample == TRUE)) {
          selectInput("plot_color", "Color By:", choices = c("", colnames(metadata())), selected = NULL)
        } else {
          return(NULL)
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
        hr(),
        h4("Explained Variance"),
        plotlyOutput("pca_variance_plot", height = 500),
        hr(),
        h4("PCA Loadings"),
        plotlyOutput("pca_loadings_plot", height = 750)
      )
    })
  })

  # Render PCA variance plot
  output$pca_variance_plot <- renderPlotly({
    req(plot_variance())
    plotly_variance <- ggplotly(plot_variance()) %>%
      layout(legend = list(orientation = 'h', x = 0.25, y = 100))

    for (i in 2:3) {
      plotly_variance$x$data[[i]]$textposition <- "top"
    }

    plotly_variance$x$data[[1]]$name <- "Individual Variance Explained"
    plotly_variance$x$data[[2]]$name <- "Cumulative Variance Explained"

    plotly_variance
  })

  # Render PCA loadings plot
  output$pca_loadings_plot <- renderPlotly({
    req(plot_loadings())
    ggplotly(plot_loadings() +
               facet_wrap(~component, scales="free_y", ncol = 2)
    ) %>%
      layout(legend = list(title=list(text='<b> Sign </b>')))
  })

  # Placeholder for dynamic palette configuration
  palette_data_dim <- reactiveVal(NULL)

  observeEvent(c(input$plot_color, input$by_sample), {
    req(input$plot_color, metadata())

    # if color variable not in merged data
    if (!input$plot_color %in% colnames(metadata())) {
      showNotification("Color variable not found in the merged data.", type = "error")
      return(NULL)
    }

    metadata <- metadata() %>% select(all_of(c(input$sample_id, input$plot_color)))
    c_type <- hd_detect_vartype(metadata[[input$plot_color]], unique_threshold = 5)

    # Dynamically render UI elements based on variable type
    if (c_type == "continuous" && input$by_sample == TRUE) {
      output$dynamic_pca_palette_ui <- renderUI({
        tagList(
          textInput("low_color_pca", "Low Color", value = "#5e4fa2"),
          textInput("middle_color_pca", "Middle Color", value = "#ffffbf"),
          textInput("high_color_pca", "High Color", value = "#9e0142")
        )
      })
    } else if (c_type == "categorical" && input$by_sample == TRUE) {
        metadata[[input$plot_color]] <- as.character(metadata[[input$plot_color]])
        metadata[[input$plot_color]][is.na(metadata[[input$plot_color]]) | metadata[[input$plot_color]] == ""] <- "Missing"
        unique_categories <- unique(na.omit(metadata[[input$plot_color]]))
    
        output$dynamic_pca_palette_ui <- renderUI({
          unique_categories <- unique(metadata[[input$plot_color]])

        tagList(
          p("Select categories and specify corresponding colors (comma-separated hex codes):"),
          
          selectInput("categories_pca", "Categories", choices = unique_categories, selected = NULL, multiple = TRUE),
          textInput("category_colors_pca", "Colors", value = "")
        )
      })
    } else {
      output$dynamic_pca_palette_ui <- NULL
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
      categories <- input$categories_pca

      # Check if input$category_colors_pca exists and is a character string
      if (!is.null(input$category_colors_pca) && is.character(input$category_colors_pca)) {
        colors <- strsplit(input$category_colors_pca, ",\\s*")[[1]]
      } else {
        showNotification("Please enter colors for the categories.", type = "error")
        palette_data_dim(NULL)
        return()  # Stop further processing here
      }

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
      showNotification("Sample ID column not found in the data. Don't forget to rerun the analysis with `By sample` checked if you want to run the analysis by sample.", type = "error")
      return(NULL)
    }

    if (input$by_sample == FALSE && !c("Features") %in% colnames(plot_data)) {
      showNotification("Features column not found in the data. Don't forget to rerun the analysis with `By sample` un-checked if you want to run the analysis by feature.", type = "error")
      return(NULL)
    }

    if (input$by_sample == FALSE) {
      text_var <- "Features"
    } else {
      text_var <- sample_id
    }
    if (!is.null(plot_settings$color_var) && input$by_sample == TRUE) {
      color_var <- plot_settings$color_var

      plot_data <- plot_data |>
        left_join(metadata() |> select(all_of(c(input$sample_id, color_var))),
                  by = input$sample_id)
      
      c_type <- hd_detect_vartype(plot_data[[color_var]], unique_threshold = 5)

      custom_palette <- palette_data_dim()

      if (c_type == "categorical") {
        plot_data[[color_var]] <- as.character(plot_data[[color_var]])
        plot_data[[color_var]][is.na(plot_data[[color_var]]) | plot_data[[color_var]] == ""] <- "Missing"
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


  # Differential Expression Section --------------------------------------------
  de_result <- reactiveVal(NULL)

  output$download_de <- downloadHandler(
    filename = function() {
      paste0("de_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(de_result(), file, row.names = FALSE)
    }
  )

  # Dynamically render plot_color input
  observe({
    output$de_variable_ui <- renderUI({
      if (is.null(metadata())) {
        selectInput("de_variable", "Variable Containing Groups:", choices = c(""), selected = NULL, disabled = TRUE)
      } else {
        selectInput("de_variable", "Variable Containing Groups:", choices = c("", colnames(metadata())), selected = NULL)
      }
    })
  })

  observe({
    req(input$de_variable, metadata())
    v_type <- hd_detect_vartype(metadata()[[input$de_variable]], unique_threshold = 5)
    correct_choices <- setdiff(colnames(metadata()), c(input$sample_id, input$de_variable))
    if (v_type == "categorical") {
      # Get unique categories from de_variable column (remove NA)
      unique_vals <- sort(unique(na.omit(metadata()[[input$de_variable]])))

      output$de_case_ui <- renderUI({
        tagList(
          selectInput("de_case", "Case Group:", choices = unique_vals, selected = NULL),
          selectInput("de_control", "Control Group(s) (comma-separated):",
                      choices = unique_vals,
                      multiple = TRUE,
                      selected = NULL),
          p("If no control group is selected, all other groups will be considered as controls."),
          selectInput("de_correct", "Metadata Variables to Correct for:",
                    choices = correct_choices,
                    multiple = TRUE,
                    selected = NULL)
        )
      })
    } else {
      output$de_case_ui <- renderUI({
        tagList(
          selectInput("de_correct", "Metadata Variables to Correct for:",
                    choices = correct_choices,
                    multiple = TRUE,
                    selected = NULL)
        )
      })
    }
  })

  observeEvent(input$de_run, {
    req(processed_data(), metadata(), input$de_variable)

    de_control <- input$de_control
    if (is.null(de_control) || length(de_control) == 0 || all(de_control == "")) {
      de_control <- NULL
    }

    de_correct <- input$de_correct
    if (is.null(de_correct) || length(de_correct) == 0 || all(de_correct == "")) {
      de_correct <- NULL
    }

    if (input$de_variable %in% colnames(metadata())) {
      if ((input$de_case %in% metadata()[[input$de_variable]] && input$de_case != "") || is.null(input$de_case)) {
        if (all(de_control %in% metadata()[[input$de_variable]]) || is.null(de_control)) {
          if (all(de_correct %in% colnames(metadata())) || is.null(de_correct)) {
            if (!input$de_variable %in% de_correct) {
              tryCatch({
                res <- hd_de_limma(
                  dat = processed_data(),
                  metadata = metadata(),
                  variable = input$de_variable,
                  case = input$de_case,
                  control = de_control,
                  correct = de_correct,
                  log_transform = input$de_log_transform
                )
                de_result(res$de_res)
              }, error = function(e) {
                showNotification(paste("Error: Issue variable. Please try a different variable."), type = "error")
              })
              } else{
              showNotification("The variable to correct for cannot be the same as the variable containing the groups.", type = "error")
            }
          } else {
            showNotification("Selected correction variable(s) not found in the metadata.", type = "error")
          }
        } else {
          showNotification("Selected control group(s) not found in the metadata.", type = "error")
        }
      } else {
        showNotification("Selected case group not found in the metadata. Please select a valid case group.", type = "error")
      }
    } else {
      showNotification("Selected variable not found in the metadata.", type = "error")
    }
  })

  # Render results table
  output$de_table <- renderDT({
    req(de_result())
    datatable(de_result(), options = list(pageLength = 5))
  })

  # Process the user input to generate the palette
  palette_data_de <- reactiveVal(NULL)
  observe({
    req(input$non_c, input$down_c, input$up_c)
    # Collect categorical palette input
    categories <- c("non-significant", "down-regulated", "up-regulated")
    colors <- c(input$non_c, input$down_c, input$up_c)

    if (length(categories) == length(colors) && length(categories) > 0) {
      palette <- setNames(colors, categories)
      palette_data_de(palette)  # Update the palette
    } else {
      palette_data_de(NULL)  # Reset palette if mismatched inputs
      showNotification("Number of categories and colors must match!", type = "error")
    }
  })

  # Render plot
  observeEvent(input$de_run, {
    req(de_result())
    plot_data <- de_result() |>
      mutate(significance = case_when(
        adj.P.Val < input$pval_lim & logFC > input$logfc_lim ~ "up-regulated",
        adj.P.Val < input$pval_lim & logFC < -input$logfc_lim ~ "down-regulated",
        TRUE ~ "non-significant"
      )) |>
      mutate(pval_y = -log10(adj.P.Val))

    if (is.null(palette_data_de)) {
      showNotification("Some/all color is not selected. Using default colors.", type = "warning")
      palette_de <- setNames(
        c("#DCDCDC", "#317EC2", "#C03830"),
        c("non-significant", "down-regulated", "up-regulated")
      )
    } else {
      palette_de <- palette_data_de()
    }

    plot <- plot_ly(
      data = plot_data,
      x = ~logFC,
      y = ~pval_y,
      color = ~significance,
      type = "scatter",
      mode = "markers",
      text = ~Feature,
      colors = palette_de,
      hoverinfo = "text"
    ) %>%
      layout(shapes = list(
        list(
          type = "line",
          x0 = input$logfc_lim, x1 = input$logfc_lim,
          yref = "paper",
          y0 = 0, y1 = 1,
          line = list(color = "black", dash = "dash", width = 2)
        ),
        list(
          type = "line",
          x0 = -input$logfc_lim, x1 = -input$logfc_lim,
          yref = "paper",
          y0 = 0, y1 = 1,
          line = list(color = "black", dash = "dash", width = 2)
        ),
        # Horizontal dashed line
        list(
          type = "line",
          xref = "paper",
          x0 = 0, x1 = 1,
          y0 = -log10(input$pval_lim), y1 = -log10(input$pval_lim),
          line = list(color = "black", dash = "dash", width = 2)
        )
      ),
      xaxis = list(title = paste("<b>", "log2(Fold Change)", "</b>")),
      yaxis = list(title = paste("<b>", "-log10(Adjusted P-value)", "</b>")),
      showlegend = FALSE
    )

    output$de_plot <- renderPlotly({
      plot
    })
  })


  # Machine Learning Section ---------------------------------------------------
  ml_result <- reactiveVal(NULL)
  roc_curve <- reactiveVal(NULL)
  feat_imp_plot <- reactiveVal(NULL)
  comp_plot <- reactiveVal(NULL)
  cm_plot <- reactiveVal(NULL)
  # Dynamically render plot_color input
  observe({
    output$ml_variable_ui <- renderUI({
      if (is.null(metadata())) {
        selectInput("ml_variable", "Variable Containing Groups:", choices = c(""), selected = NULL, disabled = TRUE)
      } else {
        selectInput("ml_variable", "Variable Containing Groups:", choices = c("", colnames(metadata())), selected = NULL)
      }
    })
  })

  observe({
    req(input$ml_variable, metadata())
    v_type <- hd_detect_vartype(metadata()[[input$ml_variable]], unique_threshold = 5)
    if (v_type == "categorical" && isFALSE(input$multiclass)) {

      unique_vals <- sort(unique(na.omit(metadata()[[input$ml_variable]])))

      output$ml_case_ui <- renderUI({
        tagList(
          selectInput("ml_case", "Case Group:", choices = unique_vals, selected = NULL),
          selectInput("ml_control", "Control Group(s) (comma-separated):",
                      choices = unique_vals,
                      multiple = TRUE,
                      selected = NULL),
          p("If no control group is selected, all other groups will be considered as controls.")
        )
      })
    } else {
      output$ml_case_ui <- NULL
    }
  })

  observe({
    req(input$ml_variable, metadata())
    v_type <- hd_detect_vartype(metadata()[[input$ml_variable]], unique_threshold = 5)
    if (v_type == "categorical" && isFALSE(input$multiclass)) {
      output$ml_balance_ui <- renderUI({
        tagList(
          checkboxInput("balance_groups", "Balance Groups", value = TRUE)
        )
      })
    } else {
      output$ml_balance_ui <- NULL
    }
  })

  observe({
    req(input$ml_variable, metadata())
    v_type <- hd_detect_vartype(metadata()[[input$ml_variable]], unique_threshold = 5)
    if (v_type == "categorical") {
      output$multiclass_ui <- renderUI({
        tagList(
          checkboxInput("multiclass", "Multiclassification", value = FALSE)
        )
      })
    } else {
      output$multiclass_ui <- NULL
    }
  })

  observe({
    req(input$ml_variable, metadata())
    v_type <- hd_detect_vartype(metadata()[[input$ml_variable]], unique_threshold = 5)
    if (v_type == "categorical" && isFALSE(input$multiclass)) {
      output$ml_palette_ui <- renderUI({
        tagList(
          textInput("case_c", "Case Group Color:", value = "#883268")
        )
      })
    } else {
      output$ml_palette_ui <- NULL
    }
  })

  # Check if the hexcode is valid function
  is_valid_hexcode <- function(hex_string) {
    pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

    if (grepl(pattern, hex_string)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  feature_importance_data <- reactiveVal(NULL)
  observeEvent(input$ml_run, {
    req(processed_data(), metadata(), input$ml_variable)

    if (isFALSE(is.numeric(input$ratio)) || input$ratio < 0.1 || input$ratio > 0.9) {
      showNotification("Please enter a valid ratio (number between 0.1 and 0.9).", type = "error")
      return(NULL)
    }
    if (isFALSE(is.numeric(input$cv_sets)) || input$cv_sets < 2 || input$cv_sets > 10 || input$cv_sets %% 1 != 0) {
      showNotification("Please enter a valid number of CV sets (integer between 2 and 10).", type = "error")
      return(NULL)
    }

    ml_control <- input$ml_control
    if (is.null(ml_control) || length(ml_control) == 0 || all(ml_control == "")) {
      ml_control <- NULL
    }

    v_type <- hd_detect_vartype(metadata()[[input$ml_variable]], unique_threshold = 5)
    if (v_type == "categorical" && isFALSE(input$multiclass)) {
      ml_case <- input$ml_case
    } else {
      ml_case <- NULL
    }

    if (input$ml_variable %in% colnames(metadata())) {
      if ((ml_case %in% metadata()[[input$ml_variable]] && ml_case != "") || is.null(ml_case)) {
        if (all(ml_control %in% metadata()[[input$ml_variable]]) || is.null(ml_control)) {
          hd_split <- hd_split_data(
            processed_data(),
            metadata(),
            variable = input$ml_variable,
            ratio = input$ratio
          )

          if (is.null(ml_case) || input$case_c == "" || is.null(input$case_c) || !is_valid_hexcode(input$case_c)) {
            palette_c <- NULL
          } else {
            palette_c <- setNames(input$case_c, input$ml_case)
          }

          res <- hd_model_rreg(
            hd_split,
            variable = input$ml_variable,
            case = ml_case,
            control = ml_control,
            balance_groups = input$balance_groups,
            cor_threshold = 0.9,
            grid_size = 5,
            cv_sets = input$cv_sets,
            mixture = 1,
            palette = palette_c,
            plot_y_labels = FALSE,
            verbose = TRUE,
            plot_title = NULL,
            seed = 123
          )
          
          ml_result(res)
          output$ml_features <- renderDataTable({
            datatable(res$features)
          })
          feature_importance_data(res$features)
          if (v_type == "categorical") {
            if (isFALSE(input$multiclass)){
              metrics <- data.frame(
                Metric = c("Selected Features", "Accuracy", "Sensitivity", "Specificity", "AUC"),
                Value = c(nrow(res$features |> dplyr::filter(Scaled_Importance > 0)),
                          res$metrics$accuracy,
                          res$metrics$sensitivity,
                          res$metrics$specificity,
                          res$metrics$auc)
              )
              transposed_metrics <- as.data.frame(t(metrics[ , -1]))
              colnames(transposed_metrics) <- metrics$Metric
              output$ml_metrics <- renderDataTable({
                datatable(transposed_metrics) %>%
                  formatRound(columns = 1, digits = 0) %>%
                  formatRound(columns = c(2, 3, 4, 5), digits = 2)
              })
            } else {
              metrics <- data.frame(
                Metric = c("Selected Features", "Accuracy", "Sensitivity", "Specificity"),
                Value = c(nrow(res$features |> dplyr::filter(Scaled_Importance > 0)),
                          res$metrics$accuracy,
                          res$metrics$sensitivity,
                          res$metrics$specificity)
              )
              auc_data <- res$metrics$auc
              auc_data[[input$ml_variable]] <- paste("AUC", auc_data[[input$ml_variable]])

              combined_metrics <- rbind(
                metrics,
                data.frame(Metric = auc_data[[input$ml_variable]], Value = auc_data$AUC)
              )

              transposed_metrics <- as.data.frame(t(combined_metrics[ , -1]))
              colnames(transposed_metrics) <- combined_metrics$Metric

              output$ml_metrics <- renderDataTable({
                datatable(
                  transposed_metrics,
                  options = list(scrollX = TRUE)
                ) %>%
                  formatRound(columns = 1, digits = 0) %>%
                  formatRound(columns = seq(2, ncol(transposed_metrics)), digits = 2)
              })
            }

            roc_curve(res$roc_curve)
            feat_imp_plot(res$feat_imp_plot)
            cm_plot <- ggplot2::autoplot(res$metrics$confusion_matrix, type = "heatmap") +
              ggplot2::scale_fill_gradient(low = "white", high = input$case_c)
            cm_plot(cm_plot)
          } else {
              metrics <- data.frame(
                Metric = c("Selected Features", "RMSE", "RSQ"),
                Value = c(nrow(res$features |> dplyr::filter(Scaled_Importance > 0)), res$metrics$rmse, res$metrics$rsq)
              )
              transposed_metrics <- as.data.frame(t(metrics[ , -1]))
              colnames(transposed_metrics) <- metrics$Metric
              output$ml_metrics <- renderDataTable({
                datatable(transposed_metrics) %>%
                  formatRound(columns = 1, digits = 0) %>%
                  formatRound(columns = c(2, 3), digits = 2)
              })
              comp_plot(res$comparison_plot)
          }
        } else {
          showNotification("Selected control group(s) not found in the metadata.", type = "error")
        }
      } else {
        showNotification("Selected case group not found in the metadata. Please select a valid case group.", type = "error")
      }
    } else {
      showNotification("Selected variable not found in the metadata.", type = "error")
    }
  })

  observe({
    req(roc_curve(), feat_imp_plot())

    output$ml_plots_ui <- renderUI({
      tagList(
        hr(),
        h4("ROC Curve"),
        plotlyOutput("roc_curve_ml", height = 500),
        hr(),
        h4("Feature Importance"),
        plotlyOutput("feat_imp_plot_ml", height = 750),
        hr(),
        h4("Confusion Matrix"),
        plotlyOutput("cm_plot", height = 500)
      )
    })
  })

  observe({
    req(comp_plot())

    output$ml_plots_ui <- renderUI({
      tagList(
        hr(),
        plotlyOutput("comp_plot_ml", height = 500)
      )
    })
  })

  output$roc_curve_ml <- renderPlotly({
    req(roc_curve())
    ggplotly(roc_curve())
  })

  output$feat_imp_plot_ml <- renderPlotly({
    req(feat_imp_plot())
    ggplotly(feat_imp_plot())
  })

  output$comp_plot_ml <- renderPlotly({
    req(comp_plot())

    ggplotly(comp_plot())
  })

  output$cm_plot <- renderPlotly({
    req(cm_plot())
    ggplotly(cm_plot())
  })

  output$download_imp <- downloadHandler(
    filename = function() {
      paste0("feature_importance", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(feature_importance_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

