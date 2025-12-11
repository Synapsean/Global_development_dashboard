#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# This Shiny app is built to read, filter and plot data from the World Bank Economic, Social, Environmental, Health, Education, Development and Energy data
# This app comes with built in datasets for Ireland (IRL), Colombia (COL), Spain (ESP), The Phillipines (PHL) and The USA (USA)
# More country data can be found here
# https://data.humdata.org/dataset/?q=Economic%2C+Social%2C+Environmental%2C+Health%2C+Education%2C+Development+and+Energy&sort=title_case_insensitive+asc&dataseries_name=World+Bank+-+Economic+Social+Environmental+Health+Education+Development+and+Energy

# Additional datasets can be added by downloading the csv file for a given country and adding to the .data/ folder
# and the app will update with the data available.

library(shiny)
library(ggplot2)
library(data.table)
library(janitor)
library(dplyr)
library(DT)
library(bslib)
library(plotly)

data_path = file.path(getwd(), "data")

# List all CSV files in the 'data/' directory
csv_files_full_paths = list.files(data_path, pattern = "^indicators_([a-z]{3})\\.csv$", full.names = TRUE, ignore.case = TRUE)

# Extract just the filenames (basename) from the full path
csv_base_names = basename(csv_files_full_paths)

# Extract unique country codes from these basenames (e.g., "irl", "col", "esp")
# Convert to uppercase for display consistency
available_countries = sort(unique(toupper(gsub("^indicators_([a-z]{3})\\.csv$", "\\1", csv_base_names, ignore.case = TRUE))))

#This curated list of indicators was created, as when plotting there are too many options to create a dropdown menu
# This list can be extended based on the indicator of interest.
curated_indicators = c(
  "Population, total", "Population, male (% of total population)", "Population, female (% of total population)", "Rural population (% of total population)", "Urban population (% of total population)",
  "GDP (current US)", "GDP per capita (current US$)", "Imports of goods and services (% of GDP)", "Military expenditure (% of GDP)",
  "Life expectancy at birth, total (years)", "Life expectancy at birth, female (years)", "Life expectancy at birth, male (years)",
  "Total greenhouse gas emissions excluding LULUCF (Mt CO2e)",
  "School enrollment, primary (gross % of total)", "School enrollment, secondary (% gross)", "School enrollment, tertiary (% gross)",
  "Access to electricity (% of population)", "Current health expenditure (% of GDP)", "Suicide mortality rate (per 100,000 population)", "Number of maternal deaths"

)

#This is for the second plot- it will be fixed using the indicator "GDP"
fixed_indicator_plot2 = "GDP (current US$)"


##This is a helper function to load, read and clean the csv files so it can be called later
load_and_process_country_data = function(country_code_to_load, base_data_path) {
  current_country_code = tolower(country_code_to_load)
  file_name = paste0("indicators_", current_country_code, ".csv")
  full_file_path = file.path(base_data_path, file_name)

  data = data.table::fread(full_file_path, header = TRUE)
  data = janitor::clean_names(data)

  # Assign Correct Classes (Type Coercion)
  numeric_cols = c("year", "value")
  for (col in intersect(numeric_cols, names(data))) {
    data[, (col) := suppressWarnings(as.numeric(trimws(get(col))))]
  }

  factor_cols = c("country_name", "country_iso3", "indicator_name", "indicator_code")

  # Handle 'country_code' derived from filename if needed
  if (!"country_code" %in% names(data)) {
    data[, country_code := toupper(current_country_code)]
    factor_cols = c(factor_cols, "country_code")
  } else {
    if ("country_code" %in% names(data) && !("country_code" %in% factor_cols)) {
      factor_cols = c(factor_cols, "country_code")
    }
  }

  for (col in intersect(factor_cols, names(data))) {
    data[, (col) := as.factor(get(col))]
  }

  return(data)
}

###UI###
# US for application to read, select and display data from the World Bank
ui = page_sidebar(
  theme = bs_theme(bootswatch = "zephyr"), # Professional, clean theme
  title = "Global Development Dashboard",

  sidebar = sidebar(
    title = "Controls",

    # Global Input: Affects everything
    selectInput("selected_country", "Primary Country:",
                choices = available_countries, selected = available_countries[1]),

    hr(),

    # --- Contextual Inputs using conditionalPanel ---
    # Only show these when the "Trends" tab is active
    conditionalPanel(
      condition = "input.main_tabs == 'Trends'",
      h5("Trend Settings"),
      uiOutput("indicator_selector"),
      numericInput("plot1_bin_size", "Bin Years (Smoothing):", value = 1, min = 1, step = 1)
    ),

    # Only show these when the "Comparison" tab is active
    conditionalPanel(
      condition = "input.main_tabs == 'Comparison'",
      h5("Comparison Settings"),
      selectInput("plot2_countries_select", "Compare Countries (Max 5):",
                  choices = available_countries, multiple = TRUE, selected = head(available_countries, 2)),
      uiOutput("plot2_year_range_slider"),
      selectInput("plot2_plot_type", "Chart Style:",
                  choices = c("Lines & Points", "Lines Only", "Points Only"))
    ),

    # Only show these when "Data Table" is active
    conditionalPanel(
      condition = "input.main_tabs == 'Data Table'",
      h5("Table Settings"),
      uiOutput("table_column_selector")
    ),

    hr(),
    helpText("Data Source: World Bank (2024).")
  ),

  # Main Content Area with Tabs
  navset_card_underline(
    id = "main_tabs", # ID is crucial for conditionalPanel logic above

    nav_panel("Trends",
              card_body(plotlyOutput("indicator_plot", height = "500px"))
    ),

    nav_panel("Comparison",
              card_body(plotlyOutput("multi_country_plot", height = "500px"))
    ),

    nav_panel("Data Table",
              DT::dataTableOutput("indicator_table")
    ),

    nav_panel("About",
              p("This dashboard visualizes World Bank development indicators."),
              p("Use the sidebar to filter countries and select specific metrics."),
              tags$ul(
                tags$li("Trends: View single-country performance over time."),
                tags$li("Comparison: Compare GDP across multiple nations."),
                tags$li("Data Table: Explore the raw dataset.")
              )
    )
  )
)


#########Server########
# --- Server Logic ---
server = function(input, output, session) {

  # 1. Load Data
  country_data_reactive = reactive({
    req(input$selected_country)
    load_and_process_country_data(input$selected_country, data_path)
  })

  # 2. Dynamic Indicators
  available_indicators_for_country = reactive({
    req(country_data_reactive())
    data = country_data_reactive()
    if(nrow(data) == 0) return(NULL)

    intersect(sort(unique(data$indicator_name)), curated_indicators)
  })

  # 3. Dynamic Columns
  available_table_columns = reactive({
    req(country_data_reactive())
    sort(names(country_data_reactive()))
  })

  # 4. Filter Data for Plot 1
  filtered_data_for_plot = reactive({
    req(input$selected_indicator, country_data_reactive(), input$plot1_bin_size)

    plot_data_raw = country_data_reactive() %>%
      filter(indicator_name == input$selected_indicator) %>%
      mutate(value = as.numeric(value)) %>%
      filter(!is.na(value))

    if (nrow(plot_data_raw) == 0) return(data.frame())

    bin_size = as.numeric(input$plot1_bin_size)
    if (bin_size > 1) {
      min_year = min(plot_data_raw$year, na.rm = TRUE)
      plot_data_raw %>%
        mutate(year = floor((year - min_year) / bin_size) * bin_size + min_year) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
    } else {
      plot_data_raw
    }
  })

  # 5. Filter Data for Plot 2 (Comparison)
  plot2_multi_country_data_reactive = reactive({
    req(input$plot2_countries_select)

    # Validation: Professional error handling
    validate(
      need(length(input$plot2_countries_select) >= 2, "Please select at least 2 countries."),
      need(length(input$plot2_countries_select) <= 5, "Please select no more than 5 countries.")
    )

    combined_data_list = lapply(input$plot2_countries_select, load_and_process_country_data, base_data_path = data_path)
    combined_data = data.table::rbindlist(combined_data_list, fill=TRUE) # fill=TRUE handles missing cols gracefully

    combined_data %>%
      filter(indicator_name == fixed_indicator_plot2) %>%
      mutate(value = as.numeric(value)) %>%
      filter(!is.na(value))
  })

  plot2_filtered_data = reactive({
    req(plot2_multi_country_data_reactive(), input$plot2_year_range)
    plot2_multi_country_data_reactive() %>%
      filter(year >= input$plot2_year_range[1] & year <= input$plot2_year_range[2])
  })

  # --- UI Outputs ---

  output$indicator_selector = renderUI({
    choices = available_indicators_for_country()
    if (is.null(choices)) return(helpText("No data available."))
    selectInput("selected_indicator", "Indicator:", choices = choices)
  })

  output$table_column_selector = renderUI({
    choices = available_table_columns()
    default = intersect(c("country_name", "year", "indicator_name", "value"), choices)
    checkboxGroupInput("selected_table_columns", "Columns:", choices = choices, selected = default)
  })

  output$plot2_year_range_slider = renderUI({
    req(plot2_multi_country_data_reactive())
    data = plot2_multi_country_data_reactive()
    rng = range(data$year, na.rm = TRUE)
    sliderInput("plot2_year_range", "Year Range:", min = rng[1], max = rng[2], value = rng, step = 1, sep = "")
  })


  #
  # ### ALL OUTPUT RENDERING FUNCTIONS (TOP LEVEL)
  #

  # Render the dynamic Indicator Selection dropdown
  output$indicator_plot = renderPlotly({
    plot_data = filtered_data_for_plot()

    validate(need(nrow(plot_data) > 0, "No data available for this selection."))

    p = ggplot(plot_data, aes(x = year, y = value)) +
      geom_line(color = "#2c3e50", size = 1) + # Custom hex color
      geom_point(color = "#e74c3c", size = 2, aes(text = paste("Year:", year, "<br>Value:", round(value, 2)))) +
      labs(title = input$selected_indicator, x = "Year", y = "Value") +
      theme_minimal()

    ggplotly(p, tooltip = "text") # Make interactive, use custom tooltip
  })

  output$multi_country_plot = renderPlotly({
    plot_data = plot2_filtered_data()

    validate(need(nrow(plot_data) > 0, "No overlapping data for selected range."))

    p = ggplot(plot_data, aes(x = year, y = value, color = country_name)) +
      labs(title = "GDP Comparison", x = "Year", y = "GDP (USD)") +
      scale_y_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) + # Nice $1B formatting
      theme_minimal() +
      theme(legend.title = element_blank())

    if (input$plot2_plot_type %in% c("Lines & Points", "Lines Only")) p = p + geom_line(size=1)
    if (input$plot2_plot_type %in% c("Lines & Points", "Points Only")) p = p + geom_point(size=2)

    ggplotly(p)
  })

  output$indicator_table = DT::renderDataTable({
    req(country_data_reactive(), input$selected_table_columns)

    # Safe column selection
    cols = intersect(input$selected_table_columns, names(country_data_reactive()))

    datatable(
      country_data_reactive()[, ..cols],
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
