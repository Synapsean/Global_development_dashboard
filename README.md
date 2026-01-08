Global Development Tracker

An interactive dashboard to visualize and compare World Bank economic, social, and health indicators across multiple nations.

**[Launch Live App](https://k2cbr8-sean-quinlan.shinyapps.io/global-dev-tracker/)**

Interactive dashboard visualizing World Bank development indicators across 5 countries (more to come!).
Built with R Shiny to demonstrate reactive data visualisation and complex filtering logic.

## Key Features
* **Reactive Filtering:** Users can dynamically slice data by Year, Country, and GDP metrics.
* **Data Cleaning:** ETL script included (`clean_data.R`) that normalizes messy World Bank CSV exports.
* **Visualization:** Uses `ggplot2` and `plotly` for interactive timeseries analysis.

## Setup
1. Open `app.R` in RStudio.
2. Install required packages:
   ```r
   install.packages(c("shiny", "dplyr", "ggplot2", "plotly"))
3. Click *Run App*
