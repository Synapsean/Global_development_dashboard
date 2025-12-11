Global Development Tracker 🌍

![Status](https://img.shields.io/badge/Status-Live-success) ![R](https://img.shields.io/badge/Made%20with-R%20%7C%20Shiny-blue)

A professional interactive dashboard to visualize and compare World Bank economic, social, and health indicators across multiple nations.

👉 **[Launch Live App](https://k2cbr8-sean-quinlan.shinyapps.io/global-dev-tracker/)**

## 📊 Overview
This application transforms raw World Bank datasets into actionable insights. 
It allows users to move beyond static reports and explore trends dynamically. Users can analyse individual country performance over decades or compare economic metrics (like GDP) between nations side-by-side.

**Key Features:**
* **Interactive Visualization:** Implemented `plotly` for hover-able data points and dynamic scaling.
* **Modern UI/UX:** Built with `bslib` using the 'Zephyr' theme for a clean, responsive professional interface.
* **Comparative Analysis:** dedicated tools to compare GDP growth between 2-5 countries simultaneously.
* **Data Smoothing:** User-adjustable binning to visualize long-term trends vs. yearly fluctuations.

## 🛠️ Tech Stack
* **Core:** R, Shiny
* **Visualization:** ggplot2, Plotly
* **Data Manipulation:** dplyr, data.table, janitor
* **UI/Theming:** bslib, DT (DataTables)
* **Dependency Management:** renv (Ensures full reproducibility)

## 📂 Project Structure
```text
.
├── app.R                # Main application logic (UI & Server)
├── data/                # Cleaned datasets (CSV)
├── renv/                # Project library (dependency isolation)
├── renv.lock            # Lockfile recording exact package versions
└── README.md            # Project documentation
