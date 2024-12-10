# **London Marathon Analysis 2024**
## *Overview*
This repository contains the analysis of the London Marathon 2024 dataset, focusing on marathon finish times across various countries and categories (Elite and Mass). The primary objective of this analysis is to visualize and explore the distribution of finish times, identify patterns, and compare performances between different countries.

The analysis involves:

- Data processing and cleaning
- Country-level performance analysis
- Visualisation of the data using interactive plots
- A Shiny app to filter and visualize marathon performance by country, continent, and category

Features:
- Interactive Violin Plot: Visualises the distribution of marathon finish times across countries, highlighting differences between continents and categories.
- Data Filtering: Users can filter the data by continent, marathon category (Elite or Mass), and minimum number of participants.
- Dynamic Titles: The Shiny app dynamically updates the plot title and other outputs based on user-selected filters.

Also included is other possible plots


## *Setup*
*Prerequisites*

To run the analysis and Shiny app, you need to have the following packages installed in R:

library(shiny)
library(plotly)
library(rvest)
library(tidyverse)
library(countrycode)
library(rsconnect)

Data provided by London marathon is of free use, participants must opt out of allowing their data for use by third parties

## Acknowledgments

Data Source: The data used in this analysis is sourced from the official London Marathon results and publicly available marathon data.

Shiny: This project utilizes the Shiny package for building interactive web applications in R.