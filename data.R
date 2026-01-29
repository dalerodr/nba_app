a <- Sys.time()

library(tidyverse)          # Easily Install and Load the 'Tidyverse'
library(shiny)              # Web Application Framework for R
library(shinydashboard)     # Create Dashboards with 'Shiny'
library(bslib)              # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
library(lubridate)          # Make Dealing with Dates a Little Easier
library(here)               # A Simpler Way to Find Your Files
library(feather)            # R Bindings to the Feather 'API'
library(nbastatR)           # R's interface to NBA data
library(shinyWidgets)       # Custom Inputs Widgets for Shiny
library(fontawesome)        # Easily Work with 'Font Awesome' Icons
library(shinycustomloader)  # Custom Loader for Shiny Outputs
library(c3)                 # 'C3.js' Chart Library
library(overviewR)          # Easily Extracting Information About Your Data
library(ggthemes)           # Extra Themes, Scales and Geoms for 'ggplot2'
library(ggplot2)            # Create Elegant Data Visualisations Using the Grammar of Graphics
library(plotly)             # Create Interactive Web Graphics via 'plotly.js'
library(DT)                 # A Wrapper of the JavaScript Library 'DataTables'
library(rsconnect)          # Deployment Interface for R Markdown Documents and Shiny Applications
library(reactablefmtr)      # Easily Customize Interactive Tables Made with Reactable
library(scatterD3)          # D3 JavaScript Scatterplot from R
library(sever)              # Customise 'Shiny' Disconnected Screens and Error Messages

# devtools::install_github("abresler/nbastatR")

`%notin%` <- Negate(`%in%`)


# FASTER file reading but BIGGER file size
game_data <- read_feather(paste0(here(), "/data_public/game_data_public.feather"))
players <- read_feather(paste0(here(), "/data_public/players_public.feather"))


# NESTED ALL DATA (1975:2020 and 2021)
current_year <- year(date(Sys.Date()))

teams_logos <- read_feather(paste0(here(), "/data_public/teams_logos.feather"))
teams_logos_NT <- read_feather(paste0(here(), "/data_public/teams_logos_NT.feather"))

b <- Sys.time()
b-a

rm(a, b)
