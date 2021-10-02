library(plotly)
library(tidyverse)
library(stringr)
library(shiny)
library(BH)

source("Group_ui.R")
source("Group_server.R")

shinyApp(ui = Group_ui, server = Group_server)