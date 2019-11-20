library(shiny)
library(shinyjqui)
library(shinyjs)
library(reactlog)
library(shinyalert)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(shiny.info)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(purrr)
library(tibble)
library(DT)

gg2themes <- list(
  "Standard ggplot2 theme" = theme_grey(),
  "Black/white theme" = theme_bw(),
  "Light theme" = theme_light(),
  "Dark theme" = theme_dark(),
  "Minimal theme" = theme_minimal(),
  "Classic/R base theme" = theme_classic()
)