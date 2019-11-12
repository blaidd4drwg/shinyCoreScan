library(shiny)
library(shinyjqui)
library(shinyjs)
library(reactlog)
library(shinyalert)
library(DT)
library(shinyWidgets)
library(shinycssloaders)

options(shiny.reactlog = TRUE)

ui <- navbarPage(
  "shinyCoreScan",
  
  ## Import page ####
  tabPanel(
    "Data import",
    useShinyjs(),
    useShinyalert(),
    sidebarLayout(
      sidebarPanel(
        h4("Data import"),
        fileInput(
          "xrfdata",
          "Upload bAXIL batch files (*.csv)",
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"),
          multiple = TRUE
        ),
        hr(),
        checkboxInput("catmode", "Concatenate core sections"),
        checkboxInput("descorder", "Sort sections in descending order", value = TRUE),
        selectizeInput("catpreview_element",
                       "Element for long core preview",
                       choices = NULL),
        width = 3
      ),
      
      mainPanel(
        conditionalPanel("input.catmode == 1", plotOutput("catplot")),
        DTOutput("sectiontable")
      )
    )
  ),
  
  ## Diagnostics page ####
  tabPanel(
    "Diagnostics",
    selectizeInput('core_id', 'Choose Core ID', NULL),
    withSpinner(
      plotOutput(
        "diagnosticsplot",
        height = 600,
        click = "diagnosticsplot_click",
        brush = brushOpts(id = "diagnosticsplot_brush")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(
        3,
        h4("Plot options"),
        
        selectizeInput(
          "diagmode",
          "Diagnostic mode",
          choices = list(
            "Element spectra (cps)" = "cpsdiag",
            "Relative standard deviation" = "relstdevdiag",
            "Goodness of fit" = "chisqdiag"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          numericInput(
            inputId = "diags_ymin",
            label = "Y min",
            value = NA
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          numericInput(
            inputId = "diags_ymax",
            label = "Y max",
            value = NA
          )
        ),
        
        sliderInput(
          "xlimdiag",
          label = "X limits (Core length)",
          min = 0,
          max = 100,
          value = c(40, 60)
        ),
        numericInput(
          "medrelstdev",
          HTML("Mark |&sigma;<sub>rel</sub>| median spectrum threshold"),
          value = 99
        ),
        numericInput(
          "maxrelstdev",
          HTML("&sigma;<sub>rel</sub>: Draw horizontal line"),
          value = 20
        ),
        numericInput(
          "chi2cutoff",
          HTML("&chi;<sup>2</sup>: Draw horizontal line"),
          value = 5
        )
        
      ),
      column(
        3,
        h4(HTML(paste(
          "Exclude data for", textOutput("coreid_text", inline = TRUE)
        ))),
        pickerInput(
          inputId = "elements_diag",
          label = "Include/Exclude Elements",
          choices = NULL,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        ),
        
        hr(),
        p(strong("Exclude depths")),
        HTML(
          paste0(
            "To exclude measurements, select/brush them on the plot and choose 'Remove points'. ",
            span(
              style = "color:red",
              "Brushed measurements are removed for every element of a core! NB: Measurements with negative cps values are always automatically excluded."
            )
          )
        ),
        br(),
        br(),
        actionButton("exclude_rmpoints", "Remove points", class = "btn-warning"),
        br(),
        br(),
        actionButton("exclude_reset", "Reset current core", class = "btn-danger"),
        br()
      ),
      column(6,
             h4(HTML(
               "Overview of changes"
             )),
             DTOutput("excltable"))
    )
  ),
  ## Plotting page ####
  tabPanel("Plotting",
           selectizeInput(
             inputId = "plotting_mode",
             label = "Select plot mode",
             choices = c("1 Core (Section) - X Elements" = "1cXe",
                         "X Cores (Sections) - 1 Element" = "Xc1e",
                         "Longcore" = "longcore"),
             multiple = FALSE
           ),
            plotOutput(
               "plotting_plotout",
               height = 600,
               click = "plotting_plotout_click",
               brush = brushOpts(id = "plotting_plotout_brush")
             ),
           
           hr(),
           
           fluidRow(
             column(
               3,
               uiOutput("plotUIinputs"),
               br()
             )
           )),
  ## Statistics page ####
  tabPanel("Statistics", ),
  
  ## Export page ####
  tabPanel("Export",
           actionButton("triggerbrowser", "Trigger browser( )"),
           verbatimTextOutput("cleandftest"))
)