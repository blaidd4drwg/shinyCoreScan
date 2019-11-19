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
          "import_xrffiles",
          "Upload bAXIL batch files (*.csv)",
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"),
          multiple = TRUE
        ),
        hr(),
        checkboxInput("import_catmode", "Concatenate core sections"),
        checkboxInput("import_descorder", "Sort sections in descending order", value = TRUE),
        selectizeInput("import_catpreview_element",
                       "Element for long core preview",
                       choices = NULL),
        width = 3
      ),
      
      mainPanel(
        conditionalPanel("input.import_catmode == 1", plotOutput("catplot")),
        DTOutput("sectiontable")
      )
    )
  ),
  
  ## Diagnostics page ####
  tabPanel(
    "Diagnostics",
    selectizeInput('diagnostics_core_id', 'Choose Core ID', NULL),
    withSpinner(
      plotOutput(
        "diagnostics_diagnosticsplot",
        height = 600,
        click = "diagnostics_diagnosticsplot_click",
        brush = brushOpts(id = "diagnostics_diagnosticsplot_brush")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(
        3,
        h4("Plot options"),
        
        selectizeInput(
          "diagnostics_diagmode",
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
            inputId = "diagnostics_ymin",
            label = "Y min",
            value = NA
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          numericInput(
            inputId = "diagnostics_ymax",
            label = "Y max",
            value = NA
          )
        ),
        
        sliderInput(
          "diagnostics_xlims",
          label = "X limits (Core length)",
          min = 0,
          max = 100,
          value = c(40, 60)
        ),
        numericInput(
          "diagnostics_medrelstdev",
          HTML("Mark |&sigma;<sub>rel</sub>| median spectrum threshold"),
          value = 99
        ),
        numericInput(
          "diagnostics_maxrelstdev",
          HTML("&sigma;<sub>rel</sub>: Draw horizontal line"),
          value = 20
        ),
        numericInput(
          "diagnostics_chi2cutoff",
          HTML("&chi;<sup>2</sup>: Draw horizontal line"),
          value = 5
        )
        
      ),
      column(
        3,
        h4(HTML(paste(
          "Exclude data for", textOutput("diagnostics_coreid_text", inline = TRUE)
        ))),
        pickerInput(
          inputId = "diagnostics_elements_excl",
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
        actionButton("diagnostics_exclude_rmpoints", "Remove points", class = "btn-warning"),
        br(),
        br(),
        actionButton("diagnostics_exclude_reset", "Reset current core", class = "btn-danger"),
        br()
      ),
      column(6,
             h4(HTML(
               "Overview of changes"
             )),
             DTOutput("diagnostics_excltable"))
    )
  ),
  ## Plotting page ####
  tabPanel("Plotting",
           selectizeInput(
             inputId = "plotting_mode",
             label = "Select plot mode",
             choices = c("Upload data first"),
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
               selectizeInput("plotting_choose_1cXe", "Choose Core", choices = NULL), # only shown for 1cXe mode
               pickerInput("plotting_choose_Xc1e", "Choose multiple cores", choices = NULL, multiple = TRUE), # only shown for Xc1e mode
               textOutput("plotting_longcorename"), # only shown for longcore mode
               pickerInput(
                 inputId = "plotting_chooseproxies",
                 label = "Calculate ratios? Choose proxies",
                 choices = NULL,
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE
               ), # always shown
               fileInput("plotting_addtraces", "Upload additional traces (*.csv)", multiple = TRUE, accept = c(
                 "text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
               ), # only shown for 1cXe mode
               actionButton("plotting_removetraces", "Remove additional traces", class = "btn-danger"), # only shown for 1cXe mode
               p(),
               actionButton("plotting_compute", "Compute traces", class = "btn-primary"),
               p(),
               pickerInput(
                 inputId = "plotting_choosetraces",
                 label = "Choose traces to plot",
                 choices = NULL,
                 options = list(`actions-box` = TRUE, `live-search` = TRUE),
                 multiple = TRUE
               ), # shown for 1cXe mode and longcore mode
               pickerInput(
                 inputId = "plotting_choosetrace_Xc1e",
                 label = "Choose trace to plot",
                 choices = NULL,
                 options = list(`actions-box` = TRUE, `live-search` = TRUE),
                 multiple = FALSE
               ), # shown for Xc1e mode
               actionButton("plotting_redraw", "(Re)draw!", class = "btn-primary"), # always shown
               p()
             )
           )),
  ## Statistics page ####
  tabPanel("Statistics", ),
  
  ## Export page ####
  tabPanel("Export",
           actionButton("triggerbrowser", "Trigger browser( )"))
)