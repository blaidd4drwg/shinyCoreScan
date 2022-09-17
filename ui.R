options(shiny.reactlog = TRUE)

ui <- navbarPage(
  "shinyCoreScan",
  ## Import page ####
  tabPanel(
    "Data import",
    shiny.info::display("v0.9-beta.2"),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        h4("Data import"),
        radioButtons("import_choosesource", 
                     "Choose data source",
                     choices = list(
                       "Sample Data" = "sampledata",
                       "File Upload" = "fileupload"
                     ),
                     selected = "sampledata"),
        fileInput(
          "import_xrffiles",
          "Upload bAXIL batch files (*.csv)",
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"),
          multiple = TRUE
        ),
        actionButton("import_loaddata", "Load & Parse XRF data"),
        hr(),
        checkboxInput("import_catmode", "Concatenate core sections"),
        checkboxInput("import_descorder", "Sort sections in descending order", value = TRUE),
        selectizeInput("import_catpreview_element",
                       "Element for long core preview",
                       choices = NULL),
        width = 3
      ),
      
      mainPanel(
        plotOutput("catplot"),
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
        p(),
        actionButton("diagnostics_exclude_rmpoints", "Remove points", class = "btn-warning"),
        p(),
        actionButton("diagnostics_exclude_reset", "Reset current core", class = "btn-danger"),
        hr(),
        downloadButton("diagnostics_saveplot", "Download diagnostics plot"),
        p()
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
             options = list(placeholder = "Upload data first"),
             multiple = FALSE,
             choices = character(0)
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
               h4("Compute traces"),
               selectizeInput("plotting_choose_1cXe", "Choose Core", choices = NULL), # only shown for 1cXe mode
               pickerInput(
                 inputId = "plotting_choose_Xc1e", 
                 label = "Choose multiple cores", 
                 choices = NULL, 
                 multiple = TRUE,
                 options = list(`actions-box` = TRUE)), # only shown for Xc1e mode
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
               p()
             ),
             column(
               3,
               h4("Plot options"),
               selectizeInput(
                 inputId = "plotting_theme", 
                 label = "Choose plot theme", 
                 choices = names(gg2themes)
               ),
               actionButton("plotting_redraw", "(Re)draw plot", class = "btn-primary"), # always shown
               downloadButton("plotting_saveplot", "Download plot"),
               p()
             )
           ))#,
  ## Statistics page ####
  #tabPanel("Statistics", ),
  
  ## Export page ####
  #tabPanel("Export", )
)