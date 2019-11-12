library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(purrr)
library(tibble)
library(DT)
library(plotly)

source("helper.R")

server <- function(input, output, session) {
  
  # Reactive functions ------------------------------------------------------
  
  # XRF data related functions in order of dependency
  
  XRFdffun <- reactive({
    # XRFdffun()
    
    ## uses the file input(s) and runs makeTidyData from helper.R for every file. The function then checks for duplicate elements and removes the Element/Voltage combo with lower overall counts
    
    ## About makeTidyData(): The function removes Rh traces, as these are contained twofold (Rh-Coh, Rh-Inc) and these would lead to ambiguity if a we select by Element. The function also assumes that the first Depth of the core is starting with an offset (usually around 45-50 mm for the green stuff in short cores). However, in long core sections, we don't have an offset (but there is no easy way to tell if we do). Thus, we set Depth(i) = Depth(i) - min(Depth) + 1 (effectively zeroeing any first measurement and then adding 1). Because of this, the depths might be shifted by 1 mm. This is in most cases not relevant as it is consistent for every core processed here.
    files <- req(input$xrfdata)
    withProgress(message = 'Processing XRF files',
                 detail = 'This may take a while...',
                 value = 0,
                 {
                   tmp <-
                     map2(
                       files$datapath,
                       1:length(files$datapath),
                       makeTidyData,
                       length(files$datapath)
                     ) %>%
                     bind_rows()
                 })
    xrftbl <- tmp %>%
      group_by(Element, Voltage) %>%
      summarise(totalcounts = sum(Area)) %>%
      arrange(Element, desc(totalcounts)) %>%
      top_n(1, totalcounts) %>%
      select(Element, Voltage) %>%
      left_join(tmp, by = c("Element", "Voltage")) %>%
      select(
        CoreID,
        Depth,
        Date,
        Time,
        Duration,
        Run,
        Rep,
        Voltage,
        Current,
        Filter,
        SlitDown,
        SlitCross,
        Excitation,
        Throughput,
        Element,
        everything()
      ) %>%
      ungroup()
    
    xrftbl
  })
  
  sectionsfun <- reactive({
    # sectionsfun()
    ## This reactive function adds, if catmode == TRUE, a new column with the overall depth z. The function expects a letter at the end of the CoreID to denote the section number. The order can be chosen to be ascending (A section at surface, B deeper, and so on) or descending (A deepest section, B next one, so on).
    ## At this point there is no support for other Longcore names/CoreID schemes.
    if (input$catmode) {
      sectdf <- XRFdffun() %>%
        group_by(CoreID) %>%
        summarise(Length = max(Depth)) %>%
        mutate(
          SectionID = str_extract(CoreID, "[[:alpha:]]+$"),
          LongcoreName = str_extract(CoreID, ".+(?=\\W[[:alpha:]]+)")
        )
      
      if (any(is.na(sectdf$LongcoreName))) {
        shinyalert(
          "Multiple cores",
          HTML(
            "Multiple longcore names were found but you chose to use concatenate mode. Make sure to use the same name and latin letters for sections at the end. <br> E.g: XYZ19-8-A or WXYZ-HIJ1-E"
          ),
          type = "error",
          html = TRUE
        )
      }
      
      if (input$descorder) {
        sectdf <- sectdf %>%
          arrange(desc(CoreID)) %>%
          mutate(z = cumsum(Length), z = c(0, z[-length(z)]))
        sectdf
      } else {
        sectdf <- sectdf %>%
          arrange(CoreID) %>%
          mutate(z = cumsum(Length), z = c(0, z[-length(z)]))
        sectdf
      }
    }
  })
  
  processedXRFdf <- reactive({
    # processedXRFdf()
    ## This function joins the depth/length information generated before 
    if (input$catmode) {
      xrfdata <- sectionsfun() %>%
        left_join(XRFdffun(), by = "CoreID") %>%
        mutate(z = z + Depth)
      xrfdata
    } else {
      XRFdffun()
    }
  })
  
  XRFdf_norep <- reactive({
    # XRFdf_norep()
    ## Function that calculates relative standard deviation (error in percent) and also initializes the excludeddata reactivevalue and removes replicates. NB: This function should be integrated into processedXRFdf() if we don't care about replicates, since we don't need processedXRFdf() anywhere else.
    processedXRFdf <- processedXRFdf() %>%
      filter(Rep %in% "Rep0") %>%
      mutate(relstdev = cpsStd / cps * 100) %>%
      ungroup()
    uniquecoreid <- unique(req(processedXRFdf$CoreID))
    excludeddata$exclelem <-
      setNames(vector("list", length(uniquecoreid)), uniquecoreid)
    excludeddata$excldepth <-
      setNames(vector("list", length(uniquecoreid)), uniquecoreid)
    print("Echoing XRFdf_norep()...")
    print(processedXRFdf)
  })
  
  XRFdf_coreid <- reactive({
    # XRFdf_coreid()
    ## This function filters the dataset per CoreID for the diagnostics page. That way we don't need to filter in plot commands.
    curcore <- req(input$core_id)
    tmpxrf <- XRFdf_norep() %>%
      filter(CoreID %in% curcore)
    tmpxrf
  })
  
  XRFdf_cleaned <- reactive({
    # XRFdf_cleaned()
    ## This function uses the data from the reactive Value excludeddata to filter out deselected measurements and elements. In the end, negative cps (physically impossible and troublesome for log ratios) are removed as well.
    XRFdata <- req(XRFdf_norep())
    print("Echoing excludeddata")
    print(excludeddata$exclelem)
    print(excludeddata$excldepth)
    
    if (!is_empty(excludeddata$exclelem)) {
    elemdf <-
      map_dfr(excludeddata$exclelem, ~ enframe(., name = NULL), .id = "CoreID") %>%
      mutate(elem_removed = TRUE) %>%
      rename(Element = value)
    }
    
    if (!is_empty(excludeddata$excldepth)) {
    depthdf <-
      map_dfr(excludeddata$excldepth, ~ enframe(., name = NULL), .id = "CoreID") %>%
      mutate(depth_removed = TRUE) %>%
      rename(Depth = value)
    }
    
    if (!(nrow(elemdf) == 0 | nrow(depthdf) == 0)) {
      tmpxrf <- XRFdata %>%
        left_join(depthdf, by = c("CoreID", "Depth")) %>%
        left_join(elemdf, by = c("CoreID", "Element")) %>%
        replace_na(list(depth_removed = FALSE, elem_removed = FALSE)) %>%
        filter(!(depth_removed | elem_removed), cps > 0)
    } else {
      tmpxrf <- XRFdata %>%
        filter(cps > 0)
    }

    tmpxrf
  })
  
  XRFdf_plotting_1cXe <- eventReactive(input$plotting_redraw,{
    # XRFdf_plotting_1cXe()
    ## This function takes the cleaned XRF data and prepares it for plotting, adding additonal traces possibly and calculating proxies.
    
    ## under construction
    CoreID <- req(input$plotting_choose1c)
    XRFdata <- req(XRFdf_cleaned())
    proxies <- req(input$plotting_chooseproxies)
    additionaltraces$filenames <- req(input$plotting_addtraces$name)
    
    
    additionaltraces$tracesdata <- map(input$plotting_addtraces$datapath, ~(read_delim(.x, delim = ";") %>% rename(Depth = 1)))
    addtraces_df <- reduce(additionaltraces$tracesdata, full_join, by = "Depth")
    
    
    if (!is_empty(proxies)) {
      
    }

    
    calcproxy <- function(x){
      
      XRFdata %>% 
        group_by(Depth) %>%
        mutate(!!paste(x, "ratio", sep = "_") := cps/cps[Element %in% !!x]) %>% 
        ungroup() %>%
        select(!!paste(x, "ratio", sep = "_") )
    }
    
    XRF_plotdf <- XRFdata %>% 
      group_modify(~map_dfc(proxies, calcproxy)) %>% 
      bind_cols(XRFdata, .) %>%
      arrange(Depth, Element)
    
    XRF_plotdf
  })
  
  # Other reactive functions
  
  currentxlims <- reactive({
    # currentxlims()
    ## We can't read from reactive values inside reactiveValues(), so we use reactive() to read out the current chosen CoreID and the maximum depth to create a vector for the x limits.
    xlims = c(min(XRFdf_coreid()$Depth), max(XRFdf_coreid()$Depth))
  })
  
  ## here we create an empty reactiveValue to hold our deselected data of all CoreIDs
  excludeddata <- reactiveValues()
  
  elementsdiag <- reactive({
    input$elements_diag
  })
  
  elementsdiag_debounced <- debounce(elementsdiag, 1000)
  
  ## empty reactiveValue to hold filenames and data of additional traces
  # additionaltraces <- reactiveValues()

# Observers ---------------------------------------------------------------
  
  # Observers for import page
  
  observe({
    ## Here we initialise selectize inputon the import page for the preview element when catmode == TRUE.
    updateSelectizeInput(
      session,
      'catpreview_element',
      choices = unique(XRFdf_norep()$Element),
      server = TRUE
    )
  })
  
  observeEvent(input$catmode, {
    ## When catmode == TRUE, we show the preview plot and overview table on the import page, otherwise we hide it.
    toggleState("descorder")
    toggleState("catpreview_element")
  })
  
  # Observers for diagnostics page
  
  observe({
    ## Here we initialise the X limits range slide on the diagnostics page
    updateSliderInput(
      session,
      "xlimdiag",
      value = currentxlims(),
      min = currentxlims()[1],
      max = currentxlims()[2]
    )
  })
  
  observe({
    ## Here we initialise the CoreID selectize input on the diagnostics page
    updateSelectizeInput(
      session,
      'core_id',
      choices = unique(XRFdf_norep()$CoreID),
      server = TRUE
    )
  })
  
  observe({
    ## Here we initialise and update the Element picker on the diagnostics page. The control gets reset when the CoreID is changed (invalidated dependency).
    updatePickerInput(
      session,
      'elements_diag',
      selected = unique(XRFdf_coreid()$Element),
      choices = unique(XRFdf_coreid()$Element)
    )
  })
  
  observe({
    ## Here we check which Elements are deselected and write them into the excludeddata$exclelem list per CoreID. This means that changing to a CoreID that already has an exclelem list that is not NULL will overwrite the content! NB: Avoiding to overwrite existing data with NULL from another CoreID is not smart because the input gets initialized with all Elements.
    deselected <-
      setdiff(unique(XRFdf_coreid()$Element), input$elements_diag)
    print(deselected)
    excludeddata$exclelem[[input$core_id]] <- deselected
  })
  
  observeEvent(input$exclude_rmpoints, {
    ## Only executed when actionbutton "remove points" is clicked. We choose all possible fields of interest in brushedDepths to be able to brush in every of the three plot modes. Then we append the data to the list excldepth, making sure not to write duplicates.
    brushedDepths <- XRFdf_coreid() %>%
      select(CoreID, Depth, Element, relstdev, Chi2, cps)
    selpoints <-
      brushedPoints(brushedDepths, input$diagnosticsplot_brush)
    
    excludeddata$excldepth[[input$core_id]] <-
      c(selpoints$Depth,
        setdiff(excludeddata$excldepth[[input$core_id]], selpoints$Depth))
  })
  
  observeEvent(input$exclude_reset, {
    ## Only executed when the "reset" button on the diagnostics page is pressed: Removes all excluded depths from the excldepth list.
    excludeddata$excldepth[[input$core_id]] <- NULL
  })
  
  # Observers for plotting page
  
  ## not correctly initialized currently
  observe({
    cleaned <- XRFdf_cleaned()
    print("Echoing the observer for plotting_chosse1c cleaned df")
    print(cleaned)
    updateSelectizeInput(
      session,
      "plotting_choose1c",
      server = TRUE,
      choices = unique(cleaned$CoreID)
    )
  })

  observe({
    cleaned <- XRFdf_cleaned()
    clean_choices <- cleaned %>% filter(CoreID %in% input$plotting_choose1c) %>% select(Element) %>% distinct() %>% .[[1]]
    print("Echoing cleanchoices")
    print(clean_choices)
    updatePickerInput(
      session,
      "plotting_chooseproxies",
      choices = unique(clean_choices)
    )
  })
  
  # Observers for export page
  
  observeEvent(input$triggerbrowser, {
    browser()
  })

# Output code -------------------------------------------------------------
  
  # Outputs on import page
  
  output$sectiontable <- renderDT({
    ## Creating the sections table on the import page (only viewed if catmode == TRUE)
    sectionsfun()}, 
    options = list(paging = FALSE, searching = FALSE))
  
  output$catplot <- renderPlot({
    catpreview <- XRFdf_norep() %>%
      filter(Element == req(input$catpreview_element))
    ggplot(catpreview, aes(x = z, y = cps)) + geom_line(aes(colour = factor(SectionID))) + xlab("Depth [mm]") + scale_y_continuous("counts per second (cps)", limits = c(NA, NA)) + labs(color = "Section ID")
  })
  
  # Outputs on diagnostics page
  
  output$coreid_text <- renderText({
    ## Printing the chosen CoreID on the diagnostics page in the second column at the bottom.
    input$core_id
  })
  
  output$excltable <- renderDT({
    ## Printing a table showing deselected measurements and elements on the diagnostics page.
    df_elem <-
      map_dfr(req(excludeddata$exclelem), ~ enframe(., name = NULL), .id = "CoreID") %>%
      group_by(CoreID) %>%
      summarise(ExcludedElements = paste0(value, collapse = ", "))
    
    df_depths <-
      map_dfr(req(excludeddata$excldepth), ~ enframe(., name = NULL), .id = "CoreID") %>%
      group_by(CoreID) %>%
      summarise(ExcludedDepths = paste0(value, collapse = ", "))
    full_join(df_elem, df_depths)
  },
    options = list(paging = FALSE, searching = FALSE),
    rownames = FALSE)
  
  output$diagnosticsplot <- renderPlot({
    ## Diagnostic plots: Three different modes
    excldepths <- excludeddata$excldepth[[input$core_id]]
    
    if (input$diagmode == "relstdevdiag") {
      XRF_relstdev <- XRFdf_coreid()
      
      XRF_relstdev_cutoff <- XRF_relstdev %>%
        group_by(CoreID, Element) %>%
        summarise(keepmedquorum = median(abs(relstdev))) %>%
        left_join(XRF_relstdev, by = c("CoreID", "Element")) %>%
        mutate(
          keepmedquorum = ifelse(
            keepmedquorum <= req(input$medrelstdev),
            "Below",
            "Exceeded"
          ),
          keepmedquorum = as.factor(keepmedquorum)
        ) %>%
        filter(Element %in% req(elementsdiag_debounced()),
               !(Depth %in% excldepths))
      
      p <-
        ggplot(XRF_relstdev_cutoff, aes(x = Depth, y = relstdev)) + scale_fill_manual("Threshold", values = c(NA, "red")) + geom_rect(aes(
          xmin = -Inf,
          xmax = +Inf,
          ymin = -Inf,
          ymax = +Inf,
          fill = keepmedquorum
        ),
        alpha = 0.5) + geom_line() + scale_y_continuous(expression(paste(sigma[rel], " Percentage std. deviation")),
                                                        limits = c(input$diags_ymin, input$diags_ymax)) + geom_hline(aes(yintercept = input$maxrelstdev), colour = "red") + facet_wrap(CoreID ~
                                                                                                                                                                                         Element, scales = "free") + scale_x_continuous("Depth [mm]", limits = input$xlimdiag)
    }
    
    if (input$diagmode == "chisqdiag") {
      XRF_chisq <- XRFdf_coreid() %>%
        filter(Element %in% req(elementsdiag_debounced()),
               !(Depth %in% excldepths))
      p <-
        ggplot(XRF_chisq, aes(x = Depth, y = Chi2)) + geom_line() + scale_y_continuous("goodness of fit",
                                                                                       limits = c(input$diags_ymin, input$diags_ymax)) + facet_wrap( ~ Element, scales = "free") + geom_hline(aes(yintercept = input$chi2cutoff), colour = "blue") + scale_x_continuous("Depth [mm]", limits = input$xlimdiag)
    }
    
    if (input$diagmode == "cpsdiag") {
      XRF_cps <- XRFdf_coreid() %>%
        filter(Element %in% req(elementsdiag_debounced()),
               !(Depth %in% excldepths))
      p <-
        ggplot(XRF_cps, aes(x = Depth, y = cps)) + geom_line() + scale_y_continuous("cps", limits = c(input$diags_ymin, input$diags_ymax)) + facet_wrap( ~
                                                                                                                                                           Element, scales = "free") + scale_x_continuous("Depth [mm]", limits = input$xlimdiag)
    }
    p
  })
  
  # Outputs on plotting page
  
  output$plotUIinputs <- renderUI({
    
    ## The following code generates the plot mode dependent input elements
    req(input$plotting_mode)
    switch(input$plotting_mode,
           "1cXe" = {returnList <- tagList(
             selectizeInput("plotting_choose1c", "2. Choose Core", choices = NULL),
             pickerInput(
               inputId = "plotting_chooseproxies",
               label = "3. Calculate ratios? Choose proxies",
               choices = NULL,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             fileInput("plotting_addtraces", "4. Upload additional traces (*.csv)", multiple = TRUE, accept = c(
               "text/csv",
               "text/comma-separated-values,text/plain",
               ".csv")
             ),
             pickerInput(
               inputId = "plotting_choosetraces",
               label = "5. Choose traces to plot",
               choices = NULL,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             actionButton("plotting_redraw", "Compute & (Re)draw!", class = "btn-primary")
           )},
           
           "Xc1e" = {returnList <- tagList(
             pickerInput(
               inputId = "plotting_chooseXc",
               label = "2. Choose Cores",
               choices = NULL,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             selectizeInput("plotting_choose1e", "3. Choose Element", choices = NULL)
           )},
           
           "longcore" = {returnList <- tagList(
             pickerInput(
               inputId = "plotting_chooseproxies_longcore",
               label = "2. Calculate ratios? Choose proxies",
               choices = NULL,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             pickerInput(
               inputId = "plotting_choosetraces_longcore",
               label = "3. Choose traces to plot",
               choices = NULL,
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             actionButton("plotting_redraw_longcore", "Compute & (Re)draw!", class = "btn-primary")
           )}
           )
    returnList
  })
  
  # Outputs on statistics page
  
  
  output$cleandftest <- renderPrint(glimpse(XRFdf_plotting_1cXe()))
  
}