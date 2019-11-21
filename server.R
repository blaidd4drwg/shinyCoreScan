source("helper.R")

server <- function(input, output, session) {
  
# Reactive functions ------------------------------------------------------
  
  # XRF data related functions in order of dependency
  
  XRFfun_tidydata <- reactive({
    # XRFfun_tidydata()
    
    ## uses the file input(s) and runs makeTidyData from helper.R for every file. The function then checks for duplicate elements and removes the Element/Voltage combo with lower overall counts
    
    ## About makeTidyData(): The function removes Rh traces, as these are contained twofold (Rh-Coh, Rh-Inc) and these would lead to ambiguity if a we select by Element. The function also assumes that the first Depth of the core is starting with an offset (usually around 45-50 mm for the green stuff in short cores). However, in long core sections, we don't have an offset (but there is no easy way to tell if we do). Thus, we set Depth(i) = Depth(i) - min(Depth) + 1 (effectively zeroeing any first measurement and then adding 1). Because of this, the depths might be shifted by 1 mm. This is in most cases not relevant as it is consistent for every core processed here.
    files <- req(input$import_xrffiles)
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
  
  XRFfun_makelongcore <- reactive({
    # XRFfun_makelongcore()
    ## This reactive function adds, if import_catmode == TRUE, a new column with the overall depth z. The function expects a letter at the end of the CoreID to denote the section number. The order can be chosen to be ascending (A section at surface, B deeper, and so on) or descending (A deepest section, B next one, so on).
    ## At this point there is no support for other Longcore names/CoreID schemes.
    if (input$import_catmode) {
      sectdf <- XRFfun_tidydata() %>%
        group_by(CoreID) %>%
        summarise(Length = max(Depth)) %>%
        mutate(
          SectionID = str_extract(CoreID, "[[:alpha:]]+$"),
          LongcoreName = str_extract(CoreID, ".+(?=\\W[[:alpha:]]+)")
        )
      
      validate(
        need(!(any(is.na(sectdf$LongcoreName))), message = "Multiple longcore names were found but you chose to use concatenate mode. Make sure to use the same name and latin letters for sections at the end. E.g: XYZ19-8-A or WXYZ-HIJ1-E")
      )
      
      if (input$import_descorder) {
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
  
  XRFfun_joinlongcore <- reactive({
    # XRFfun_joinlongcore()
    ## This function joins the depth/length information generated before 
    if (input$import_catmode) {
      xrfdata <- XRFfun_makelongcore() %>%
        left_join(XRFfun_tidydata(), by = "CoreID") %>%
        mutate(z = z + Depth)
      xrfdata
    } else {
      XRFfun_tidydata()
    }
  })
  
  XRFfun_rmrepeats <- reactive({
    # XRFfun_rmrepeats()
    ## Function that calculates relative standard deviation (error in percent) and also initializes the excludeddata reactivevalue and removes replicates. NB: This function should be integrated into XRFfun_joinlongcore() if we don't care about replicates, since we don't need XRFfun_joinlongcore() anywhere else.
    XRFfun_joinlongcore <- XRFfun_joinlongcore() %>%
      filter(Rep %in% "Rep0") %>%
      mutate(relstdev = cpsStd / cps * 100) %>%
      ungroup()
    uniquecoreid <- unique(req(XRFfun_joinlongcore()$CoreID))
    excludeddata$exclelem <-
      setNames(vector("list", length(uniquecoreid)), uniquecoreid)
    excludeddata$excldepth <-
      setNames(vector("list", length(uniquecoreid)), uniquecoreid)
    XRFfun_joinlongcore
  })
  
  XRFfun_diags_coreid <- reactive({
    # XRFfun_diags_coreid()
    ## This function filters the dataset per CoreID for the diagnostics page. That way we don't need to filter in plot commands.
    curcore <- req(input$diagnostics_core_id)
    tmpxrf <- XRFfun_rmrepeats() %>%
      filter(CoreID %in% curcore)
    tmpxrf
  })
  
  XRFfun_diags_cleaned <- reactive({
    # XRFfun_diags_cleaned()
    ## This function uses the data from the reactive Value excludeddata to filter out deselected measurements and elements. In the end, negative cps (physically impossible and troublesome for log ratios) are removed as well.
    XRFdata <- req(XRFfun_rmrepeats())
    
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
        filter(!(depth_removed | elem_removed))
    } else {
      tmpxrf <- XRFdata
    }

    tmpxrf
  })
  
  XRFfun_plotting_compute <- reactive({
    cleaned <- req(XRFfun_diags_cleaned())
    pmode <- req(input$plotting_mode)
    proxies <- plotting_proxies$proxylist
    
    calcproxy <- function(x) {
      XRFdata %>% 
        group_by(Depth) %>%
        mutate(!!paste(x, "ratio", sep = "_") := cps/cps[Element %in% !!x]) %>% 
        ungroup() %>%
        select(!!paste(x, "ratio", sep = "_"))
    }
    
    calcproxy_Xc1e <- function(x) {
      XRFdata_Xc1e %>% 
        group_by(CoreID, Depth) %>%
        mutate(!!paste(x, "ratio", sep = "_") := cps/cps[Element %in% !!x]) %>% 
        ungroup() %>%
        select(!!paste(x, "ratio", sep = "_"))
    }
    
    calcproxy_longcore <- function(x) {
      XRFdata_longcore %>% 
        group_by(z) %>%
        mutate(!!paste(x, "ratio", sep = "_") := cps/cps[Element %in% !!x]) %>% 
        ungroup() %>%
        select(!!paste(x, "ratio", sep = "_"))
    }
    
    switch(pmode,
           "1cXe" = {
             core_1c <- req(input$plotting_choose_1cXe)
             XRFdata <- cleaned %>%
               filter(CoreID %in% core_1c) %>%
               select(Depth, Element, cps)
             
             if (!is_empty(input$plotting_addtraces)) {
               additionaltraces$filenames <- input$plotting_addtraces$name
               
               additionaltraces$tracesdata <-
                 map(input$plotting_addtraces$datapath,
                     ~ (read_delim(.x, delim = ";")))

               validate(
                 need(
                   all(map_lgl(additionaltraces$tracesdata, ~ (colnames(.x)[1] == "Depth"))), 
                   message = "Depth not found. Please check the format of the csv file."
                   )
               )
               
               traces_plotting <-
                 reduce(additionaltraces$tracesdata, full_join, by = "Depth") %>%
                 mutate(Depth = 10 * Depth)
               
               browser()
             }
             
             if (is_empty(proxies) & is_empty(additionaltraces$filenames)) {
               xrf_plotting <- XRFdata %>% 
                 transmute(Depth = Depth, Varname = paste(.$Element, "cps"), Value = cps)
             }
             
             if (is_empty(proxies) & !is_empty(additionaltraces$filenames)) {
               xrf_plotting <- XRFdata %>% 
                 transmute(Depth = Depth, Varname = paste(.$Element, "cps"), Value = cps) %>% 
                 spread(Varname, Value) %>% 
                 full_join(traces_plotting, by = "Depth") %>%
                 gather(Varname, Value, -1) %>%
                 drop_na()
             }
             
             if (!is_empty(proxies) & is_empty(additionaltraces$filenames)) {
               xrf_plotting <- XRFdata %>% 
                 group_modify(~map_dfc(proxies, calcproxy)) %>% 
                 bind_cols(XRFdata, .) %>%
                 gather(Measure, Value, -c("Depth","Element")) %>%
                 mutate(Varname = if_else(str_detect(.$Measure, "ratio"), paste0(.$Element, "/", str_extract(.$Measure, "[:alpha:]+")), paste(.$Element, .$Measure))) %>%
                 select(-c("Element","Measure"))
             }
             
             if (!is_empty(proxies) & !is_empty(additionaltraces$filenames)) {
               xrf_plotting <- XRFdata %>% 
                 group_modify(~map_dfc(proxies, calcproxy)) %>% 
                 bind_cols(XRFdata, .) %>%
                 gather(Measure, Value, -c("Depth","Element")) %>%
                 mutate(Varname = if_else(str_detect(.$Measure, "ratio"), paste0(.$Element, "/", str_extract(.$Measure, "[:alpha:]+")), paste(.$Element, .$Measure))) %>%
                 select(-c("Element","Measure")) %>%
                 spread(Varname, Value) %>%
                 full_join(traces_plotting, by = "Depth") %>%
                 gather(Varname, Value, -1) %>%
                 drop_na()
             }
           },
           
           "Xc1e" = {
             cores_Xc <- req(plotting_choose_Xc1e_debounced())
             XRFdata_Xc1e <- cleaned %>%
               select(CoreID, Depth, Element, cps) %>% 
               filter(CoreID %in% cores_Xc)
             
             xrf_plotting <- XRFdata_Xc1e %>% 
               group_modify(~map_dfc(proxies, calcproxy_Xc1e)) %>% 
               bind_cols(XRFdata_Xc1e, .) %>%
               gather(Measure, Value, -c("CoreID", "Depth", "Element")) %>%
               mutate(Varname = if_else(str_detect(.$Measure, "ratio"), paste0(.$Element, "/", str_extract(.$Measure, "[:alpha:]+")), paste(.$Element, .$Measure))) %>%
               select(-c("Element","Measure"))
           },
           
           "longcore" = {
             XRFdata_longcore <- cleaned %>%
               select(z, SectionID, Element, cps)
             
             xrf_plotting <- XRFdata_longcore %>% 
               group_modify(~map_dfc(proxies, calcproxy_longcore)) %>% 
               bind_cols(XRFdata_longcore, .) %>%
               gather(Measure, Value, -c("z", "SectionID", "Element")) %>%
               mutate(Varname = if_else(str_detect(.$Measure, "ratio"), paste0(.$Element, "/", str_extract(.$Measure, "[:alpha:]+")), paste(.$Element, .$Measure))) %>%
               select(-c("Element","Measure"))
           })
    
    xrf_plotting
  })
  
  XRFfun_plotting_redraw <- eventReactive(input$plotting_redraw, {
    pmode <- req(input$plotting_mode)
    plotdata <- XRFfun_plotting_compute()
    gg2theme <- req(plotting_gg2theme())
    
    switch(pmode,
           "1cXe" = {
             plotdata_1cXe <- plotdata %>%
               filter(Varname %in% req(input$plotting_choosetraces))
             p <- ggplot(data = plotdata_1cXe, aes(x = Depth, y = Value)) + facet_grid(.~Varname, scales = "free_x") + scale_x_reverse("Depth [mm]") + scale_y_continuous("") + coord_flip() + gg2theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + geom_line()
           },
           
           "Xc1e" = {
             plotdata_Xc1e <- plotdata %>%
               filter(Varname %in% req(input$plotting_choosetrace_Xc1e))
             p <- ggplot(data = plotdata_Xc1e, aes(x = Depth, y = Value)) + facet_grid(Varname~CoreID, scales = "free_x") + scale_x_reverse("Depth [mm]") + scale_y_continuous("") + coord_flip() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + geom_line()
           },
           
           "longcore" = {
             plotdata_longcore <- plotdata %>%
               filter(Varname %in% req(input$plotting_choosetraces))
             p <- ggplot(data = plotdata_longcore, aes(x = z, y = Value, colour = SectionID)) + facet_grid(.~Varname, scales = "free_x") + scale_x_reverse("Depth [mm]") + scale_y_continuous("") + coord_flip() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + geom_line() + scale_color_viridis()
           })
    
    p
  })
  
  # Other reactive functions
  
  diagnostics_currentxlims <- reactive({
    # diagnostics_currentxlims()
    ## We can't read from reactive values inside reactiveValues(), so we use reactive() to read out the current chosen CoreID and the maximum depth to create a vector for the x limits.
    xlims = c(min(XRFfun_diags_coreid()$Depth), max(XRFfun_diags_coreid()$Depth))
  })
  
  ## here we create an empty reactiveValue to hold our deselected data of all CoreIDs
  excludeddata <- reactiveValues()
  
  additionaltraces <- reactiveValues()
  
  diagnostics_elements_excl <- reactive({
    input$diagnostics_elements_excl
  })
  
  diagnostics_elements_excl_debounced <- debounce(diagnostics_elements_excl, 1000)
  
  plotting_choose_Xc1e <- reactive({
    input$plotting_choose_Xc1e
  })
  
  plotting_choose_Xc1e_debounced <- debounce(plotting_choose_Xc1e, 1000)
  
  plotting_chooseproxies <- reactive({
    input$plotting_chooseproxies
  })
  
  plotting_chooseproxies_debounced <- debounce(plotting_chooseproxies, 1000)
  
  plotting_proxies <- reactiveValues()
  
  plotting_gg2theme <- reactive({
    gg2themes[[input$plotting_theme]]
  })
  
  ## empty reactiveValue to hold filenames and data of additional traces
  # additionaltraces <- reactiveValues()

# Observers ---------------------------------------------------------------
  
  # Observers for import page
  
  observeEvent(input$import_catmode, {
    ## Here we initialise selectize input on the import page for the preview element when import_catmode == TRUE.
    updateSelectizeInput(
      session,
      'import_catpreview_element',
      choices = unique(XRFfun_rmrepeats()$Element),
      server = TRUE
    )
  })
  
  observeEvent(input$import_catmode, {
    ## When import_catmode == TRUE, we show the preview plot and overview table on the import page, otherwise we hide it.
    toggleState("import_descorder")
    toggleState("import_catpreview_element")
  })
  
  # Observers for diagnostics page
  
  observeEvent(input$diagnostics_core_id, {
    ## Here we initialise the X limits range slide on the diagnostics page
    updateSliderInput(
      session,
      "diagnostics_xlims",
      value = diagnostics_currentxlims(),
      min = diagnostics_currentxlims()[1],
      max = diagnostics_currentxlims()[2]
    )
  })
  
  observe({
    ## Here we initialise the CoreID selectize input on the diagnostics page
    updateSelectizeInput(
      session,
      'diagnostics_core_id',
      choices = unique(XRFfun_rmrepeats()$CoreID),
      server = TRUE
    )
  })
  
  observe({
    ## Here we initialise and update the Element picker on the diagnostics page. The control gets reset when the CoreID is changed (invalidated dependency).
    updatePickerInput(
      session,
      'diagnostics_elements_excl',
      selected = unique(XRFfun_diags_coreid()$Element),
      choices = unique(XRFfun_diags_coreid()$Element)
    )
  })
  
  observeEvent(input$diagnostics_elements_excl, {
    ## Here we check which Elements are deselected and write them into the excludeddata$exclelem list per CoreID. This means that changing to a CoreID that already has an exclelem list that is not NULL will overwrite the content! NB: Avoiding to overwrite existing data with NULL from another CoreID is not smart because the input gets initialized with all Elements.
    deselected <-
      setdiff(unique(XRFfun_diags_coreid()$Element), input$diagnostics_elements_excl)
    excludeddata$exclelem[[input$diagnostics_core_id]] <- deselected
  })
  
  observeEvent(input$diagnostics_exclude_rmpoints, {
    ## Only executed when actionbutton "remove points" is clicked. We choose all possible fields of interest in brushedDepths to be able to brush in every of the three plot modes. Then we append the data to the list excldepth, making sure not to write duplicates.
    brushedDepths <- XRFfun_diags_coreid() %>%
      select(CoreID, Depth, Element, relstdev, Chi2, cps)
    selpoints <-
      brushedPoints(brushedDepths, input$diagnostics_diagnosticsplot_brush)
    
    excludeddata$excldepth[[input$diagnostics_core_id]] <-
      c(selpoints$Depth,
        setdiff(excludeddata$excldepth[[input$diagnostics_core_id]], selpoints$Depth))
  })
  
  observeEvent(input$diagnostics_exclude_reset, {
    ## Only executed when the "reset" button on the diagnostics page is pressed: Removes all excluded depths from the excldepth list.
    excludeddata$excldepth[[input$diagnostics_core_id]] <- NULL
  })
  
  
  # Observers for plotting page
  
  observeEvent(XRFfun_diags_cleaned(), {
    cleaned <- XRFfun_diags_cleaned()
    
    if (any(str_detect(colnames(cleaned), "LongcoreName"))) {
      updateSelectizeInput(
        session,
        "plotting_mode",
        server = TRUE,
        choices = c("1 Core (Section) - X Elements" = "1cXe",
                    "X Cores (Sections) - 1 Element" = "Xc1e",
                    "Longcore" = "longcore")
      )
    } else {
      updateSelectizeInput(
        session,
        "plotting_mode",
        server = TRUE,
        choices = c("1 Core (Section) - X Elements" = "1cXe",
                    "X Cores (Sections) - 1 Element" = "Xc1e")
      )
    }
    
      updateSelectizeInput(
        session,
        "plotting_choose_1cXe",
        server = TRUE,
        choices = unique(cleaned$CoreID)
      )
      
      updatePickerInput(
        session,
        "plotting_choose_Xc1e",
        choices = unique(cleaned$CoreID)
      )
  })
  
  observeEvent(input$plotting_mode, {
    ## If the user changes the plotting mode, we show/hide the already existing input controls. This way we can initialise the input controls and be sure that they already exist (e.g. not are created on demand with renderUI)
    
    pmode <- req(input$plotting_mode)
    switch(pmode,
           "1cXe" = {
             shinyjs::show(id = "plotting_choose_1cXe")
             shinyjs::show(id = "plotting_addtraces")
             shinyjs::show(id = "plotting_choosetraces")
             
             shinyjs::hide(id = "plotting_choose_Xc1e")
             shinyjs::hide(id = "plotting_longcorename")
             shinyjs::hide(id = "plotting_choosetrace_Xc1e")
           },
           
           "Xc1e" = {
             shinyjs::show(id = "plotting_choose_Xc1e")
             shinyjs::show(id = "plotting_choosetrace_Xc1e")
             
             shinyjs::hide(id = "plotting_choose_1cXe")
             shinyjs::hide(id = "plotting_addtraces")
             shinyjs::hide(id = "plotting_longcorename")
             shinyjs::hide(id = "plotting_choosetraces")
           },
           
           "longcore" = {
             shinyjs::show(id = "plotting_longcorename")
             shinyjs::show(id = "plotting_choosetraces")
             
             shinyjs::hide(id = "plotting_choose_1cXe")
             shinyjs::hide(id = "plotting_choose_Xc1e")
             shinyjs::hide(id = "plotting_addtraces")
             shinyjs::hide(id = "plotting_choosetrace_Xc1e")
           })
  })
  
  observeEvent(input$plotting_choose_1cXe, {
    cleaned <- XRFfun_diags_cleaned()
    clean_choices <- cleaned %>% filter(CoreID %in% req(input$plotting_choose_1cXe)) %>% select(Element) %>% distinct() %>% .[[1]]
    
    updatePickerInput(
      session,
      "plotting_chooseproxies",
      choices = unique(clean_choices)
    )
  })
  
  observeEvent(plotting_choose_Xc1e_debounced(), {
    cleaned <- XRFfun_diags_cleaned()
    clean_choices <- cleaned %>% filter(CoreID %in% req(plotting_choose_Xc1e_debounced())) %>% select(Element) %>% distinct() %>% .[[1]]
    
    updatePickerInput(
      session,
      "plotting_chooseproxies",
      choices = unique(clean_choices)
    )
  })
  
  observeEvent(plotting_chooseproxies_debounced(), {
    plotting_proxies$proxylist <- req(plotting_chooseproxies_debounced())
  })
  
  observeEvent(XRFfun_plotting_compute(), {
    computed <- XRFfun_plotting_compute()
    
    ## filter out elements that have been excluded previously for 1 or more cores -> common elements only
    
    updatePickerInput(
      session,
      "plotting_choosetraces",
      choices = unique(computed$Varname)
    )
    
    updatePickerInput(
      session,
      "plotting_choosetrace_Xc1e",
      choices = unique(computed$Varname)
    )
  })
  
  # Observers for export page

# Output code -------------------------------------------------------------
  
  # Outputs on import page
  
  output$sectiontable <- renderDT({
    ## Creating the sections table on the import page (only viewed if import_catmode == TRUE)
    XRFfun_makelongcore()}, 
    options = list(paging = FALSE, searching = FALSE))
  
  output$catplot <- renderPlot({
    if (input$import_catmode) {
      catpreview <- XRFfun_rmrepeats() %>%
        filter(Element == req(input$import_catpreview_element))
      ggplot(catpreview, aes(x = z, y = cps)) + geom_line(aes(colour = factor(SectionID))) + xlab("Depth [mm]") + scale_y_continuous("counts per second (cps)", limits = c(NA, NA)) + labs(color = "Section ID")
    }
  })
  
  # Outputs on diagnostics page
  
  output$diagnostics_coreid_text <- renderText({
    ## Printing the chosen CoreID on the diagnostics page in the second column at the bottom.
    input$diagnostics_core_id
  })
  
  output$diagnostics_excltable <- renderDT({
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
  
  output$diagnostics_diagnosticsplot <- renderPlot({
    ## Diagnostic plots: Three different modes
    excldepths <- excludeddata$excldepth[[input$diagnostics_core_id]]
    
    if (input$diagnostics_diagmode == "relstdevdiag") {
      XRF_relstdev <- XRFfun_diags_coreid()
      
      XRF_relstdev_cutoff <- XRF_relstdev %>%
        group_by(CoreID, Element) %>%
        summarise(keepmedquorum = median(abs(relstdev))) %>%
        left_join(XRF_relstdev, by = c("CoreID", "Element")) %>%
        mutate(
          keepmedquorum = ifelse(
            keepmedquorum <= req(input$diagnostics_medrelstdev),
            "Below",
            "Exceeded"
          ),
          keepmedquorum = as.factor(keepmedquorum)
        ) %>%
        filter(Element %in% req(diagnostics_elements_excl_debounced()),
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
                                                        limits = c(input$diagnostics_ymin, input$diagnostics_ymax)) + geom_hline(aes(yintercept = input$diagnostics_maxrelstdev), colour = "red") + facet_wrap(CoreID ~
                                                                                                                                                                                         Element, scales = "free") + scale_x_continuous("Depth [mm]", limits = input$diagnostics_xlims)
    }
    
    if (input$diagnostics_diagmode == "chisqdiag") {
      XRF_chisq <- XRFfun_diags_coreid() %>%
        filter(Element %in% req(diagnostics_elements_excl_debounced()),
               !(Depth %in% excldepths))
      p <-
        ggplot(XRF_chisq, aes(x = Depth, y = Chi2)) + geom_line() + scale_y_continuous("goodness of fit",
                                                                                       limits = c(input$diagnostics_ymin, input$diagnostics_ymax)) + facet_wrap( ~ Element, scales = "free") + geom_hline(aes(yintercept = input$diagnostics_chi2cutoff), colour = "blue") + scale_x_continuous("Depth [mm]", limits = input$diagnostics_xlims)
    }
    
    if (input$diagnostics_diagmode == "cpsdiag") {
      XRF_cps <- XRFfun_diags_coreid() %>%
        filter(Element %in% req(diagnostics_elements_excl_debounced()),
               !(Depth %in% excldepths))
      p <-
        ggplot(XRF_cps, aes(x = Depth, y = cps)) + geom_line() + scale_y_continuous("cps", limits = c(input$diagnostics_ymin, input$diagnostics_ymax)) + facet_wrap( ~
                                                                                                                                                           Element, scales = "free") + scale_x_continuous("Depth [mm]", limits = input$diagnostics_xlims)
    }
    p
  })
  
  output$diagnostics_saveplot <- downloadHandler(
    filename = function() {
      "XRF_diagnostics.pdf"
    },
    content = function(file) {
      ggsave(file, width = 12, height = 10, device = "pdf")
    },
    contentType = "application/pdf"
  )
  
  output$plotting_saveplot <- downloadHandler(
    filename = function() {
      "XRF_plotting.pdf"
    },
    content = function(file) {
      ggsave(file, width = 12, height = 10, device = "pdf")
    },
    contentType = "application/pdf"
  )
  
  # Outputs on plotting page
  
  output$plotting_plotout <- renderPlot({
    ## error handling here needed
    XRFfun_plotting_redraw()
  })
  
  output$plotting_longcorename <- renderText({
    cleaned <- XRFfun_diags_cleaned()
    if (any(str_detect(colnames(cleaned), "LongcoreName"))) {
      print(paste("Longcore name:", unique(cleaned$LongcoreName)))
    } else
      return(NULL)
  })
  
  # Outputs on statistics page
  
}