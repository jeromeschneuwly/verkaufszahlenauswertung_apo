
server <- function(input, output) {
  
  plot_levels <- reactive({
    date_1 <- input$dateRange[1]
    date_2 <- input$dateRange[2]
    level1 <- paste(zoo::as.yearmon(date_1-365), zoo::as.yearmon(date_1-1), sep = "-")
    level2 <- paste(zoo::as.yearmon(date_1), zoo::as.yearmon(date_2), sep = "-")
    levelsplot <- c(level1, level2)
    return(levelsplot)
  })

  plot_y_label <- reactive({
    y_lab <- vector()
    if(input$varselection == "Kumulierte_Absolute_Marge") {
      y_lab <- "Gewinn (CHF)"
    } else if(input$varselection == "Umsatz") {
      y_lab <- "Umsatz (CHF)"
    } else {
      y_lab <- "Packungen (Stk.)"
    }
  })
  
  
  data_raw <- reactive({
    dataset <- if(input$apotheke == "Zentrum") {
      read.delim(paste0(zentrum_path, "Monatsauswertung_alle_Produkte_2023.csv"),
                        sep = ';', fileEncoding = 'latin1')
    } else {
      read.delim(paste0(glatt_path, "Monatsauswertung_2023.csv"),
                        sep = ';', fileEncoding = 'latin1')
    }
  })
  
  apodata <- reactive ({
    date_1 <- input$dateRange[1]
    date_2 <- input$dateRange[2]
    data_prep <- data_raw() %>%
      left_join(mapping_full, by = c("Pharmacode", "Jahr")) %>%
      mutate(Datum = as.Date(paste('1-', Monat, '-', Jahr, sep=''), "%d-%m-%Y"),
             Zeitraum = case_when(Datum < date_2 & Datum >= date_1 ~ 
                                    plot_levels()[2],
                                  Datum < date_1 & Datum >= date_1-365 ~ 
                                    plot_levels()[1],
                                  Datum < date_1-365 ~ "vorher",
                                  TRUE ~ 'nachher'),
             Kategorie_6 = paste(Marke, Kategorie_5, sep = "_"))
    return(data_prep)
  })
  
  apodata_filtered <- reactive ({
    if(input$saleselection == 'Bar') {
      apodata_filtered <- apodata() %>% 
        filter(Verkaufsart == 'Bar' & !Pharmacode %in% c(10016054, 7816252, 10015237, 10015202))
    } else if(input$saleselection == 'Bar & Rezept') {
      apodata_filtered <- apodata() %>% 
        filter(Verkaufsart %in% c('Bar', 'Rezept') & !Pharmacode %in% c(10016054, 7816252, 10015237, 10015202))
    } else if(input$saleselection == 'Rezept') {
      apodata_filtered <- apodata() %>% 
        filter(Verkaufsart == 'Rezept' & !Pharmacode %in% c(10016054, 7816252, 10015237, 10015202))
    }
    return(apodata_filtered)
  })


  outVar <- reactive({
  apodata_filtered <- apodata_filtered()
  col <- apodata_filtered %>% dplyr::select(input$colselection) # Diese Spalte wird ausgewählt über die UI
  col.s <- unname(unlist(unique(col)))
  col.s <- sort(col.s)
  return(col.s)
  })
  
  output$detailselection = renderUI({
    selectInput("Bezeichnung", "Bezeichnung wählen:", choices = outVar(), selectize = FALSE)
  })
  
  apo_data_agg <- reactive({
    predata <- apodata_filtered()
    if(!is.null(input$colselection) && input$colselection != "" && 
       !is.null(input$Bezeichnung) && input$Bezeichnung != "") {
      if(input$timeaggregation == 'Jahr') {
        data_agg <- predata %>%
          dplyr::filter(.data[[input$colselection]] == input$Bezeichnung) %>% 
          group_by(Jahr) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T) %>% 
          mutate(Jahr = as.factor(Jahr))
      } else if (input$timeaggregation == 'Zeitraum') {
        data_agg <- predata %>%
          dplyr::filter(.data[[input$colselection]] == input$Bezeichnung) %>% 
          group_by(Zeitraum) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T)
      }
      return(data_agg)
    }
    return(data.frame())
  })
  
  
  table_data <- reactive({
    data_diff <- apo_data_agg()
    if(input$timeaggregation == 'Zeitraum') {
      data_diff <- data_diff %>% 
        filter(!Zeitraum %in% c("vorher", "nachher"))
    }
    total_diff <- vector()
    diff <- vector()  # Create a diff variable before the loop
    for(i in 0:nrow(data_diff)) {
      if(i == 0) {
        diff[i+1] <- NA_real_
      } else {
        diff[i+1] <- as.numeric(100/data_diff[i, 2]*data_diff[i+1,2]-100)
      }
      total_diff <- c(total_diff, diff[i])
    }
    data_diff <- data_diff %>% 
      mutate(`Veränderung (%)` = total_diff)
    return(data_diff)
  })
  
  output$einzeltable <- renderTable({
    tab <- table_data()
    colnames(tab)[2] <- plot_y_label()
    tab
  })
  
  
  output$zeitraumplot <- renderPlot({
    if(!is.null(apo_data_agg()) && nrow(apo_data_agg()>0)) {
      plot_data <- apo_data_agg()
      if(input$timeaggregation == "Jahr") {
        plot_data <- plot_data %>% 
          filter(Jahr != "2021")
      } else if (input$timeaggregation == "Zeitraum") {
        time1 <- plot_levels()[1]
        time2 <- plot_levels()[2]
        plot_data <- plot_data %>% filter(!Zeitraum %in% c("vorher", "nachher"))
        plot_data$Zeitraum <- factor(plot_data$Zeitraum, 
                                     levels = c(time1, time2))
      }

      ggplot(plot_data, aes(x = plot_data[[1]], y = plot_data[[2]], fill = plot_data[[1]])) +
        geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single")) +
        theme_classic() +
        scale_fill_brewer(palette = "Dark2") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(size = 22),
              axis.text.y = element_text(size = 22),
              plot.title = element_text(size = 22),
              axis.title.y = element_text(size = 22),
              legend.position = "none",
              axis.ticks.x = element_blank()) +
        labs(y = plot_y_label(), title = input$Bezeichnung)
    }
  })
  
  
  
  
  output$monatsplot <- renderPlot({
    monthplot <- apodata_filtered() %>% 
      mutate(Jahr = as.factor(Jahr),
             Monat = as.factor(Monat)) %>% 
      dplyr::filter(.data[[input$colselection]] == input$Bezeichnung) %>% 
      group_by(Monat, Jahr) %>% 
      summarise_at(vars(input$varselection), sum, na.rm = T)
      
    ggplot(monthplot, aes(x = Monat, y = monthplot[[3]], fill = Jahr, group = Monat)) +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single")) +
      theme_classic() +
      scale_fill_brewer(palette = "Dark2") +
      theme(axis.title.x = element_text(size = 22),
            axis.text.x = element_text(size = 22),
            axis.text.y = element_text(size = 22),
            plot.title = element_text(size = 22),
            axis.title.y = element_text(size = 22),
            legend.position = "bottom",
            legend.text = element_text(size = 22),
            legend.title = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(y = plot_y_label(), title = input$Bezeichnung)
  })
  
  apo_data_total_agg <- reactive({
    predata <- apodata_filtered()
    
    if(input$filterselection == "Relevant") {
      data_filtered <- predata %>% 
        filter(Relevant == 'ja')
    } else if(input$filterselection == "Marge_Prozent") {
      data_filtered <- predata %>% 
        filter(Marge_Prozent < 28 & Verkaufsart != 'Kredit')
    } else if (input$filterselection == "Selbstwahl") {
      data_filtered <- predata %>% 
        filter(Selbstwahl == 'ja' & Verkaufsart != 'Kredit')
    } else if (input$filterselection == "Topseller") {
      data_filtered <- predata %>% 
        filter(Topseller == 'ja' & Verkaufsart != 'Kredit')
    }
      if(input$timeaggregation == 'Jahr') {
        data_agg <- data_filtered %>%
          group_by(Jahr) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T) %>% 
          mutate(Jahr = as.factor(Jahr))
      } else if (input$timeaggregation == 'Zeitraum') {
        data_filtered <- data_filtered %>% filter(!Zeitraum %in% c("vorher", "nachher"))
        data_agg <- data_filtered %>%
          group_by(Zeitraum) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T)
      }
      return(data_agg)
  })
  
  
  gesamt_table_data <- reactive({
    data_diff <- apo_data_total_agg()
    if(input$timeaggregation == 'Zeitraum') {
      data_diff <- data_diff %>% 
        filter(!Zeitraum %in% c("vorher", "nachher"))
    }
    total_diff <- vector()
    diff <- vector()  # Create a diff variable before the loop
    for(i in 0:nrow(data_diff)) {
      if(i == 0) {
        diff[i+1] <- NA_real_
      } else {
        diff[i+1] <- as.numeric(100/data_diff[i, 2]*data_diff[i+1,2]-100)
      }
      total_diff <- c(total_diff, diff[i])
    }
    data_diff <- data_diff %>% 
      mutate(`Veränderung (%)` = total_diff)
    return(data_diff)
  })
  
  
  output$totaltable <- renderTable({
    tab <- gesamt_table_data()
    colnames(tab)[2] <- plot_y_label()
    tab
  })
  
  output$gesamtauswertung <- renderPlot({
    if(!is.null(apo_data_total_agg()) && nrow(apo_data_total_agg()>0)) {
      plot_data <- apo_data_total_agg()
      if(input$timeaggregation == "Jahr") {
        plot_data <- plot_data %>%
          filter(Jahr != "2021")
      } else if (input$timeaggregation == "Zeitraum") {
        time1 <- plot_levels()[1]
        time2 <- plot_levels()[2]
        plot_data$Zeitraum <- factor(plot_data$Zeitraum, 
                                     levels = c(time1, time2))
        plot_data <- plot_data %>% dplyr::mutate(across(2, as.numeric))
      }
    ggplot(plot_data, aes(x = plot_data[[1]], y = plot_data[[2]], 
                           fill = plot_data[[1]], group = plot_data[[1]])) +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single")) +
      theme_classic() +
      scale_fill_brewer(palette = "Dark2") +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 22),
            axis.text.y = element_text(size = 22),
            plot.title = element_text(size = 22),
            axis.title.y = element_text(size = 22),
            legend.position = "none",
            axis.ticks.x = element_blank()) +
      labs(y = plot_y_label())
    }
  })
}