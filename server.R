
server <- function(input, output) {
  
  plot_levels <- reactive({
    
    date_1 <- input$dateRange[1]
    date_2 <- input$dateRange[2]
    level1 <- paste(zoo::as.yearmon(date_1-365), zoo::as.yearmon(date_1-1), sep = "-")
    level2 <- paste(zoo::as.yearmon(date_1), zoo::as.yearmon(date_2), sep = "-")
    levelsplot <- c(level1, level2)
    return(levelsplot)
  })
  
  
  
  apodata <- reactive({
    dataset <- if(input$apotheke == "Zentrum") {
      read.delim(paste0(zentrum_path, "Monatsauswertung_2023.csv"),
                        sep = ';', fileEncoding = 'latin1')
    } else {
      read.delim(paste0(glatt_path, "Monatsauswertung_2023.csv"),
                        sep = ';', fileEncoding = 'latin1')
    }
    date_1 <- input$dateRange[1]
    date_2 <- input$dateRange[2]
    
    dataset <- dataset %>%
      filter(Relevant == 'ja') %>% 
      left_join(mapping_kat5_clean_no_change, by = c("Pharmacode")) %>% 
      left_join(mapping_kat5_changed, by = c("Pharmacode", "Jahr")) %>%
      mutate(Datum = as.Date(paste('1-', Monat, '-', Jahr, sep=''), "%d-%m-%Y"),
             Zeitraum = case_when(Datum < date_2 & Datum >= date_1 ~ 
                                    plot_levels()[2],
                                  Datum < date_1 & Datum >= date_1-365 ~ 
                                    plot_levels()[1],
                                  Datum < date_1-365 ~ "vorher",
                                  TRUE ~ 'nachher'),
             Kategorie_5 = coalesce(Kategorie_5_stable, Kategorie_5_changed),
             Kategorie_6 = paste(Marke, Kategorie_5, sep = "_"))
      
    return(dataset)
  })
  
  
  
  
  outVar <- reactive({
  apodata <- apodata()
  col <- apodata %>% dplyr::select(input$colselection) # Diese Spalte wird ausgewählt über die UI
  col.s <- unname(unlist(unique(col)))
  col.s <- sort(col.s)
  return(col.s)
  })
  
  output$detailselection = renderUI({
    selectInput("Bezeichnung", "Bezeichnung wählen:", choices = outVar(), selectize = FALSE)
  })
  
  
  
  apo_data_agg <- reactive({
    predata <- apodata()
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
  
  output$einzeltable <- renderTable({
    apo_data_agg()
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
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(size = 22),
              axis.text.y = element_text(size = 22),
              plot.title = element_text(size = 22),
              axis.title.y = element_text(size = 22),
              legend.position = "none") +
        labs(y = input$varselection, title = input$Bezeichnung)
    }
  })
  
  output$monatsplot <- renderPlot({
    monthplot <- apodata() %>% 
      mutate(Jahr = as.factor(Jahr),
             Monat = as.factor(Monat)) %>% 
      dplyr::filter(.data[[input$colselection]] == input$Bezeichnung) %>% 
      group_by(Monat, Jahr) %>% 
      summarise_at(vars(input$varselection), sum, na.rm = T)
      
    ggplot(monthplot, aes(x = Monat, y = monthplot[[3]], fill = Jahr, group = Monat)) +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single")) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 22),
            axis.text.x = element_text(size = 22),
            axis.text.y = element_text(size = 22),
            plot.title = element_text(size = 22),
            axis.title.y = element_text(size = 22),
            legend.position = "bottom",
            legend.text = element_text(size = 22)) +
      labs(y = input$varselection, title = input$Bezeichnung)
  })
  
  
  apo_data_total_agg <- reactive({
    predata <- apodata()
      if(input$timeaggregation == 'Jahr') {
        data_agg <- predata %>%
          group_by(Jahr) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T) %>% 
          mutate(Jahr = as.factor(Jahr))
      } else if (input$timeaggregation == 'Zeitraum') {
        predata <- predata %>% filter(!Zeitraum %in% c("vorher", "nachher"))
        data_agg <- predata %>%
          group_by(Zeitraum) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T)
      }
      return(data_agg)
  })
  
  output$totaltable <- renderTable({
    apo_data_total_agg()
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
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 22),
            axis.text.y = element_text(size = 22),
            plot.title = element_text(size = 22),
            axis.title.y = element_text(size = 22),
            legend.position = "none") +
      labs(y = input$varselection)
    }
  })
}