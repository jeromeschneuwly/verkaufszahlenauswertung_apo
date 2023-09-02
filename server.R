
server <- function(input, output) {
  
  apodata <- reactive({
    dataset <- if(input$apotheke == "Zentrum") {
      read.delim(paste0(zentrum_path, "Monatsauswertung_2023.csv"),
                        sep = ';', fileEncoding = 'latin1')
    } else {
      read.delim(paste0(glatt_path, "Monatsauswertung_2023.csv"),
                        sep = ';', fileEncoding = 'latin1')
    }
    year_1 <- as.numeric(year(input$dateRange[1]))-1
    year_2 <- as.numeric(year(input$dateRange[1]))
    year_3 <- as.numeric(year(input$dateRange[2]))
    split_month <- as.numeric(month(input$dateRange[2]))
    month_1 <- as.numeric(month(input$dateRange[1]))
    
    dataset <- dataset %>%
      filter(Relevant == 'ja') %>% 
      left_join(mapping_kat5_clean_no_change, by = c("Pharmacode")) %>% 
      left_join(mapping_kat5_changed, by = c("Pharmacode", "Jahr")) %>% 
      mutate(Zeitraum = case_when(Jahr == year_1 & Monat < split_month ~
                                    "vorher",
                                  Jahr == year_1 & Monat >= split_month |
                                    Jahr == year_2 & Monat < split_month ~
                                    paste(month_1, year_1, "-",
                                          month_1-1, year_2),
                                  Jahr == year_2 & Monat >= split_month |
                                    Jahr == year_3 & Monat < split_month ~
                                    paste(month_1, year_2, "-",
                                          month_1-1, year_3)),
             Kategorie_5 = coalesce(Kategorie_5_stable, Kategorie_5_changed),
             Kategorie_6 = paste(Marke, Kategorie_5, sep = "_")) 
      
      
    return(dataset)
  })
  
  plot_levels <- reactive({
    year_1 <- as.numeric(year(input$dateRange[1]))-1
    year_2 <- as.numeric(year(input$dateRange[1]))
    year_3 <- as.numeric(year(input$dateRange[2]))
    split_month <- as.numeric(month(input$dateRange[2]))
    month_1 <- as.numeric(month(input$dateRange[1]))
    level1 <- paste(month_1, year_1, "-",
          month_1-1, year_2)
    level2 <- paste(month_1, year_2, "-",
          month_1-1, year_3)
    levelsplot <- c(level1, level2)
    return(levelsplot)
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
          summarise_at(vars(input$varselection), sum, na.rm = T)
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
  
  output$testout <- renderTable({
    apo_data_agg()
  })
  
  # output$type <- renderText({
  #   str(outVar())
  # })
  output$yearlyplot <- renderPlot({
    if(!is.null(apo_data_agg()) && nrow(apo_data_agg()>0)) {
      plot_data <- apo_data_agg()
      if(input$timeaggregation == 'Jahr') {
        plot_data <- plot_data %>% dplyr::mutate(Jahr = as.factor(Jahr))
      } else if (input$timeaggregation == 'Zeitraum') {
        time1 <- plot_levels()[1]
        time2 <- plot_levels()[2]
        plot_data$Zeitraum <- factor(plot_data$Zeitraum, 
                                     levels = c('vorher', time1, time2))
      }
      plot_data <- plot_data %>% dplyr::mutate(across(2, as.numeric))

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
  
  
}