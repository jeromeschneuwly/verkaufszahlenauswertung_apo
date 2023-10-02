
server <- function(input, output) {
res_auth <- secure_server(
  check_credentials = check_credentials(credentials)
)
#auth0_server(function(input, output, session) {
  plot_levels <- reactive({
    date_1 <- as.Date(input$dateRange[1])
    date_2 <- as.Date(input$dateRange[2])
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
      read.delim("./Monatsauswertung_alle_Produkte_2023.csv",
                        sep = ';', fileEncoding = 'latin1')
    } else {
      read.delim("./Monatsauswertung_2023.csv",
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
    apodata_filtered <- apodata_filtered %>% 
      filter(!Zeitraum %in% c("vorher", "nachher"))
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
        data_agg <- predata %>%
          dplyr::filter(.data[[input$colselection]] == input$Bezeichnung) %>% 
          group_by(Zeitraum) %>% 
          summarise_at(vars(input$varselection), sum, na.rm = T)
      return(data_agg)
    }
    return(data.frame())
  })
  
  table_data <- reactive({
    data_diff <- apo_data_agg()
    data_diff_tot <- data.frame()
    total_diff <- vector()
    diff <- vector()  # Create a diff variable before the loop
    diff_chf <- vector()
    total_diff_chf <- vector()
    for(i in 0:nrow(data_diff)) {
      if(i == 0) {
        diff[i+1] <- NA_real_
        diff_chf[i+1] <- NA_real_
      } else {
        diff[i+1] <- as.numeric(100/data_diff[i, 2]*data_diff[i+1,2]-100)
        diff_chf[i+1] <- as.numeric(data_diff[i+1,2]-data_diff[i, 2])
      }
      total_diff <- c(total_diff, diff[i])
      total_diff_chf <- c(total_diff_chf, diff_chf[i])
    }
    data_diff <- data_diff %>% 
      mutate(`Veränderung (%)` = format(round(total_diff, digits = 1), nsmall = 1),
             `Veränderung (CHF)` = format_with_apostrophe(format(round(total_diff_chf, digits = 0), nsmall = 0)))
    return(data_diff)
  })
  
  output$einzeltable <- renderTable({
    tab <- table_data()
    tab[[2]] <- round(as.numeric(tab[[2]]), digits = 0)
    tab[[2]] <- sapply(tab[[2]], format_with_apostrophe)
    colnames(tab)[2] <- plot_y_label()
    tab
  })
  
  output$zeitraumplot <- renderPlot({
    if(!is.null(apo_data_agg()) && nrow(apo_data_agg()>0)) {
      plot_data <- apo_data_agg()
      time1 <- plot_levels()[1]
      time2 <- plot_levels()[2]
      plot_data$Zeitraum <- factor(plot_data$Zeitraum, 
                                   levels = c(time1, time2))
      plot_data[[2]] <- round(plot_data[[2]], digits = 0)
      ggplot(plot_data, aes(x = plot_data[[1]], y = plot_data[[2]], fill = plot_data[[1]])) +
        geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single")) +
        theme_classic() +
        scale_fill_brewer(palette = "Dark2") +
        scale_y_continuous(labels = function(x) format(x, big.mark = "'")) +
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
      scale_y_continuous(labels = function(x) format(x, big.mark = "'")) +
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
    if (input$filterselection == "Alle") {
      data_filtered <- predata
    } else if(input$filterselection == "Relevant") {
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
    data_agg <- data_filtered %>%
      group_by(Zeitraum) %>% 
      summarise_at(vars(input$varselection), sum, na.rm = T)
    return(data_agg)
  })
  
  gesamt_table_data <- reactive({
    data_diff <- apo_data_total_agg()
      total_diff <- vector()
      diff <- vector()  # Create a diff variable before the loop
      diff_chf <- vector()
      total_diff_chf <- vector()
      for(i in 0:nrow(data_diff)) {
        if(i == 0) {
          diff[i+1] <- NA_real_
          diff_chf[i+1] <- NA_real_
        } else {
          diff[i+1] <- as.numeric(100/data_diff[i, 2]*data_diff[i+1,2]-100)
          diff_chf[i+1] <- as.numeric(data_diff[i+1,2]-data_diff[i, 2])
        }
        total_diff <- c(total_diff, diff[i])
        total_diff_chf <- c(total_diff_chf, diff_chf[i])
      }
      data_diff <- data_diff %>% 
        mutate(`Veränderung (%)` = format(round(total_diff, digits = 1), nsmall = 1),
               `Veränderung (CHF)` = format_with_apostrophe(format(round(total_diff_chf, digits = 0), nsmall = 0)))
    return(data_diff)
  })
  
  output$totaltable <- renderTable({
    tab <- gesamt_table_data()
    tab[[2]] <- round(tab[[2]], digits = 0)
    tab[[2]] <- sapply(tab[[2]], format_with_apostrophe)
    colnames(tab)[2] <- plot_y_label()
    tab
  })
  
  output$gesamtauswertung <- renderPlot({
    if(!is.null(apo_data_total_agg()) && nrow(apo_data_total_agg()>0)) {
      plot_data <- apo_data_total_agg()
        time1 <- plot_levels()[1]
        time2 <- plot_levels()[2]
        plot_data$Zeitraum <- factor(plot_data$Zeitraum, 
                                     levels = c(time1, time2))
        plot_data <- plot_data %>% dplyr::mutate(across(2, as.numeric))
    ggplot(plot_data, aes(x = plot_data[[1]], y = plot_data[[2]], 
                           fill = plot_data[[1]], group = plot_data[[1]])) +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single")) +
      theme_classic() +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(labels = function(x) format(x, big.mark = "'")) +
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