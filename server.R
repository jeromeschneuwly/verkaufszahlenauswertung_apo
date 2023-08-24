
server <- function(input, output) {
  
  apodata <- reactive({
    dataset <- if(input$apotheke == "Zentrum") {
      read.delim(paste0(zentrum_path, "Monatsauswertung.csv"),
                        sep = ';', fileEncoding = 'latin1')
    } else {
      read.delim(paste0(glatt_path, "Monatsauswertung.csv"),
                        sep = ';', fileEncoding = 'latin1')
    }
    dataset <- dataset %>% 
      mutate(Zeitraum = case_when(Jahr == 2021 & Monat < 9 ~
                                    "vorher",
                                  Jahr == 2021 & Monat >=9 | Jahr == 2022 & Monat < 9 ~ 
                                    "Sept21-Aug22",
                                  Jahr == 2022 & Monat >=9 | Jahr == 2023 & Monat < 9 ~
                                    "Sept22-Aug23"))
    
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
      }
      plot_data <- plot_data %>% dplyr::mutate(across(2, as.numeric))
      
      ggplot(plot_data, aes(x = plot_data[[1]], y = plot_data[[2]])) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_bw()
    }
  })
  
  
}