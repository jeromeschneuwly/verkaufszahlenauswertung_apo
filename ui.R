ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("apotheke", "Apotheke auswählen", c("Zentrum", "Glatt")),
      
      prettyRadioButtons("colselection", "Typ auswählen:", 
                         choices = c("Produkt" = "Artikelbezeichnung", 
                                     "Kategorie 4" = "Kategorie_4",
                                     "Marke" = "Marke"), 
                         selected = "Artikelbezeichnung", shape = "round"),
      
      prettyRadioButtons("timeaggregation", "Monate oder Jahre vergleichen:", 
                         choices = c("Jahr" = "Jahr", 
                                     "Monat" = "Monat",
                                     "Auswertungszeitraum" = "Zeitraum"), 
                         selected = "Jahr", shape = "round"),
      
      
      conditionalPanel(
        "input.timeaggregation == 'Monat'",
        sliderInput("range", "Zeitraum wählen", min = 1, max = 12, value = c(1,6), step = 1)
        
      ),
      uiOutput("detailselection"),
      
      selectInput("varselection", "Variable wählen:", 
                  choices = c("Absolute_Marge", "Umsatz", "Packungen"), 
                  selectize = FALSE, selected = "Umsatz")
      
    ),
    mainPanel(
      tableOutput("testout"),
      #textOutput("type")
      plotOutput("yearlyplot")
    )
  )
)