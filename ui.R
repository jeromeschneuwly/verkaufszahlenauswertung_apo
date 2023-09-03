ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("apotheke", "Apotheke auswählen", c("Zentrum", "Glatt")),
      
      prettyRadioButtons("colselection", "Typ auswählen:", 
                         choices = c("Produkt" = "Artikelbezeichnung", 
                                     "Kategorie 5" = "Kategorie_5",
                                     "Kategorie 6" = "Kategorie_6",
                                     "Marke" = "Marke"), 
                         selected = "Artikelbezeichnung", shape = "round"),
      
      prettyRadioButtons("timeaggregation", "Vergleichszeitraum wählen:", 
                         choices = c("Jahr" = "Jahr",
                                     "Auswertungszeitraum" = "Zeitraum"), 
                         selected = "Jahr", shape = "round"),
      
      
      conditionalPanel(
        "input.timeaggregation == 'Zeitraum'",
        dateRangeInput('dateRange',label = "Zeitraum:", format = "dd.mm.yyyy", 
                       language = "de", start = "2022-09-01", end = "2023-08-31",
                       startview = "year", separator = " - ")
      ),
      uiOutput("detailselection"),
      
      selectInput("varselection", "Variable wählen:", 
                  choices = c("Kumulierte_Absolute_Marge", "Umsatz", "Packungen"), 
                  selectize = FALSE, selected = "Kumulierte_Absolute_Marge")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Einzelauswertung", 
                 plotOutput("zeitraumplot"),
                 tableOutput("einzeltable")
                 ),
        tabPanel("Monatsauswertung",
                 plotOutput("monatsplot")
                 ),
        tabPanel("Gesamtauswertung",
                 plotOutput("gesamtauswertung"),
                 tableOutput("totaltable")
                 )
        
      )
    )
  )
)