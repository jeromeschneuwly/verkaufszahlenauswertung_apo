ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("apotheke", "Apotheke auswählen", c("Zentrum", "Glattpark")),
      
      prettyRadioButtons("colselection", "Typ auswählen:", 
                         choices = c("Produkt" = "Artikelbezeichnung", 
                                     "Kategorie 5" = "Kategorie_5",
                                     "Kategorie 6" = "Kategorie_6",
                                     "Marke" = "Marke"), 
                         selected = "Artikelbezeichnung", shape = "round"),
      
      selectInput("filterselection", "Filter wählen:", 
                  choices = c("Marge (<28%)" = "Marge_Prozent", "Auswertungsrelevante Produkte" = "Relevant", 
                              "Verkaufsart" = "Verkaufsart", 
                              "Selbstwahl" = "Selbstwahl", "Topseller" = "Topseller"), 
                  selectize = FALSE, selected = "Relevant"),
      
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
      
      selectInput("saleselection", "Verkaufsarten wählen:", 
                  choices = c("Bar", "Rezept", "Bar & Rezept"), 
                  selectize = FALSE, selected = "Bar"),
      
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