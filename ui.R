ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("apotheke", "Apotheke auswählen", c("Zentrum", "Glattpark")),
      
      conditionalPanel(condition = "input.conditionedPanels != 3",
                       prettyRadioButtons("colselection", "Typ auswählen:", 
                                          choices = c("Produkt" = "Artikelbezeichnung", 
                                                      "Hauptkategorie" = "Kategorie_5",
                                                      "Marke & Kategorie" = "Kategorie_6",
                                                      "Marke" = "Marke"), 
                                          selected = "Artikelbezeichnung", shape = "round")),
      
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
      
      conditionalPanel(condition = "input.conditionedPanels == 3",
                       selectInput("filterselection", "Filter wählen:", 
                                   choices = c("Marge (<28%)" = "Marge_Prozent", 
                                               "Auswertungsrelevante Produkte" = "Relevant", 
                                               "Selbstwahl" = "Selbstwahl", "Topseller" = "Topseller"), 
                                   selectize = FALSE, selected = "Relevant")),
      
      selectInput("saleselection", "Verkaufsarten wählen:", 
                  choices = c("Bar", "Rezept", "Bar & Rezept"), 
                  selectize = FALSE, selected = "Bar"),
      
      conditionalPanel(condition = "input.conditionedPanels != 3",
                       uiOutput("detailselection")),
      
      
      selectInput("varselection", "Variable wählen:", 
                  choices = c("Gewinn" = "Kumulierte_Absolute_Marge", 
                              "Umsatz" = "Umsatz", "Anzahl Packungen" = "Packungen"), 
                  selectize = FALSE, selected = "Kumulierte_Absolute_Marge")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Einzelauswertung", id = "produkte", value = 1, br(),
                 plotOutput("zeitraumplot"),
                 br(),
                 tableOutput("einzeltable")
        ),
        tabPanel("Monatsauswertung", id = "monthplot", value = 2, br(),
                 plotOutput("monatsplot")
        ),
        tabPanel("Gesamtauswertung", id = "gesamt", value = 3, br(),
                 plotOutput("gesamtauswertung"),
                 br(),
                 tableOutput("totaltable")
        ),
        id = "conditionedPanels"
        
      )
    )
  )
)