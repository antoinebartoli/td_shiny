# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')
names(consos)

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(
    title="Analyses des consommations electriques"
  ),
  
  dashboardSidebar(
    # Choix du département 
    selectInput("dep",
                "Choisissez votre departement:",
                choices = levels(consos$nom_departement),
                selected = 'Doubs'),
    # Choix de l'année 
    selectInput("annee",
                "Choisissez votre année:",
                choices = sort(unique(consos$annee)),
                multiple = TRUE,
                selected = '2015')
  ),
  
  dashboardBody(
    h5(textOutput('nom_dep')
       , dataTableOutput('ma_table',width = "40%"))
    
    
    , plotOutput('repartition')
  )
) 


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$nom_dep <- renderText({input$dep})
  
  filtre <- reactive({
    consos %>% 
    filter(nom_departement == input$dep) %>%
    filter(annee %in% input$annee)
  })
  
  output$ma_table <- renderDataTable({
    out <-  filtre() %>%
      select(annee,  conso_totale_residentiel_mwh_,
             conso_totale_professionnel_mwh_,
             conso_totale_agriculture_mwh_,
             conso_totale_tertiaire_mwh_,
             conso_totale_autres_mwh_)
    #print(out)
    out
  } )
  
  
  output$repartition <- renderPlot({
    
    df_filtre <- filtre() %>%
      select(annee,  conso_totale_residentiel_mwh_,
             conso_totale_professionnel_mwh_,
             conso_totale_agriculture_mwh_,
             conso_totale_tertiaire_mwh_,
             conso_totale_autres_mwh_) %>%
      tidyr::pivot_longer(-c("annee"))
    
    
    ggplot(df_filtre) +
      geom_bar(stat = 'identity') +
      aes(y  = value, x = annee, fill = name)
    
  }) 
}




# Run the application 
shinyApp(ui = ui, server = server)