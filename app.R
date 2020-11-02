# libraries
library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(tidyverse)


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')

##Consos mailles régionales pour l onglet regions



# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
  'Analyses des consommations electriques',
  
  tabPanel('Mon département',
           id = 'departements',
  


  sidebarLayout(
    sidebarPanel(
      
      # Choix du département 
      selectInput("dep",
                  "Choisissez votre departement:",
                  choices = consos$nom_departement %>% unique() %>% sort,
                  selected = 'Doubs'),
      # Choix de l'année
      selectInput("annee",
                  "Choisissez l'annee:",
                  choices = consos$annee %>% unique() %>% sort,
                  selected = 2011,
                  multiple = TRUE)
    ),
    
    
    mainPanel(
      ##affichage du nom du departement
      h3(textOutput('nom_dep')),
      
      dataTableOutput('ma_table'),
      
      plotOutput('repartition')
      
    )
    
    ##TODO : répartition des consos par secteur et année
    
    ##TODO: évolution des consos par secteur au cours du temps
    
  )
  
  ) ###fin du premier onglet
  
  
  #####TODO: rajouter les onglets suivants :
  #####Analyse des determinants de la conso
  #####Cartographie
  
)



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nom_dep <- renderText({input$dep})
  
  # Cette fonction filtre le jeu de données entier
  # pour ne garder que ce qui est intéressant
  

  filtre <- reactive({
    ##TODO: rajouter aussi un filtre sur les annees
    consos %>% 
      filter(nom_departement == input$dep) %>% 
      filter(annee %in% input$annee)
  })
  
  ##Creation de la table a afficher
  ##TODO : remplacer par un datatable (dans server et ui)
  ##TODO: prendre toute la table et pas les six premieres lignes 
   output$ma_table <- renderDataTable({
   out <-  filtre() %>%
     select(- contains('superficie'),
            - contains('residences'),
            - contains('taux')
            ,- contains('geos')) %>% 
     select(contains('conso'))
   print(out)
   out
  } )
   
  output$repartition <- renderPlot({
    df_filtre <- filtre() %>% 
      select(annee,
             conso_totale_residentiel_mwh_,
             conso_totale_professionnel_mwh_,
             conso_totale_agriculture_mwh_,
             conso_totale_tertiaire_mwh_,
             conso_totale_autres_mwh_) %>% 
      pivot_longer(-c("annee"))
    
    ggplot(df_filtre) +
      geom_bar(stat = 'identity') +
      aes(y = value, x = annee, fill = name)
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)
