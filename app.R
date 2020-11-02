# libraries
library(shiny)
library(dplyr)


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
                  selected = 2011)
    ),
    
    
    mainPanel(
      ##affichage du nom du departement
      h3(textOutput('nom_dep')),
      
      ####TODO: remplacer par la table par un datatable 
      tableOutput('ma_table')
      
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
  
  output$nom_dep <- renderText({
    paste('ANALYSE DU DEPARTEMENT', input$dep)
  })
  
  # Cette fonction filtre le jeu de données entier
  # pour ne garder que ce qui est intéressant
  

  filtre <- reactive({
    ##TODO: rajouter aussi un filtre sur les annees
    consos %>% 
      filter(nom_departement == input$dep) %>% 
      filter(annee == input$annee)
  })
  
  ##Creation de la table a afficher
  ##TODO : remplacer par un datatable (dans server et ui)
  ##TODO: prendre toute la table et pas les six premieres lignes 
   output$ma_table <- renderTable({
   out <-  filtre() %>%
     select(- contains('superficie'),
            - contains('residences'),
            - contains('taux')
            ,- contains('geos'))
   print(out)
   out
  } )
  
}




# Run the application 
shinyApp(ui = ui, server = server)
