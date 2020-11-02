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
ui <- dashboardPage(
  dashboardHeader(
    title = "Analyses des consommations electriques"
    ),
  
  dashboardSidebar(
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
  
  
  dashboardBody(
    h5(textOutput('nom_dep'),
       dataTableOutput('ma_table', width = "40%")),
    
    # barplots des repartitions par secteur et par an   
    plotOutput('repartition'),
    
    # courbes des evolutions des differents secteurs
    plotOutput('evolution')
    
  )
  
) ###fin du premier onglet


#####TODO: rajouter les onglets suivants :
#####Analyse des determinants de la conso
#####Cartographie




# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nom_dep <- renderText({input$dep})
  
  # Cette fonction filtre le jeu de données entier
  # pour ne garder que ce qui est intéressant
  
  
  filtre <- reactive({
    out <- consos %>% 
      filter(nom_departement == input$dep) %>% 
      filter(annee %in% input$annee)
    
    out <- out %>% 
      select(annee,
             conso_totale_residentiel_mwh_,
             conso_totale_professionnel_mwh_,
             conso_totale_agriculture_mwh_,
             conso_totale_tertiaire_mwh_,
             conso_totale_autres_mwh_)
    
    out
  })
  
  ##Creation de la table a afficher
  ##TODO: prendre toute la table et pas les six premieres lignes 
  output$ma_table <- renderDataTable({
    out <- filtre() %>%
      mutate_all(list(round)) %>% 
      rename_all(list(str_replace), pattern = 'conso_totale', replacement = '')
    out
  })
  
  # barplots par annee, couleurs par secteur d'activite
  output$repartition <- renderPlot({
    df_filtre <- filtre() %>%
      pivot_longer(-c("annee"))
    
    ggplot(df_filtre) +
      geom_bar(stat = 'identity') +
      aes(y = value, x = annee, fill = name)
  })
  
  # graphique des courbes
  output$evolution <- renderPlot({
    df <- filtre() %>% 
      tidyr::pivot_longer(-c("annee"))
    
    fig <- ggplot(df) +
      aes(y = value, x = annee, color = name) +
      geom_line() +
      theme_bw() +
      theme(legend.position = 'bottom')
    
    fig
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)
