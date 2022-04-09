library(data.table) 
library(tidyverse)
library(shiny) 
library(tibble) 
library(shinythemes) 
library(plotly) 
library(wordcloud2) 
library(highcharter)
library(shinyWidgets)

##FORMATAÇÃO PÁGINA

shinyUI(fluidPage(theme = shinytheme("journal"),
            
  navbarPage("Observatório Grenal",
                             
#APRESENTAÇÃO

    tabPanel("Apresentação",
          h1("Observatório Grenal"),
          br(),
          p("Grêmio e Inter se enfrentaram pela primeira vez em 1909 e mais 436 vezes depois disso."),
          p("Torcedores de Grêmio e Inter usam números para tentar convencer que seu time é o melhor no clássico Grenal."),
          p("Torcedores colorados recorrem ao fato de o Inter ter mais vitórias que o rival em toda história do clássico. Um torcedor gremista nascido na metade da década de 1980 pode revidar dizendo que viu mais vitórias do Grêmio do que do Inter em sua vida."),
          p("Torcedores gremistas recorrem ao fato de o Grêmio ter aplicado as maiores goleadas. Um torcedor colorado pode revidar dizendo que seu time aplicou mais goleadas."),
          p("O que é melhor?"),
          p("Tudo é questão de ponto de vista. Qual é o seu?"),
          p("Nesta plataforma, você pode navegar por dados de vitórias, gols, “homens Grenal” e placares. Forme seu ponto de vista e descubra números para utilizar na próxima discussão com seu rival."),
          ),

#VITÓRIAS
  #VITÓRIAS ACUMULADAS

    navbarMenu(title = "Vitórias",
      tabPanel("Vitórias acumuladas",
        titlePanel("Vitórias acumuladas em grenais por cada um dos times"),
          sidebarLayout(
            sidebarPanel(
            
              dateRangeInput('data_escolhida_v', strong("Selecione o intervalo de tempo:"), 
                            start = '1909-07-18', end = Sys.Date(),
                            min = '1909-07-18', max = Sys.Date(),
                            startview='year', language='pt', separator='até', format = "dd/mm/yyyy"),
            
              pickerInput('estadio_v', 'Selecione o(s) estádio(s):', 
                         as.character(unique(data_vitorias$estadio)),
                         multiple=TRUE,
                         selected = c("Arena", "Baixada", "Beira-Rio", "Chácara dos Eucaliptos", "Eucaliptos", "Olímpico", "Outros")),
          
              pickerInput('campeonato_v', 'Selecione o(s) campeonato(s):', 
                         as.character(unique(data_vitorias$campeonato)),
                         multiple=TRUE,
                         selected = c("Amistoso", "Brasileiro", "Citadino", "Gaúcho", "Outros"))),
          
            mainPanel(
              tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
              ),
              plotlyOutput("grafico_vitorias")
    ))),
    
  #DIFERENÇA DE VITÓRIAS
      
      tabPanel("Diferença de vitórias",
        titlePanel("Diferença de vitórias em grenais entre os dois times"),
             sidebarLayout(
               sidebarPanel(
    
                 dateRangeInput('data_escolhida_v2', strong("Selecione o intervalo de tempo:"), 
                                start = '1909-07-18', end = Sys.Date(),
                                min = '1909-07-18', max = Sys.Date(),
                                startview='year', language='pt', separator='até', format = "dd/mm/yyyy"),
                 
                 pickerInput('estadio_v2', 'Selecione o(s) estádio(s):', 
                             as.character(unique(data_vitorias$estadio)),
                             multiple=TRUE,
                             selected = c("Arena", "Baixada", "Beira-Rio", "Chácara dos Eucaliptos", "Eucaliptos", "Olímpico", "Outros")),
                 
                 pickerInput('campeonato_v2', 'Selecione o(s) campeonato(s):', 
                             as.character(unique(data_vitorias$campeonato)),
                             multiple=TRUE,
                             selected = c("Amistoso", "Brasileiro", "Citadino", "Gaúcho", "Outros"))),
               
               mainPanel(
                tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
                ),
                plotlyOutput("grafico_dif_vitorias")
      )))),
    
#GOLS

    tabPanel("Gols",
      titlePanel("Distribuição dos gols em grenais"),
        sidebarLayout(
          sidebarPanel(
            
            radioButtons('variavel_g', 'Variável:', choices = unique(data_gols$variavel)),
            
            selectInput('time_g', 'Time:', choices = unique(data_gols$time)),
            
            dateRangeInput('data_escolhida_g', strong("Selecione o intervalo de tempo:"), 
                          start = '1909-07-18', end = Sys.Date(),
                          min = '1909-07-18', max = Sys.Date(),
                          startview='year', language='pt', separator='até', format = "dd/mm/yyyy"),
          
            pickerInput('estadio_g', 'Selecione o(s) estádio(s):', 
                       as.character(unique(data_gols$estadio)),
                       multiple=TRUE,
                       selected = c("Arena", "Baixada", "Beira-Rio", "Chácara dos Eucaliptos", "Eucaliptos", "Olímpico", "Outros")),
          
            pickerInput('campeonato_g', 'Selecione o(s) campeonato(s):', 
                       as.character(unique(data_gols$campeonato)),
                       multiple=TRUE,
                       selected = c("Amistoso", "Brasileiro", "Citadino", "Gaúcho", "Outros"))),
                    
          mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            plotlyOutput("grafico_gols")
    ))),

#HOMEM-GRENAL

    tabPanel("Homens Grenal",
      titlePanel("Jogadores com gols em grenais"),
        sidebarLayout(
          sidebarPanel(
            pickerInput('time_hg', 'Selecione o(s) time(s):', 
                        as.character(unique(data_hg$time)),
                        multiple=TRUE,
                        selected = c("Grêmio", "Internacional")),
            
            dateRangeInput('data_escolhida_hg', strong("Selecione o intervalo de tempo:"), 
                           start = '1909-07-18', end = Sys.Date(),
                           min = '1909-07-18', max = Sys.Date(),
                           startview='year', language='pt', separator='até', format = "dd/mm/yyyy"),
          
            pickerInput('estadio_hg', 'Selecione o(s) estádio(s):', 
                        as.character(unique(data_hg$estadio)),
                        multiple=TRUE,
                        selected = c("Arena", "Baixada", "Beira-Rio", "Chácara dos Eucaliptos", "Eucaliptos", "Olímpico", "Outros")),
            
            pickerInput('campeonato_hg', 'Selecione o(s) campeonato(s):', 
                        as.character(unique(data_hg$campeonato)),
                        multiple=TRUE,
                        selected = c("Amistoso", "Brasileiro", "Citadino", "Gaúcho", "Outros"))),
            
          mainPanel(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            wordcloud2Output("word_cloud_h_grenal")))),

#PLACARES

tabPanel("Placares",
         titlePanel("Placares mais comuns nos grenais"),
         sidebarLayout(
           sidebarPanel(

             dateRangeInput('data_escolhida_p', strong("Selecione o intervalo de tempo:"), 
                            start = '1909-07-18', end = Sys.Date(),
                            min = '1909-07-18', max = Sys.Date(),
                            startview='year', language='pt', separator='até', format = "dd/mm/yyyy"),
   
             pickerInput('estadio_p', 'Selecione o(s) estádio(s):', 
                         as.character(unique(data_placar$estadio)),
                         multiple=TRUE,
                         selected = c("Arena", "Baixada", "Beira-Rio", "Chácara dos Eucaliptos", "Eucaliptos", "Olímpico", "Outros")),
             
             pickerInput('campeonato_p', 'Selecione o(s) campeonato(s):', 
                         as.character(unique(data_placar$campeonato)),
                         multiple=TRUE,
                         selected = c("Amistoso", "Brasileiro", "Citadino", "Gaúcho", "Outros"))),
           
           mainPanel(
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             highchartOutput("tree_map_placar"))))   
)))
                             