library(data.table) 
library(dplyr)
library(tidyr)
library(shiny)
library(tibble) 
library(shinythemes)
library(plotly) 
library(wordcloud2) 
library(highcharter)

#VITÓRIAS ACUMULADAS

shinyServer(function(input, output) {
  
    output$grafico_vitorias <- renderPlotly({
  
    if(length(input$estadio_v)==0) return(NULL)
    if(length(input$campeonato_v)==0) return(NULL)
    if(is.na(input$data_escolhida_v[2])) return(NULL)
    if(is.na(input$data_escolhida_v[1])) return(NULL)
#   if((input$data_escolhida_v)<'1909-07-18') return(NULL)
#   if((input$data_escolhida_v)>Sys.Date()) return(NULL)
    if((input$data_escolhida_v[1])>input$data_escolhida_v[2]) return(NULL)

    estadio_escolhido_v <- input$estadio_v
    campeonato_escolhido_v <- input$campeonato_v
    
    data_vitorias <- as_tibble(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
      arrange(N)
    
    data_vitorias$data2 <-as.Date(data_vitorias$data, format="%d/%m/%Y")
    
    base_aux <- data_vitorias %>%
      filter(data2>=input$data_escolhida_v[1] & data2<=input$data_escolhida_v[2] & estadio %in% estadio_escolhido_v & campeonato %in% campeonato_escolhido_v)

    base_aux$vitoria_gremio[base_aux$saldo_gremio<=0] <- 0
    base_aux$vitoria_gremio[base_aux$saldo_gremio>0] <- 1
    base_aux$vitoria_acumulada_gremio <- cumsum(base_aux$vitoria_gremio)
    
    base_aux$vitoria_inter[base_aux$saldo_inter<=0] <- 0
    base_aux$vitoria_inter[base_aux$saldo_inter>0] <- 1
    base_aux$vitoria_acumulada_inter <- cumsum(base_aux$vitoria_inter)
    
    base_aux$empate[base_aux$saldo_gremio==0] <- 1
    base_aux$empate[base_aux$saldo_gremio!=0] <- 0
    base_aux$empate_acumulado <- cumsum(base_aux$empate)
    
    base_aux <- base_aux %>%
      select(vitoria_acumulada_gremio, vitoria_acumulada_inter, empate_acumulado, estadio, N, data, data2)%>%
      gather(serie, acumulado, vitoria_acumulada_gremio, vitoria_acumulada_inter, empate_acumulado)
    
    base_aux$serie[base_aux$serie=="vitoria_acumulada_gremio"] <- "Vitórias do Grêmio"
    base_aux$serie[base_aux$serie=="vitoria_acumulada_inter"] <- "Vitórias do Internacional"
    base_aux$serie[base_aux$serie=="empate_acumulado"] <- "Empates"
    
    plot_ly(base_aux, x = ~as.Date(data2, format="%d/%m/%Y"), y = ~acumulado, 
            type = 'scatter', mode = 'lines', color = ~serie, colors=c('gray', 'deepskyblue', 'red'), hoverinfo="text",
            text = ~paste0(serie, "<br>", "Grenal nº ", N, "<br>", "Data:", data, "<br>", "Acumulado:", acumulado))%>%
    layout(title = NULL, xaxis = list(title = "Data"), yaxis = list(title = "Acumulado"))
    
    })  

#DIFERENÇA DE VITÓRIAS
    
    output$grafico_dif_vitorias <- renderPlotly({
        
        if(length(input$estadio_v2)==0) return(NULL)
        if(length(input$campeonato_v2)==0) return(NULL)
        if(is.na(input$data_escolhida_v2[2])) return(NULL)
        if(is.na(input$data_escolhida_v2[1])) return(NULL)
#       if((input$data_escolhida_v2)<'1909-07-18') return(NULL)
#       if((input$data_escolhida_v2)>Sys.Date()) return(NULL)
        if((input$data_escolhida_v2[1])>input$data_escolhida_v2[2]) return(NULL)
        
        estadio_escolhido_v2 <- input$estadio_v2
        campeonato_escolhido_v2 <- input$campeonato_v2
        
        data_vitorias <- as_tibble(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
          arrange(N)
        
        data_vitorias$data2 <-as.Date(data_vitorias$data, format="%d/%m/%Y")
        
        base_aux2 <- data_vitorias %>%
          filter(data2>=input$data_escolhida_v2[1] & data2<=input$data_escolhida_v2[2] & estadio %in% estadio_escolhido_v2 & campeonato %in% campeonato_escolhido_v2)
        
        base_aux2$vitoria_gremio[base_aux2$saldo_gremio<=0] <- 0
        base_aux2$vitoria_gremio[base_aux2$saldo_gremio>0] <- 1
        base_aux2$vitoria_acumulada_gremio <- cumsum(base_aux2$vitoria_gremio)
        
        base_aux2$vitoria_inter[base_aux2$saldo_inter<=0] <- 0
        base_aux2$vitoria_inter[base_aux2$saldo_inter>0] <- 1
        base_aux2$vitoria_acumulada_inter <- cumsum(base_aux2$vitoria_inter)
        
        base_aux2 <- base_aux2 %>%
          mutate(dif_vitorias=vitoria_acumulada_gremio-vitoria_acumulada_inter)
        
        base_aux2$pos <- ifelse(base_aux2$dif_vitorias >= 0 , base_aux2$dif_vitorias, NA)
        base_aux2$neg <- ifelse(base_aux2$dif_vitorias <= 0 , base_aux2$dif_vitorias, NA)

        plot_ly(base_aux2, x = ~as.Date(data2, format="%d/%m/%Y"), y = ~pos, 
                type = 'scatter', mode = 'lines', hoverinfo="text", line = list(color = 'deepskyblue'), showlegend = FALSE,
                text = ~paste0("Vitórias do Grêmio - Vitórias do Internacional = ", dif_vitorias, "<br>", "Grenal nº ", N, "<br>", "Data:", data, "<br>"))%>%
        
        add_trace(base_aux2, x = ~as.Date(data2, format="%d/%m/%Y"), y = ~neg, 
                  type = 'scatter', mode = 'lines', hoverinfo="text", line = list(color = 'red'), showlegend = FALSE,
                  text = ~paste0("Vitórias do Grêmio - Vitórias do Internacional = ", dif_vitorias, "<br>", "Grenal nº ", N, "<br>", "Data:", data, "<br>"))%>%
        
        layout(title = NULL, xaxis = list(title = "Data"), yaxis = list(title = "Diferença"))
        
      })  
      
#GOLS

  output$grafico_gols <- renderPlotly({
    
    variavel_escolhida_g <- input$variavel_g
    time_escolhido_g <- input$time_g
    estadio_escolhido_g <- input$estadio_g
    campeonato_escolhido_g <- input$campeonato_g
    
    if(length(input$estadio_g)==0) return(NULL)
    if(length(input$campeonato_g)==0) return(NULL)
    if(is.na(input$data_escolhida_g[2])) return(NULL)
    if(is.na(input$data_escolhida_g[1])) return(NULL)
#   if((input$data_escolhida_g)<'1909-07-18') return(NULL)
#   if((input$data_escolhida_g)>Sys.Date()) return(NULL)
    if((input$data_escolhida_g[1])>input$data_escolhida_g[2]) return(NULL)
    
    if (time_escolhido_g=="Grêmio") color <- "deepskyblue"
    if (time_escolhido_g=="Internacional") color <- "red"
    
    data_gols <- as_tibble(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
      select(gremio, inter, saldo_gremio, saldo_inter, estadio, campeonato, N, data) %>%
      gather(time, valor, gremio, inter, saldo_gremio, saldo_inter)
    
    data_gols$data2 <-as.Date(data_gols$data, format="%d/%m/%Y")
    
    data_gols$variavel <- "Total de gols"
    data_gols$variavel[data_gols$time=="saldo_gremio"|data_gols$time=="saldo_inter"] <- "Saldo de gols"
    
    data_gols$time[data_gols$time=="gremio"] <- "Grêmio"
    data_gols$time[data_gols$time=="saldo_gremio"] <- "Grêmio"
    data_gols$time[data_gols$time=="inter"] <- "Internacional"
    data_gols$time[data_gols$time=="saldo_inter"] <- "Internacional"
    
    data_gols <- data_gols %>%
      filter(data2>=input$data_escolhida_g[1] & data2<=input$data_escolhida_g[2] & time %in% time_escolhido_g, variavel %in% variavel_escolhida_g & estadio %in% estadio_escolhido_g & campeonato %in% campeonato_escolhido_g) 
    
    plot_ly(data_gols, x = ~valor, type="histogram", histnorm = "sum", marker = list(color = color), hoverinfo="y", xbins=list(size=1)) %>%
    layout(title = NULL, bargap=0.1, xaxis = list(title="Total de gols | Saldo de gols", tickmode='linear'), yaxis=list(title = "Número de jogos", type='linear'))
    
    })
    
#HOMEM-GRENAL

  output$word_cloud_h_grenal <- renderWordcloud2({
    
    if(length(input$time_hg)==0) return(NULL)
    if(length(input$estadio_hg)==0) return(NULL)
    if(length(input$campeonato_hg)==0) return(NULL)
    if(is.na(input$data_escolhida_hg[2])) return(NULL)
    if(is.na(input$data_escolhida_hg[1])) return(NULL)
 #  if((input$data_escolhida_hg)<'1909-07-18') return(NULL)
 #  if((input$data_escolhida_hg)>Sys.Date()) return(NULL)
    if((input$data_escolhida_hg[1])>input$data_escolhida_hg[2]) return(NULL)

    estadio_escolhido_hg <- input$estadio_hg
    campeonato_escolhido_hg <- input$campeonato_hg
    time_escolhido_hg <- input$time_hg

    data_hg <- as_tibble(fread(input="HomemGrenal.csv", sep=";", encoding = 'UTF-8')) 
    
    data_hg$data2 <-as.Date(data_hg$data, format="%d/%m/%Y")
    
    data_hg <- data_hg %>%
      arrange(N) %>%
      filter(time %in% time_escolhido_hg, estadio %in% estadio_escolhido_hg, campeonato %in% campeonato_escolhido_hg) %>%
      filter(data2>=input$data_escolhida_hg[1] & data2<=input$data_escolhida_hg[2])
    
    data_hg$aux <- 1
    
    gols <- data_hg %>%
      filter(jogador!="?")%>%
      group_by(jogador)%>%
      summarise(gols=sum(aux))%>%
      arrange(desc(gols))
    
    wordcloud2(gols, size=0.4, minSize = 1)
    
    })
 
#PLACARES
  
  output$tree_map_placar <- renderHighchart({
    
    if(length(input$estadio_p)==0) return(NULL)
    if(length(input$campeonato_p)==0) return(NULL)
    if(is.na(input$data_escolhida_p[2])) return(NULL)
    if(is.na(input$data_escolhida_p[1])) return(NULL)
#   if((input$data_escolhida_p)<'1909-07-18') return(NULL)
#   if((input$data_escolhida_p)>Sys.Date()) return(NULL)
    if((input$data_escolhida_p[1])>input$data_escolhida_p[2]) return(NULL)
    
    estadio_escolhido_p <- input$estadio_p
    campeonato_escolhido_p <- input$campeonato_p
    
    data_placar <- as_tibble(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
      arrange(N)
    
    data_placar$data2 <-as.Date(data_placar$data, format="%d/%m/%Y")
    
    data_placar$placar1 <- ifelse(data_placar$gremio >= data_placar$inter, data_placar$gremio, data_placar$inter)
    data_placar$placar2 <- ifelse(data_placar$gremio >= data_placar$inter, data_placar$inter, data_placar$gremio)
    
    data_placar$placar <- paste(data_placar$placar1, " x ", data_placar$placar2)
    
    data_placar <- data_placar %>%
      filter(estadio %in% estadio_escolhido_p, campeonato %in% campeonato_escolhido_p) %>%
      filter(data2>=input$data_escolhida_p[1] & data2<=input$data_escolhida_p[2])
    
    data_placar <- data_placar %>%
      group_by(placar)%>%
      summarise(n=n())%>%
      arrange(desc(n))
    
    hchart(data_placar, 'treemap', hcaes(x = 'placar', value = 'n', color = 'n')) %>% 
      hc_colorAxis(stops = color_stops(colors = viridis::plasma(10)))
    
    })
  
  
})