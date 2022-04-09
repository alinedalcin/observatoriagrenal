library(data.table) 
library(tidyverse)
library(shiny) 
library(tibble) 
library(shinythemes) 
library(plotly) 
library(wordcloud2) 
library(highcharter)

data_vitorias <- tbl_df(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
  arrange(N)

data_gols <- tbl_df(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
  select(gremio, inter, saldo_gremio, saldo_inter, estadio, campeonato, N, data) %>%
  gather(time, valor, gremio, inter, saldo_gremio, saldo_inter)

data_gols$variavel <- "Total de gols"
data_gols$variavel[data_gols$time=="saldo_gremio"|data_gols$time=="saldo_inter"] <- "Saldo de gols"

data_gols$time[data_gols$time=="gremio"] <- "Grêmio"
data_gols$time[data_gols$time=="saldo_gremio"] <- "Grêmio"
data_gols$time[data_gols$time=="inter"] <- "Internacional"
data_gols$time[data_gols$time=="saldo_inter"] <- "Internacional"

data_hg <- tbl_df(fread(input="HomemGrenal.csv", sep=";", encoding = 'UTF-8')) %>%
  arrange(N)

data_placar <- tbl_df(fread(input="GrenalVis.csv", sep=";", encoding = 'UTF-8')) %>%
  arrange(N)
