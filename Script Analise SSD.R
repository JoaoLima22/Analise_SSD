####                                  ####
###  Stript desenvolvido por:          ###
###  João Vitor Lima Castro Teixeira   ###
####                                  ####

############## Instalo e carrego as bibliotecas nescessarias ############# 

install.packages("ggplot2")
install.packages("boot")
install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")

library(ggplot2)
library(boot)
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)



############# Importar dados do arquivo CSV ############# 

caminho_do_arquivo_csv <- "Dados-Analise.csv"
dados <- read.table(caminho_do_arquivo_csv, header = TRUE, sep = ",")
dados <- na.omit(dados)

# Funcao para calcular o intervalo de confianca
ic <- function(vetor, confianca = 0.95) {
  resultado_teste <- t.test(vetor, conf.level = confianca)
  intervalo_confianca <- resultado_teste$conf.int
  return(intervalo_confianca)
}



############# Filtragem de dados #############

#Filtra por SSD C e Escrita
dados <- select(filter(dados, SSD == "SSD C"), escrita, tentativas_app, insercao) %>% rename(Valor = escrita, Query = tentativas_app, Insercao = insercao)

#Filtra por SSD C e Leitura
#dados <- select(filter(dados, SSD == "SSD C"), leitura, tentativas_app, insercao) %>% rename(Valor = leitura, Query = tentativas_app, Insercao = insercao)

#Filtra por SSD D e Escrita
#dados <- select(filter(dados, SSD == "SSD D"), escrita, tentativas_app, insercao) %>% rename(Valor = escrita, Query = tentativas_app, Insercao = insercao)

#Filtra por SSD D e Leitura
#dados <- select(filter(dados, SSD == "SSD D"), leitura, tentativas_app, insercao) %>% rename(Valor = leitura, Query = tentativas_app, Insercao = insercao)

# Converto a seguinte coluna como numeric
dados$Valor  <- as.numeric(trimws(dados$Valor))



############# Converto as colunas Query e Insercao para numericas ############# 

## Separo por Query
query1 <- select(filter(dados, Query == 1), Valor, Insercao)
query5 <- select(filter(dados, Query == 5), Valor, Insercao)
query9 <- select(filter(dados, Query == 9), Valor, Insercao)

## Separo por Query  e insercao
query1inter1  <- select(filter(query1, Insercao == 1), Valor)
query1inter16 <- select(filter(query1, Insercao == 16), Valor)
query1inter32 <- select(filter(query1, Insercao == 32), Valor)

query5inter1  <- select(filter(query5, Insercao == 1), Valor)
query5inter16 <- select(filter(query5, Insercao == 16), Valor)
query5inter32 <- select(filter(query5, Insercao == 32), Valor)

query9inter1  <- select(filter(query9, Insercao == 1), Valor)
query9inter16 <- select(filter(query9, Insercao == 16), Valor)
query9inter32 <- select(filter(query9, Insercao == 32), Valor)



############# Defino os dados que irao para o grafico ############# 

dados_grafico <- bind_rows(
  data.frame(
    Query = "1 Tentativa",
    Insercao = c("1 Giga", "16 Gigas", "32 Gigas"),
    Media = c(mean(query1inter1$Valor), mean(query1inter16$Valor), mean(query1inter32$Valor)),
    Infe = c(ic(query1inter1$Valor)[1], ic(query1inter16$Valor)[1], ic(query1inter32$Valor)[1]),
    Supe = c(ic(query1inter1$Valor)[2], ic(query1inter16$Valor)[2], ic(query1inter32$Valor)[2])
  ),
  data.frame(
    Query = "5 Tentativas",
    Insercao = c("1 Giga", "16 Gigas", "32 Gigas"),
    Media = c(mean(query5inter1$Valor), mean(query5inter16$Valor), mean(query5inter32$Valor)),
    Infe = c(ic(query5inter1$Valor)[1], ic(query5inter16$Valor)[1], ic(query5inter32$Valor)[1]),
    Supe = c(ic(query5inter1$Valor)[2], ic(query5inter16$Valor)[2], ic(query5inter32$Valor)[2])
  ),
  data.frame(
    Query = "9 Tentativas",
    Insercao = c("1 Giga", "16 Gigas", "32 Gigas"),
    Media = c(mean(query9inter1$Valor), mean(query9inter16$Valor), mean(query9inter32$Valor)),
    Infe = c(ic(query9inter1$Valor)[1], ic(query9inter16$Valor)[1], ic(query9inter32$Valor)[1]),
    Supe = c(ic(query9inter1$Valor)[2], ic(query9inter16$Valor)[2], ic(query9inter32$Valor)[2])
  )
)



############# Grafico ############# 

# Criar o grafico de barras agrupadas com erro
grafico <- ggplot(dados_grafico, aes(x = Insercao, y = Media, fill = Query, group = Query)) +
  # Insercao de Dados
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  # Insersao dos IC's
  geom_errorbar(aes(ymin = Infe, ymax = Supe), position = position_dodge(0.9), width = 0.25, color = "Grey45") +
  
  # Titulo e laterais
  labs(title = "",
       x = "Gigas Transferidos", y = "Media de MB/s") +
  theme_minimal() +

  # Valores no topo de cada coluna (Media no topo)
  geom_text(aes(label = sprintf("%.2f", Media), y = Media, group = Query), position = position_dodge(0.9), vjust = -0.5, color = "black") +
  

  ############# Regular a escala ############# 

  # Escala para Escrita
  scale_y_continuous(breaks = seq(0, 2300, by = 200), limits = c(0, 2300), expand = c(0, 0)) +

  # Escala para leitura
  #scale_y_continuous(breaks = seq(0, 1300, by = 200), limits = c(0, 1300), expand = c(0, 0)) +

  # Cor para a legenda
  scale_fill_manual(values = c("1 Tentativa" = "lightgreen", "5 Tentativas" = "lightblue", "9 Tentativas" = "lightcoral")) +
  # Adicionar contorno na legenda
  theme(
    legend.background = element_rect(color = "black")  
  ) +
  # Alterar a descricao da legenda
  labs(fill = "Numero de Tentativas")

# Exibo o grafico
print(grafico)


