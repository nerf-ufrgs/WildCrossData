#PACOTES NECESSARIOS
library(magrittr)
library(janitor)
library(readxl)
library(hms)
library(lubridate)
library(tidyverse)

#LENDO A PASTA ONDE ESTAO AS PLANILHAS (APENAS AS PLANILHAS)
excel <- list.files(path = "Excel", #fuça a pasta "Dados"
                    pattern = "^\\w.+xlsx$", #busca um padrão de arquivo que termine com "xlsx"
                    full.names = TRUE, #retorna o caminho completo do arquivo
                    recursive = TRUE) #fuça nas subpastas do caminho escolhido

#SEPARANDO APENAS O NOME DE CADA PLANILHA E RETIRANDO A EXTENSÃO DA LOCALIAÇÃO DA PLANILHA, APENAS PARA FACILITAR
nomes <- excel %>% 
  str_split(., "/|\\.", #fatia o endereço completo dos arquivos baseado em / e . Aqui pode ter diferença para o Windows em função do uso de \ nos caminhos.
            simplify = TRUE) %>% # transforma o resultado em data frame
  as_tibble() %>% #transforma em data frame tipo tibble
  pull(V3) #transforma a coluna V2 em vetor

load <- excel %>% 
  set_names(., nomes) #atribui à lista de arquivos (excel) os nomes do vetor extraído no passo anterior


#Lê a planilha de AF
af <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 5, #lê a segunda planilha do arquivo excel (dados câmeras)
             #range = cell_cols(1:3), #define as colunas que devem ser lidas
             #skip = 3, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na"),
             col_names = TRUE) %>% 
    select(1) %>% 
    remove_empty("rows") #%>% #remove as linhas vazias (em que os elementos de todas as colunas são NA)
})

fence <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 4, #lê a segunda planilha do arquivo excel (dados câmeras)
             #range = cell_cols(1:3), #define as colunas que devem ser lidas
             #skip = 3, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na"),
             col_names = TRUE) %>% 
    select(1) %>% 
    remove_empty("rows") #%>% #remove as linhas vazias (em que os elementos de todas as colunas são NA)
})

#Lê a planilha de under e overpasses
under <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 2, #lê a segunda planilha do arquivo excel (dados câmeras)
             #range = cell_cols(1:3), #define as colunas que devem ser lidas
             #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na")) %>% 
    #col_names = nome_colunas_reg)
    select(2) #%>%  #seleciona as colunas de 1 a 4
  #set_names(nome_colunas_reg) #renomeia as colunas conforme os nomes do vetor nome_colunas_af
})

over <- map(.x = load, function(arquivo){
  read_excel(arquivo, #lê o arquivo excel que está no endereço em load
             sheet = 3, #lê a segunda planilha do arquivo excel (dados câmeras)
             #range = cell_cols(1:3), #define as colunas que devem ser lidas
             #skip = 1, #pula a primeira linha do arquivo (por causa da mesclagem)
             na = c("NA", "na")) %>% 
    #col_names = nome_colunas_reg)
    select(2) #%>%  #seleciona as colunas de 1 a 4
  #set_names(nome_colunas_reg) #renomeia as colunas conforme os nomes do vetor nome_colunas_af
})


#VERIFICA SE OS IDS DAS ESTRUTURAS NA PLANILHA DAS CAMERAS ESTAO NA PLANILHA UNDER E OVER
checa_ID_under <- map2(under, af, function(x, y){
  setdiff(y$Structure_ID, x$Structure_ID) %>% 
    enframe(name = "Erro", value = "Structure_ID")
}) %>% #MOSTRA APENAS OS IDS ERRADOS QUE NAO ESTAO NA ABA DAS ESTRUTURAS
  discard(~ nrow(.x) == 0) %>% #descarta os arquivos sem erros
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble()

checa_ID_over <- map2(over, af, function(x, y){
  setdiff(y$Structure_ID, x$Structure_ID) %>% 
    enframe(name = "Erro", value = "Structure_ID")
}) %>% #MOSTRA APENAS OS IDS ERRADOS QUE NAO ESTAO NA ABA DAS ESTRUTURAS
  discard(~ nrow(.x) == 0) %>% #descarta os arquivos sem erros
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble()

#ACHAR SOLUCAO PARA JUNTAR RESPOSTAS DE UNDER E OVER DOS IDS DE ESTRUTURAS NAO LISTADAS NA CAMERA 
#PRECISAMOS APENAS DAS ESTRUTURAS NAO LISTADAS NEM EM UNDER E NEM EM OVER
#SEGUIR COM FENCING

checa_ID_fence <- map2(fence, under, function(x, y){
  setdiff(y$Structure_ID, x$Structure_ID) %>% 
    enframe(name = "Erro", value = "Structure_ID")
}) %>% #MOSTRA APENAS OS QUAIS ESTRUTURAS NAO TEM CERCA
  discard(~ nrow(.x) == 0) %>% #descarta os arquivos sem erros
  plyr::ldply(., .fun = data.frame, .id = "Arquivo") %>%  #junta todos os dados em um único data frame
  as_tibble()
